#define new(t,...)\
    ((t*) memcpy(malloc(sizeof(t)), &(t){__VA_ARGS__}, sizeof(t)))

#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct string { int len; char chars[]; } string;
typedef struct location { char *fn; int ln, col; } location;
typedef struct value value;
typedef struct list list;
typedef struct fn fn;
typedef struct data data;
typedef struct node node;
typedef struct rules rules;
typedef struct tuple tuple;
typedef struct senv senv;
typedef struct env env;
typedef struct infix { char *id; int lhs, rhs; struct infix *next; } infix;
typedef struct vars { char *id; struct vars *next; } vars;

typedef enum toktype {
    // Keep in sync with toks.
    // Single-character punctuation must be between LPAREN and TSEMI.
    // Reserved words must come after TID.
    TEND, TINT, TSTRING, TLPAREN, TRPAREN, TLBRACE, TRBRACE,
    TCOMMA, TBACK, TSEMI, TID, TEQUAL, TIF, TTHEN, TELSE, TLET, TREC,
    TAND, TIN, TTRUE, TFALSE, TCASE, TBAR, TFN, TARROW, TINFIXL,
    TINFIXR, TREF, TDEREF, TDATATYPE,
} toktype;

char *toks[] = {
    "end", "int", "string", "(", ")", "[", "]", ",", "`", ";", "id",
    "=", "if", "then", "else", "let", "rec", "and", "in",
    "true", "false", "case", "|", "fn", "->", "infixl", "infixr",
    "ref", "!", "datatype", 0
};

struct value {
    enum {
        NO_VALUE, NIL, UNIT, BOOLE, INT, STRING, LIST, REF, DATA, TUPLE, FN
    } form;
    union {
        int _int;
        string *str;
        list *list;
        fn *fn;
        value *ref;
        tuple *tuple;
        struct data *data;
    };
};
struct list { value hd, tl; };
struct fn { char *par; node *body; struct env *env; int remain, op; };
struct tuple { int len; value xs[]; };
struct data { char *constr; value arg; };

struct node {
    enum {
        ELIT, EVAR, ECONS, EREF, EDEREF, ESEQ, EAPP, ETUPLE, EIF,
        ELET, ECASE, EFN,
    } form;
    location loc;
    union {
        value val;
        struct { char *id; int index; };
        struct { node *lhs, *rhs; };
        struct { node *a, *b, *c; } _if;
        struct { bool rec; rules *rules; node *body; } let;
        struct { char *par; node *body; } fn;
        struct { node *val; rules *rules; } _case;
        struct { int len; node **xs; } tuple;
        node *ref;
    };
};

struct rules { node *lhs, *rhs; struct rules *next; };

struct env { value val; env *next; };

struct senv { char *id; value val; senv *next; };

enum {
    OP_CONS, OP_EQUAL, OP_NOT_EQUAL, OP_LESS, OP_LESS_EQUAL,
    OP_GREATER, OP_GREATER_EQUAL, OP_ADD, OP_SUB, OP_MUL, OP_DIV,
    OP_REM, OP_JOIN, OP_STRLEN, OP_CHAR_AT, OP_SUBSTR, OP_STARTS,
    OP_FIND_STR, OP_ORD, OP_CHR, OP_ASSIGN, OP_PR, OP_READ_FILE,
    OP_WRITE_FILE,
};

char    source[65536];
char    tokbuf[sizeof source];
char    *src, *sol;
location sloc;
bool    peeked;
toktype token;
int     tokint;
string  *tokstr;
string  *interns[65536];
int     ninterns;
infix   *infixes;
value   nil={NIL}, unit={UNIT}, _true={BOOLE,._int=1}, _false={BOOLE};
value   single_char[256], empty_string, noval={NO_VALUE};
char    *ignore;


void _Noreturn fatal(location loc, char *msg, ...) {
    va_list     ap;
    va_start(ap, msg);
    printf("boot: error %s:%d:%d: ", loc.fn, loc.ln, loc.col);
    vprintf(msg, ap);
    puts("");
    exit(1);
}

#define syntax(...) fatal(sloc, __VA_ARGS__)

#define raise(loc, msg, x)\
    (fputs("\noffender: ", stdout), printval(x), fatal(loc, msg))

string *newstr(char *chars, int len) {
    string *str = malloc(sizeof *str + len + 1);
    str->len = len;
    if (chars) memcpy(str->chars, chars, len);
    str->chars[len] = 0;
    return str;
}

string *intern(char *chars, int len) {
    if (len < 0) len = strlen(chars);

    for (string **i = interns + ninterns; i-- > interns; )
        if (len == (*i)->len && !memcmp(chars, (*i)->chars, len)) return *i;

    return interns[ninterns++] = newstr(chars, len);
}

#define cstr(CHARS) intern(CHARS, -1)->chars

// Type Checking.
#define ck(X,TYPE) if (!is##TYPE(X)) raise(loc, "not " #TYPE, X)
#define ckint(X) ck(X, int)
#define ckstr(X) ck(X, str)

// C struct constructors.
#define value(f,...) ((value) {f, __VA_ARGS__})
#define node(F, LOC, ...) new(node, F, .loc=LOC, __VA_ARGS__)
#define rules(lhs, rhs, next) new(struct rules, lhs, rhs, next)
#define senv(id, val, next) new(senv, id, val, next)
#define env(x, next) new(struct env, x, next)

// Expression constructors.
#define elit(loc, x) node(ELIT, loc, .val=x)
#define evar(loc, x) node(EVAR, loc, .id=x)
#define econs(loc, hd, tl) node(ECONS, loc, .lhs=hd, .rhs=tl)
#define eref(loc, e) node(EREF, loc, .ref=e)
#define ederef(loc, e) node(EDEREF, loc, .ref=e)
#define eseq(loc, a, b) node(ESEQ, loc, .lhs=a, .rhs=b)
#define eapp(loc, f, x) node(EAPP, loc, .lhs=f, .rhs=x)
#define etuple(loc, len, xs) node(ETUPLE, loc, .tuple={len, xs})
#define eif(loc, a, b, c) node(EIF, loc, ._if={a,b,c})
#define elet(loc, rec, rules, body) node(ELET, loc, .let={rec, rules, body})
#define ecase(loc, val, rules) node(ECASE, loc, ._case={val, rules})
#define efn(loc, par, body) node(EFN, loc, .fn={par, body})

// Value constructors.
#define theint(x) value(INT, ._int=x)
#define thestr(x) value(STRING, .str=x)
#define thelist(x) value(LIST, .list=x)
#define theref(x) value(REF, .ref=x)
#define thedata(x) value(DATA, .data=x)
#define thetup(x) value(TUPLE, .tuple=x)
#define thefn(x) value(FN, .fn=x)

// Value predicates.
#define type_of(X) ((X).form)
#define isnoval(x) (type_of(x) == NO_VALUE)
#define isnil(x) (type_of(x) == NIL)
#define isunit(x) (type_of(x) == UNIT)
#define isbool(x) (type_of(x) == BOOLE)
#define isint(x) (type_of(x) == INT)
#define isstr(x) (type_of(x) == STRING)
#define islist(x) (type_of(x) == LIST)
#define isref(x) (type_of(x) == REF)
#define isdata(x) (type_of(x) == DATA)
#define isdataconstr(x) (isdata(x) && !datahasarg(x))
#define istup(x) (type_of(x) == TUPLE)
#define isfn(x) (type_of(x) == FN)

// Value extractors.
#define boolval(x) (x._int)
#define intval(x) (x._int)
#define strval(x) (x.str)
#define listval(x) (x.list)
#define refval(x) (x.ref)
#define dataval(x) (x.data)
#define tupval(x) (x.tuple)
#define fnval(x) (x.fn)

// Convenience extractors.
#define slen(x) (strval(x)->len)
#define schars(x) (strval(x)->chars)
#define hd(x) (listval(x)->hd)
#define tl(x) (listval(x)->tl)
#define dataconstr(x) (dataval(x)->constr)
#define dataarg(x) (dataval(x)->arg)
#define datahasarg(x) (!isnoval(dataarg(x)))
#define tuplen(x) (tupval(x)->len)
#define tupn(x, n) (tupval(x)->xs[n])
#define fnpar(x) (fnval(x)->par)
#define fnbody(x) (fnval(x)->body)
#define fnenv(x) (fnval(x)->env)
#define fnremain(x) (fnval(x)->remain)
#define fnop(x) (fnval(x)->op)

// Value modifiers.
#define tupsetn(x, n, y) (tupval(x)->xs[n] = y)
#define fnsetenv(x, e) (fnval(x)->env = e)

// Structure constructors.
#define cons(x, y) thelist(new(struct list, x, y))
#define newdata(constr, arg) thedata(new(struct data, constr, arg))
#define newfn(par, body, env) thefn(new(struct fn, par, body, env, 0, 0))
#define newnative(par, env, remain, op)\
    thefn(new(struct fn, par, 0, env, remain, op))

value newref(value x) {
    value *ref = new(value);
    *ref = x;
    return theref(ref);
}

value newtup(int len) {
    tuple *tup = malloc(sizeof *tup + len * sizeof *tup->xs);
    tup->len = len;
    return thetup(tup);
}

bool equal(value x, value y) {
    if (type_of(x) != type_of(y)) return false;
    if (isnil(x)) return true;
    if (isunit(x)) return true;
    if (isbool(x)) return boolval(x) == boolval(y);
    if (isint(x)) return intval(x) == intval(y);
    if (isstr(x))
        return strval(x) == strval(y) ||
            (slen(x) == slen(y) && !memcmp(schars(x), schars(y), slen(x)));
    if (islist(x)) {
        if (listval(x) == listval(y)) return true;
        for ( ; islist(x) && islist(y); x = tl(x), y =tl(y))
            if (!equal(hd(x), hd(y))) return false;
        return equal(x, y);
    }
    if (isref(x)) return refval(x) == refval(y);
    if (isdata(x)) return dataval(x) == dataval(y) ||
        (dataconstr(x) == dataconstr(y) && equal(dataarg(x), dataarg(y)));
    if (istup(x)) {
        if (tupval(x) == tupval(y)) return true;
        if (tuplen(x) != tuplen(y)) return false;
        for (int i = 0; i < tuplen(x); i++)
            if (!equal(tupn(x, i), tupn(y, i))) return false;
        return true;
    }
    if (isfn(x)) return fnval(x) == fnval(y);
    if (isnoval(x)) return isnoval(y);
    return false;
}

void prval(value x) {
    if (isnil(x)) fputs("[]", stdout);
    else if (isunit(x)) fputs("()", stdout);
    else if (isbool(x)) fputs(boolval(x)? "true": "false", stdout);
    else if (isint(x)) printf("%d", intval(x));
    else if (isstr(x)) fwrite(schars(x), 1, slen(x), stdout);
    else if (islist(x)) {
        putchar('[');
        for (value i = x; islist(i); i = tl(i))
            prval(hd(i)),
            fputs(isnil(tl(i))? "]": ", ", stdout);
    }
    else if (isref(x)) fputs("ref ", stdout), prval(*refval(x));
    else if (isdata(x)) {
        fputs(dataconstr(x), stdout);
        if (datahasarg(x)) { putchar(' '); prval(dataarg(x)); }
    }
    else if (istup(x)) {
        for (int i = 0; i < tuplen(x); i++)
            fputs(i? ", ": "(", stdout),
            prval(tupn(x, i));
        putchar(')');
    }
    else if (isfn(x)) fputs("(fn)", stdout);
}

#define printval(x) (prval(x), puts(""))

value join(location loc, value list) {
    int     len = 0;
    for (value i = list; islist(i); i = tl(i)) {
        if (!isstr(hd(i))) raise(loc, "joining non-string", hd(i));
        len += slen(hd(i));
    }

    string *str = newstr(0, len);
    int     base = 0;
    for (value i = list; islist(i); i = tl(i)) {
        memcpy(str->chars + base, schars(hd(i)), slen(hd(i)));
        base += slen(hd(i));
    }
    return thestr(str);
}

int find_string(string *big, int after, string *small) {
    if (small->len == 0) return 0;
    if (big->len < small->len) return -1;
    int limit = big->len - small->len + 1;
    for (int i = after; i < limit; ) {
        if (!memcmp(big->chars + i, small->chars, small->len))
            return i;
        i++;
        while (i < limit && big->chars[i] != *small->chars) i++;
    }
    return -1;
}

void opensrc(char *path) {
    sloc = (location) {strdup(path), 1, 1};
    src = sol = source;
    peeked = false;
    FILE    *file = fopen(path, "rb");
    if (!file) syntax("cannot open file");
    source[fread(source, 1, sizeof source, file)] = 0;
    fclose(file);
}

#define get_loc() ((void) peek(0), sloc) /* next() has to set sloc */

toktype next(void) {
    char    *t = tokbuf;

    if (peeked) return peeked = false, token;

    for ( ; isspace(*src) || *src == '#'; src++)
        if (*src == '\n') sloc.ln++, sloc.col = 1, sol = src + 1;
        else if (*src == '#') while (src[1] && src[1] != '\n') src++;

    sloc.col = src - sol + 1;

    if (!*src) return token = TEND;

    if (isdigit(src[*src == '-']))
        return tokint = strtol(src, &src, 0), token = TINT;

    for (toktype i = TLPAREN; i <= TSEMI; i++)
        if (*src == toks[i][0]) return src++, token = i;

    if (*src == '"') {
        src++;
        while (true)
            if (!*src) syntax("unclosed string");
            else if (*src == '"') {
                tokstr = intern(tokbuf, t - tokbuf);
                return src++, token = TSTRING;
            }
            else if (*src != '\\') *t++ = *src++;
            else switch ((src += 2)[-1]) {
            case 'n': *t++ = '\n'; break;
            default: *t++ = src[-1]; break;
            }
    }

    char *base = src, *symbol = "!$%&*+-./:<=>?@^|~";
    while (isalnum(*src) || *src == '_') src++;
    if (src == base) while (*src && strchr(symbol, *src)) src++;
    if (src == base) syntax("illegal token");
    tokstr = intern(base, src - base);

    for (toktype i = TID + 1; toks[i]; i++) // Reserved words.
        if (toks[i] == tokstr->chars) return token = i;
    return token = TID;
}

#define want(t) (peeked = next() != t, !peeked)
#define peek(t) (peeked = next(), t == token)
#define need(t) ((void) (want(t) || (syntax("need %s", toks[t]), false)))

node *expr(void);

node *suffix(toktype t, node *get(void)) {
    node *x = get();
    return need(t), x;
}

rules *sequence(rules *get(bool first)) {
    rules   *x, *out = get(true);
    rules   **lastp = &out->next;
    while ((x = get(false)))
        *lastp = x,
        lastp = &x->next;
    return out;
}

infix *is_infix(void) {
    for (infix *i = infixes; i; i = i->next)
        if (i->id == tokstr->chars) return i;
    return 0;
}

node *listexpr(void) {
    if (want(TRBRACE)) return elit(sloc, nil);
    location loc = get_loc();
    node    *hd = expr();
    node    *tl = want(TCOMMA)? listexpr():
        (need(TRBRACE), elit(loc, nil));
    return econs(loc, hd, tl);
}

node *fnexpr(toktype delim) {
    if (want(delim)) return expr();
    location loc = get_loc();
    char    *par = (need(TID), tokstr->chars);
    node    *body = fnexpr(delim);
    return efn(loc, par, body);
}

node *aexpr(bool required) {
    location loc = get_loc();
    if (!required && peek(TID) && is_infix()) return 0;
    if (want(TINT)) return elit(loc, theint(tokint));
    if (want(TTRUE)) return elit(loc, _true);
    if (want(TFALSE)) return elit(loc, _false);
    if (want(TSTRING)) return elit(loc, thestr(tokstr));
    if (want(TID)) return evar(loc, tokstr->chars);
    if (want(TLPAREN)) {
        node    *tmp[64];
        int     n = 0;
        do {
            if (peek(TRPAREN)) break;
            if (n > sizeof tmp / sizeof *tmp) syntax("tuple too large");
            tmp[n++] = expr();
        } while (want(TCOMMA));
        need(TRPAREN);

        if (!n) return elit(loc, unit);
        if (n == 1) return tmp[0];
        node **xs = memcpy(malloc(n * sizeof *xs), tmp, n * sizeof *xs);
        return etuple(loc, n, xs);
    }
    if (want(TLBRACE)) return listexpr();
    if (want(TREF)) return eref(loc, aexpr(true));
    if (want(TDEREF)) return ederef(loc, aexpr(true));
    if (want(TFN)) return fnexpr(TARROW);
    if (required) syntax("need expression");
    return 0;
}

node *iexpr(int level) {
    if (level == 10) {
        node    *x, *f = aexpr(true);
        while ((x = aexpr(false))) f = eapp(f->loc, f, x);
        return f;
    }

    node    *lhs = iexpr(level + 1);

    for (infix *i = 0;
        (peek(TID) && (i = is_infix()) && i->lhs == level) ||
        (peek(TBACK) && level == 9); )
    {
        location loc = get_loc();
        next();
        if (token == TBACK) {
            char    *id = (need(TID), tokstr->chars);
            lhs = eapp(loc, eapp(loc, evar(loc, id), lhs), iexpr(level + 1));
        }
        else if (!strcmp(i->id, ":"))
            lhs = econs(loc, lhs, iexpr(i->rhs));
        else if (!strcmp(i->id, "&&"))
            lhs = eif(loc, lhs, iexpr(i->rhs), elit(loc, _false));
        else if (!strcmp(i->id, "||"))
            lhs = eif(loc, lhs, elit(loc, _true), iexpr(i->rhs));
        else
            lhs = eapp(loc, eapp(loc, evar(loc, i->id), lhs), iexpr(i->rhs));
    }

    return lhs;
}

rules *dec(bool first) {
    if (!want(TAND) && !first) return 0;
    node    *lhs = aexpr(true);
    return rules(lhs, fnexpr(TEQUAL), 0);
}

rules *rule(bool first) {
    if (!want(TBAR)) return 0;
    node    *lhs = suffix(TARROW, expr);
    return rules(lhs, expr(), 0);
}

node *expr(void) {
    node    *e;
    location loc = get_loc();

    if (want(TIF)) {
        node    *a = suffix(TTHEN, expr);
        node    *b = suffix(TELSE, expr);
        node    *c = expr();
        e = eif(loc, a, b, c);
    } else if (want(TLET)) {
        bool    rec = want(TREC);
        rules   *rules = ((void) want(TAND), sequence(dec));
        node    *body = (need(TIN), expr());
        e = elet(loc, rec, rules, body);
    } else if (want(TCASE)) {
        node    *val = expr();
        rules   *rules = sequence(rule);
        e = ecase(loc, val, rules);
    } else
        e = iexpr(0);

    if (want(TSEMI)) {
        loc = sloc; // Location of semicolon not of the next token.
        return eseq(loc, e, expr());
    }

    return e;
}

node **program(node **lastp, senv **env) {
    while (!want(TEND))
        if (want(TINFIXL) || want(TINFIXR)) {
            int     rhs = token == TINFIXL? 1: 0;
            int     lhs = (need(TINT), tokint);
            while (want(TID))
                infixes = new(infix, tokstr->chars, lhs, lhs + rhs, infixes);
        }
        else if (want(TLET)) {
            location loc = sloc; // The loc of `let` not the next token.
            bool    rec = want(TREC);
            rules   *rules = ((void) want(TAND), sequence(dec));
            *lastp = elet(loc, rec, rules, 0);
            lastp = &(*lastp)->let.body;
        }
        else if (want(TDATATYPE)) {
            want(TID); need(TID); need(TEQUAL); want(TBAR);
            do {
                char *constr = (need(TID), tokstr->chars);
                *env = senv(constr, newdata(constr, noval), *env);
                if (want(TLPAREN)) {
                    int level = 1;
                    while (level)
                        if (want(TLPAREN)) level++;
                        else if (want(TRPAREN)) level--;
                        else if (want(TEND)) syntax("type arg not closed");
                        else next();
                }
            } while (want(TBAR));
        }
        else
            syntax("need top-level statement");
    return lastp;
}

senv *lookup(char *id, senv *env, int *index) {
    int scratch;
    if (!index) index = &scratch;
    *index = 0;
    for (senv *i = env; i; i = i->next, (*index)++)
        if (i->id == id) return i;
    return 0;
}

senv *link_pattern(node *e, senv *env) {
    senv    *var;

    switch (e->form) {
    case ELIT:
        return env;

    case EVAR:
        if (e->id == ignore) return env;
        var = lookup(e->id, env, 0);
        if (var && isdataconstr(var->val)) { // Transform into constr.
            *e = *elit(e->loc, var->val);
            return env;
        }
        return senv(e->id, noval, env);

    case ECONS:
        env = link_pattern(e->lhs, env);
        return link_pattern(e->rhs, env);

    case ETUPLE:
        for (int i = 0; i < e->tuple.len; i++)
            env = link_pattern(e->tuple.xs[i], env);
        return env;

    case EAPP:
        link_pattern(e->lhs, env); // Will transform if constr.
        if (e->lhs->form == EVAR)
            fatal(e->lhs->loc, "not a consructor: %s", e->lhs->id);
        if (e->lhs->form != ELIT || !isdataconstr(e->lhs->val))
            goto invalid;
        return link_pattern(e->rhs, env);

    case ESEQ:
    case EREF:
    case EDEREF:
    case EIF:
    case ELET:
    case ECASE:
    case EFN:
        ;
    }

invalid:
    fatal(e->loc, "invalid pattern");
    return env;
}

// Calculate the index for variables.
// Replace variables if their value is known.
void link_vars(node *e, senv *env) {
    senv    *var, *local = env;

    switch (e->form) {
    case ELIT:
        break;

    case EVAR:
        var = lookup(e->id, env, &e->index);
        if (!var) fatal(e->loc, "undefined: %s", e->id);
        if (!isnoval(var->val)) // If value constant, replace.
            *e = *elit(e->loc, var->val);
        break;

    case ECONS:
    case EAPP:
    case ESEQ:
        link_vars(e->lhs, env);
        link_vars(e->rhs, env);
        break;

    case EREF:
    case EDEREF:
        link_vars(e->ref, env);
        break;

    case ETUPLE:
        for (int i = 0; i < e->tuple.len; i++)
            link_vars(e->tuple.xs[i], env);
        break;

    case EIF:
        link_vars(e->_if.a, env);
        link_vars(e->_if.b, env);
        link_vars(e->_if.c, env);
        break;

    case ELET:
        if (e->let.rec) {
            for (rules *i = e->let.rules; i; i = i->next) {
                if (i->lhs->form != EVAR || i->rhs->form != EFN)
                    fatal(i->lhs->loc, "let rec only defines functions");
                local = link_pattern(i->lhs, local);
            }
            for (rules *i = e->let.rules; i; i = i->next)
                link_vars(i->rhs, local);
        } else
            for (rules *i = e->let.rules; i; i = i->next) {
                link_vars(i->rhs, local);
                local = link_pattern(i->lhs, local);
            }
        link_vars(e->let.body, local);
        break;

    case ECASE:
        link_vars(e->_case.val, env);
        for (rules *i = e->_case.rules; i; i = i->next)
            link_vars(i->rhs, link_pattern(i->lhs, env));
        break;

    case EFN:
        link_vars(e->fn.body, senv(e->fn.par, noval, env));
        break;
    }
}

value operate(location loc, value x, env *env, int op) {
    static int type_checks[128] = {
        [OP_LESS]=1, [OP_LESS_EQUAL]=1, [OP_GREATER]=1,
        [OP_GREATER_EQUAL]=1, [OP_ADD]=1, [OP_SUB]=1,
        [OP_MUL]=1, [OP_DIV]=1, [OP_REM]=1,
    };
    if (type_checks[op] == 1) { ckint(env->val); ckint(x); }
    else if (type_checks[op] == 2) { ckstr(env->val); ckstr(x); }

    FILE    *file;
    int     i, j, len;
    value   str;

    switch (op) {

    case OP_CONS:
        if (!isnil(x)) ck(x, list);
        return cons(env->val, x);

    case OP_EQUAL:
        return equal(env->val, x)? _true: _false;
    case OP_NOT_EQUAL:
        return equal(env->val, x)? _false: _true;

    case OP_LESS:
        return intval(env->val) < intval(x)? _true: _false;
    case OP_LESS_EQUAL:
        return intval(env->val) <= intval(x)? _true: _false;
    case OP_GREATER:
        return intval(env->val) > intval(x)? _true: _false;
    case OP_GREATER_EQUAL:
        return intval(env->val) >= intval(x)? _true: _false;

    case OP_ADD:
        return theint(intval(env->val) + intval(x));
    case OP_SUB:
        return theint(intval(env->val) - intval(x));
    case OP_MUL:
        return theint(intval(env->val) * intval(x));
    case OP_DIV:
        return theint(intval(env->val) / intval(x));
    case OP_REM:
        return theint(intval(env->val) % intval(x));

    case OP_JOIN:
        return join(loc, x);
    case OP_STRLEN:
        ckstr(x);
        return theint(slen(x));
    case OP_CHAR_AT:
        ckstr(env->val); ckint(x);
        str = env->val;
        i = intval(x);
        if (i < 0) i += slen(str);
        if (i < 0 || i >= slen(str)) raise(loc, "out of bounds", x);
        return single_char[(unsigned char) schars(str)[i]];
    case OP_SUBSTR:
        ckstr(env->next->val); ckint(env->val); ckint(x);
        str = env->next->val;
        i = intval(env->val);
        j = intval(x);
        if (i < 0) i += slen(str);
        if (j < 0) j += slen(str) + 1;
        if (i < 0 || i >= slen(str)) raise(loc, "out of bounds", env->val);
        if (j < 0 || j > slen(str)) raise(loc, "out of bounds", x);
        if (j < i) raise(loc, "indexes cross", x);
        if (i == j) return empty_string;
        if (i == j - 1) return single_char[(unsigned char) schars(str)[i]];
        return thestr(newstr(schars(str) + i, j - i));
    case OP_STARTS:
        ckstr(env->val); ckstr(x);
        if (slen(x) == 0) return _true;
        if (slen(env->val) < slen(x)) return _false;
        return memcmp(schars(env->val), schars(x), slen(x))? _false: _true;
    case OP_FIND_STR:
        ckstr(env->next->val); ckint(env->val); ckstr(x);
        i = intval(env->val);
        if (i < 0) i += slen(env->next->val);
        if (i < 0 || i >= slen(env->next->val))
            raise(loc, "out of bounds", env->val);
        return theint(find_string(strval(env->next->val), i, strval(x)));
    case OP_ORD:
        ckstr(x);
        return theint((unsigned char) schars(x)[0]);
    case OP_CHR:
        ckint(x);
        return single_char[intval(x) % 256];

    case OP_ASSIGN:
        ck(env->val, ref);
        return *refval(env->val) = x;

    case OP_PR:
        prval(x);
        return x;

    case OP_READ_FILE:
        ckstr(x);
        file = fopen(schars(x), "rb");
        if (!file) return _false;
        fseek(file, 0, SEEK_END);
        len = ftell(file);
        rewind(file);
        str = thestr(newstr(0, len));
        fread(schars(str), 1, len, file);
        fclose(file);
        return str;

    case OP_WRITE_FILE:
        ckstr(env->val); ckstr(x);
        file = fopen(schars(env->val), "wb");
        if (!file) return _false;
        fwrite(schars(x), 1, slen(x), file);
        fclose(file);
        return _true;
    }

    fatal(loc, "UNHANDLED OPERATION: %d", op);
    return unit;
}

// Bind pattern to value. Return new env. NULL on falure.
env *bind_pattern(node *pat, value x, env *env) {

    if (!env) return 0;

    switch (pat->form) {
    case ELIT:
        return equal(pat->val, x)? env: 0;
    case EVAR:
        return pat->id == ignore? env: env(x, env);

    case ECONS:
        if (!islist(x)) return 0;

        env = bind_pattern(pat->lhs, hd(x), env);
        return bind_pattern(pat->rhs, tl(x), env);

    case ETUPLE:

        if (!istup(x) || pat->tuple.len != tuplen(x)) return 0;

        for (int i = 0; i < pat->tuple.len && env; i++)
            env = bind_pattern(pat->tuple.xs[i], tupn(x, i), env);
        return env;

    case EAPP:
        if (!isdata(x) || !datahasarg(x)) return 0;
        if (dataconstr(pat->lhs->val) != dataconstr(x)) return 0;
        return bind_pattern(pat->rhs, dataarg(x), env);

    case EREF:
    case EDEREF:
    case ESEQ:
    case EIF:
    case ELET:
    case ECASE:
    case EFN:
        break;
    }
    return 0;
}

value eval(node *e, env *env) {
    value       f, x, y;
    struct env  *local;

top:
    switch (e->form) {
    case ELIT:
        return e->val;

    case EVAR:
        local = env;
        for (int i = e->index; i--; )
            local = local->next;
        return local->val;

    case EREF:
        return newref(eval(e->ref, env));

    case EDEREF:
        x = eval(e->ref, env);
        if (!isref(x)) raise(e->loc, "not ref", x);
        return *refval(x);

    case ECONS:
        x = eval(e->lhs, env);
        y = eval(e->rhs, env);
        if (!isnil(y) && !islist(y)) raise(e->loc, "not list", y);
        return cons(x, y);

    case ETUPLE:
        x = newtup(e->tuple.len);
        for (int i = 0; i < e->tuple.len; i++)
            tupsetn(x, i, eval(e->tuple.xs[i], env));
        return x;

    case EAPP:
        f = eval(e->lhs, env);
        x = eval(e->rhs, env);

        if (isdataconstr(f))
            return newdata(dataconstr(f), x);

        if (!isfn(f)) raise(e->loc, "not function", f);

        if (!fnbody(f) && fnremain(f) == 1)
            return operate(e->loc, x, fnenv(f), fnop(f));

        if (!fnbody(f))
            return newnative(fnpar(f), env(x, fnenv(f)),
                fnremain(f) - 1, fnop(f));

        env = env(x, fnenv(f));
        e = fnbody(f);
        goto top;

    case ESEQ:
        eval(e->lhs, env);
        e = e->rhs;
        goto top;

    case EIF:
        x = eval(e->_if.a, env);
        if (!isbool(x)) raise(e->loc, "not bool", x);
        e = boolval(x)? e->_if.b: e->_if.c;
        goto top;

    case ELET:
        if (e->let.rec) {
            struct env *base = env;
            for (rules *i = e->let.rules; i; i = i->next)
                env = env(eval(i->rhs, env), env);
            for (struct env *i = env; i != base; i = i->next)
                fnsetenv(i->val, env);
        } else
            for (rules *i = e->let.rules; i; i = i->next) {
                x = eval(i->rhs, env);
                env = bind_pattern(i->lhs, x, env);
                if (!env) raise(i->lhs->loc, "could not bind pattern", x);
            }
        e = e->let.body;
        goto top;

    case ECASE:
        x = eval(e->_case.val, env);
        for (rules *i = e->_case.rules; i; i = i->next) {
            local = bind_pattern(i->lhs, x, env);
            if (!local) continue;
            e = i->rhs;
            env = local;
            goto top;
        }
        raise(e->loc, "could not bind pattern", x);

    case EFN:
        return newfn(e->fn.par, e->fn.body, env);
    }

    fatal(e->loc, "UNHANDLED: %d", e->form);
    return unit;
}

env *senv_to_env(senv *in) {
    return in? env(in->val, senv_to_env(in->next)): 0;
}

int main(int argc, char **argv) {
    for (toktype i = TID + 1; toks[i]; i++)
        toks[i] = cstr(toks[i]);
    for (int i = 0; i < 256; i++)
        single_char[i] = thestr(intern((char[]){i}, 1));
    ignore = cstr("_");

    senv *env = 0;
    env = senv(cstr(":"), newnative("", 0, 2, OP_CONS), env);
    env = senv(cstr("=="), newnative("", 0, 2, OP_EQUAL), env);
    env = senv(cstr("<>"), newnative("", 0, 2, OP_NOT_EQUAL), env);
    env = senv(cstr("<"), newnative("", 0, 2, OP_LESS), env);
    env = senv(cstr("<="), newnative("", 0, 2, OP_LESS_EQUAL), env);
    env = senv(cstr(">"), newnative("", 0, 2, OP_GREATER), env);
    env = senv(cstr(">="), newnative("", 0, 2, OP_GREATER_EQUAL), env);
    env = senv(cstr("+"), newnative("", 0, 2, OP_ADD), env);
    env = senv(cstr("-"), newnative("", 0, 2, OP_SUB), env);
    env = senv(cstr("*"), newnative("", 0, 2, OP_MUL), env);
    env = senv(cstr("/"), newnative("", 0, 2, OP_DIV), env);
    env = senv(cstr("rem"), newnative("", 0, 2, OP_REM), env);
    env = senv(cstr("join"), newnative("", 0, 1, OP_JOIN), env);
    env = senv(cstr("strlen"), newnative("", 0, 1, OP_STRLEN), env);
    env = senv(cstr("char_at"), newnative("", 0, 2, OP_CHAR_AT), env);
    env = senv(cstr("substr"), newnative("", 0, 3, OP_SUBSTR), env);
    env = senv(cstr("starts_with"), newnative("", 0, 2, OP_STARTS), env);
    env = senv(cstr("find_string"), newnative("", 0, 3, OP_FIND_STR), env);
    env = senv(cstr("ord"), newnative("", 0, 1, OP_ORD), env);
    env = senv(cstr("chr"), newnative("", 0, 1, OP_CHR), env);
    env = senv(cstr(":="), newnative("", 0, 2, OP_ASSIGN), env);
    env = senv(cstr("pr"), newnative("", 0, 1, OP_PR), env);
    env = senv(cstr("read_file"), newnative("", 0, 1, OP_READ_FILE), env);
    env = senv(cstr("write_file"), newnative("", 0, 2, OP_WRITE_FILE), env);

    node    *e = 0, **lastp = &e;
    opensrc("boot.ml");
    lastp = program(lastp, &env);
    opensrc(argv[1]);
    lastp = program(lastp, &env);
    *lastp = elit(sloc, unit);
    link_vars(e, env);
    eval(e, senv_to_env(env));
    puts("done.");
}
