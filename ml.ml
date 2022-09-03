datatype value =
| NIL
| BOOL (bool)
| INT (int)
| STRING (string)

datatype type =
| TYPEVAR (string, type option ref)
| REGULAR (string)
| TYPECON (type, string)
| FNTYPE (type, type)
| TUPTYPE (type list)

datatype expr =
| ELIT (string, value)
| EVAR (string, string)
| ETUP (string, expr list)
| ECONS (string, expr, expr)
| EDEREF (string, expr)
| EFN (string, string, expr)
| EAPP (string, expr, expr)
| EIF (string, expr, expr, expr)
| ELET (string, bool, (expr, expr) list, expr)
| ECASE (string, expr, (expr, expr) list)
| ETYPING (string, expr, type)

let typevar () = TYPEVAR ("UNRENAMED_TYPEVAR", ref NONE)
and named_typevar id = TYPEVAR (id, ref NONE)
and regular id = REGULAR (id)
and typecon arg id = TYPECON (arg, id)
and fntype par ret = FNTYPE (par, ret)
and tuptype list = TUPTYPE (list)

let elit loc val = ELIT (loc, val)
and evar loc id = EVAR (loc, id)
and etup loc list = ETUP (loc, list)
and econs loc lhs rhs = ECONS (loc, lhs, rhs)
and ederef loc arg = EDEREF (loc, arg)
and efn loc par body = EFN (loc, par, body)
and eapp loc lhs rhs = EAPP (loc, lhs, rhs)
and eif loc a b c = EIF (loc, a, b, c)
and elet loc is_rec decs body = ELET (loc, is_rec, decs, body)
and ecase loc val rules = ECASE (loc, val, rules)
and etyping loc expr type = ETYPING (loc, expr, type)


let fatal loc msg =
    print (join ["boot2: ", loc, ": ", msg]);
    exit 1

let bool_type = regular "bool"
let int_type = regular "int"
let string_type = regular "string"

let basis_types = [
    ("bool", bool_type),
    ("int", int_type),
    ("string", string_type),
    ("list", typecon (typevar ()) "list"),
    ("option", typecon (typevar ()) "option"),
    ("ref", typecon (typevar ()) "ref"),
]

let basis_constrs = [
    ("NONE", typecon (typevar ()) "option"),
    ("SOME", let a = typevar () in fntype a (typecon a "option")),
]






let loc_of expr =
    case expr
    | ELIT (loc, _) -> loc
    | EVAR (loc, _) -> loc
    | ETUP (loc, _) -> loc
    | ECONS (loc, _, _) -> loc
    | EDEREF (loc, _) -> loc
    | EFN (loc, _, _) -> loc
    | EAPP (loc, _, _) -> loc
    | EIF (loc, _, _, _) -> loc
    | ELET (loc, _, _, _) -> loc
    | ECASE (loc, _, _) -> loc
    | ETYPING (loc, _, _) -> loc





# Shorten the equality chain of typevars.
# Either way, return the most concrete type known now.
let rec prune type =
    case type
    | TYPEVAR (_, ref NONE) -> type
    | TYPEVAR (_, inst) ->
        let (SOME type') = !inst in
        let type'' = prune type' in
        inst := SOME type'';
        type''
    | _ -> type

let rec occurs_in var type =
    let type = prune type in
    case type
    | TYPEVAR (_, inst') -> let (TYPEVAR (_, inst)) = var in inst == inst'
    | REGULAR (_) -> false
    | TYPECON (type', _) -> var `occurs_in type'
    | FNTYPE (par, ret) -> var `occurs_in par || var `occurs_in ret
    | TUPTYPE (items) -> any (occurs_in var) items

let rec unifies a b =
    let a = prune a in
    let b = prune b in
    case (a, b)
    | (TYPEVAR (_, a'), _) ->
        if a `occurs_in b then
            a == b
        else
            a' := SOME b;
            true
    | (_, TYPEVAR (_, b')) -> unifies b a
    | (REGULAR a', REGULAR b') -> a' == b'
    | (TYPECON (a', a''), TYPECON (b', b'')) -> a'' == b'' && unifies a' b'
    | (FNTYPE (a', a''), FNTYPE (b', b'')) -> unifies a' b' && unifies a'' b''
    | (TUPTYPE a', TUPTYPE b') ->
        same_length a' b' && all (uncurry unifies) (zip a' b')
    | (_, _) -> false

let fresh nongenerics type =
    let swaps = ref [] in
    let rec loop type =
        let type = prune type in
        case type
        | TYPEVAR _ ->
            if any (occurs_in type) nongenerics then type else substitute type
        | REGULAR (_) -> type
        | TYPECON (arg, id) -> typecon (loop arg) id
        | FNTYPE (par, ret) -> fntype (loop par) (loop ret)
        | TUPTYPE (items) -> tuptype (map loop items)
    and substitute (TYPEVAR (_, inst)) =
        for_option (lookup inst !swaps) identity (fn () -> create_new inst)
    and create_new inst =
        let new = typevar () in
        swaps := (inst, new) : !swaps;
        new
    in loop type

# Most type variables are created without names.
# Even ones that are named must be renamed so no two typevars print the same.
# E.g. `fn (x :: a) (y :: a) -> 0` should be typed `a -> b -> int`.
let rename_type_vars types =
    let uid = ref 0 in
    let rec
    and get_vars seen type =
        case prune type
        | TYPEVAR (_, instance) ->
            for_option (lookup instance seen)
                (fn _ -> seen)
                (fn () -> rename seen instance)
        | REGULAR (id) -> seen
        | TYPECON (arg, id) -> get_vars seen arg
        | FNTYPE (par, ret) -> get_vars (get_vars seen par) ret
        | TUPTYPE (items) -> foldl get_vars seen items
    and rename seen instance =
        let id = chr (96 + (uid += 1)) in
        (instance, id) : seen
    and rebuild alist type =
        case prune type
        | TYPEVAR (_, instance) ->
            (case lookup instance alist | SOME id -> TYPEVAR (id, instance))
        | REGULAR (_) -> type
        | TYPECON (arg, id) -> typecon (rebuild alist arg) id
        | FNTYPE (par, ret) -> fntype (rebuild alist par) (rebuild alist ret)
        | TUPTYPE (items) -> tuptype (map (rebuild alist) items)
    in
    let alist = foldl get_vars [] types in
    map (rebuild alist) types

let rec repr_type type =
    let with_parens type =
        case type
        | FNTYPE _ -> join ["(", repr_type type, ")"]
        | _ -> repr_type type
    in
    case prune type
    | TYPEVAR (id, _) -> id
    | REGULAR (id) -> id
    | TYPECON (arg, id) -> join [with_parens arg, " ", id]
    | FNTYPE (par, ret) -> join [with_parens par, " -> ", repr_type ret]
    | TUPTYPE (items) -> "(" ^ join (separate_map ", " repr_type items) ^ ")"






# Scan tokens from source.
let scan filename source =
    let

    # Reserved words and token definitions.
    and resv = ["=", "if", "then", "else", "let", "rec", "and", "in",
        "true", "false", "case", "|", "fn", "->", "infixl", "infixr",
        "!", "datatype", "::"]
    and isid c = isalnum c || c == "_" || c == "'"
    and issym c = "!$%&*+-./:<=>?@^|~" `has_string c
    and ispun c = "()[],;`" `has_string c
    and check_resv (loc, _, id) =
        (loc, if resv `contains id then id else "id", id)

    # Scanner state.
    and _index = ref 0
    and _line = ref 1
    and _col = ref 1
    in

    let rec
    and get_loc () =
        join [filename, ":", int_to_string !_line, ":", int_to_string !_col]

    and error msg = fatal (get_loc ()) msg

    and peek () = if !_index < size source then source `char_at !_index else ""

    and advance () =
        let c = peek () in
        (if c == "\n" then _line += 1; _col := 1 else _col += 1);
        _index += 1;
        c

    and advance_while true_for =
        !_index < size source && true_for (source `char_at !_index) &&
            (advance (); advance_while true_for)

    and while true_for =
        let base = !_index - 1 in
        advance_while true_for;
        substr source base !_index

    and skip_space () =
        let c = peek () in
        if isspace c then advance_while isspace; skip_space ()
        else if c == "#" then advance_while (<> "\n"); skip_space ()
        else c

    and read_string () =
        let base = !_index in
        let rec loop () =
            case advance ()
            | "" -> error "unclosed string"
            | "\"" -> substr source base (!_index - 1)
            | "\\" -> advance (); loop ()
            | _ -> loop ()
        in loop ()

    and read_token () =
        let loc = get_loc () in
        let c = advance () in
        if isdigit c then (loc, "int", while isdigit)
        else if c == "-" && isdigit (peek ()) then (loc, "int", while isdigit)
        else if ispun c then (loc, c, c)
        else if c == "\"" then (loc, "string", read_string ())
        else if isid c then check_resv (loc, "id", while isid)
        else if issym c then check_resv (loc, "id", while issym)
        else error ("invalid token: " ^ c)

    and read_tokens tokens =
        if skip_space () == "" then
            (get_loc (), "end", "") : tokens
        else
            read_tokens (read_token () : tokens)

    in
    reverse (read_tokens [])





# Parse tokens into expressions.
let parse tokens =

    let

    and _feed = ref tokens
    and _tokloc = ref ""
    and _toktype = ref ""
    and _tokstr = ref ""

    and _infixes = ref []
    and _types = ref basis_types
    and _constrs = ref []

    and get_loc () = let ((l, _, _) : _) = !_feed in l

    and advance () =
        case !_feed
        | (_, "end", _) : _ -> ()
        | (l, t, s) : rest ->
            _feed := rest;
            _tokloc := l;
            _toktype := t;
            _tokstr := s

    and error_at loc msg = fatal loc msg
    and error_next msg = error_at (get_loc ()) msg
    and error_here msg = error_at !_tokloc msg

    and peek type = let ((_, t, _) : _) = !_feed in t == type
    and want type = peek type && (advance (); true)
    and need type = if want type then !_tokstr else error_next ("need " ^ type)

    and prefix type get = need type; get ()
    and suffix type get = let x = get () in need type; x

    and get_id () = need "id"

    and infix_next () =
        let ((_, type, id) : _) = !_feed in
        if type == "`" then SOME ("`", (9, 10))
        else if type == "id" then assoc id !_infixes
        else NONE

    in

    let rec

    and program () =
        # Top-level is only non-recursive.
        # Transform `let rec f ... and g ...`
        # to `let (f, g) = let rec f ... g ... in (f, g)`
        let transform_rec loc is_rec decs =
            if is_rec then
                let ids = etup loc (map fst decs) in
                [(ids, elet loc true decs ids)]
            else decs
        in

        let rec read_top_level all_decs =
            if want "infixl" || want "infixr" then
                infix_dec (!_tokstr == "infixl");
                read_top_level all_decs
            else if want "datatype" then
                datatype_dec ();
                read_top_level all_decs
            else if want "let" then
                let loc = !_tokloc in
                let is_rec = want "rec" in
                let decs = transform_rec loc is_rec (required_sequence dec) in
                read_top_level (decs ++ all_decs)
            else if want "end" then
                reverse all_decs
            else error_next "need top-level dec"

        in

        let loc = get_loc () in
        let all_decs = read_top_level [] in
        let body = elet loc false all_decs (etup loc []) in
        (!_types, !_constrs, !_infixes, body)

    and infix_dec is_left_assoc =
        let lhs = string_to_int (need "int") in
        let rhs = lhs + (if is_left_assoc then 1 else 0) in
        let ids = sequence (fn _ -> if want "id" then SOME !_tokstr else NONE) in
        app (fn id -> _infixes := (id, (lhs, rhs)) : !_infixes) ids

    and datatype_dec () =
        let arg_id = if want "(" then SOME (suffix ")" get_id) else NONE in
        let id = get_id () in
        is_none (lookup id !_types) ||
            error_here ("type redefined: " ^ id);
        let arg = map_option named_typevar arg_id in
        let dt = for_option arg (fn arg -> typecon arg id) (fn() -> regular id) in
        let types' = (id, dt) : !_types in

        _types := (if is_some arg then (arg_id, val_of arg) : types' else types');

        need "=";
        required_sequence (constr_dec dt);
        _types := types'

    and constr_dec dt first =
        if want "|" || first then
            let id = get_id () in
            is_none (lookup id !_constrs) ||
                error_here ("constructor redefined: " ^ id);
            let type = if peek "(" then fntype (type ()) dt else dt in
            _constrs := (id, type) : !_constrs;
            SOME ()
        else NONE

    and type () =
        let constr_app _ =
            if want "id" then
                case lookup !_tokstr !_types
                | SOME (TYPECON (_, id)) -> SOME id
                | SOME (_) -> error_here ("does not take arg: " ^ !_tokstr)
                | NONE -> error_here ("undefined type: " ^ !_tokstr)
            else NONE

        and atype () =
            if want "id" then
                case lookup !_tokstr !_types
                | SOME type -> type
                | NONE ->
                    let out = named_typevar !_tokstr in
                    _types := (!_tokstr, out) : !_types;
                    out
            else if want "(" then tuptype (csv type ")")
            else error_next "need type"

        and main () =
            let base = atype () in
            let out = foldl typecon base (sequence constr_app) in
            if want "->" then fntype out (type ()) else out

        in

        let old_types = !_types in
        let out = main () in
        _types := old_types; # Clean up any temporary type variables.
        out

    and expr () =
        let rec loop e =
            let loc = get_loc () in
            if want "::" then
                loop (etyping loc e (type ()))
            else if want ";" then
                loop (elet false loc [(evar loc "_", e)] (expr ()))
            else e
        in loop (_expr ())

    and _expr () =
        let loc = get_loc () in
        if want "if" then
            let a = expr () in
            let b = prefix "then" expr in
            let c = prefix "else" expr in
            eif loc a b c
        else if want "let" then
            let is_rec = want "rec" in
            let decs = required_sequence dec in
            let body = prefix "in" expr in
            elet loc is_rec decs body
        else if want "case" then
            let val = expr () in
            let rules = required_sequence rule in
            ecase loc val rules
        else
            iexpr 0

    and iexpr level =
        if level == 10 then
            let lhs = val_of (aexpr true) in
            case sequence aexpr
            | [] -> lhs
            | args -> foldl (fn f x -> eapp (loc_of x) f x) lhs args
        else
            let rec loop lhs =
                let loc = get_loc () in
                case infix_next ()
                | SOME ("`", (_, rhs_lvl)) ->
                    let id = advance (); get_id () in
                    let rhs = iexpr rhs_lvl in
                    loop (eapp loc (eapp loc (evar loc id) lhs) rhs)
                | SOME (":", (_, rhs_lvl)) ->
                    let rhs = advance (); iexpr rhs_lvl in
                    loop (econs (loc_of lhs) lhs rhs) # This loc improves errs.
                | SOME (_, (_, rhs_lvl)) ->
                    let id = advance () in
                    let rhs = iexpr rhs_lvl in
                    loop (eapp loc (eapp loc (evar loc id) lhs) rhs)
                | NONE ->
                    lhs
            in loop (iexpr (level + 1))

    and aexpr required =
        let loc = get_loc () in

        if not required && is_some (infix_next ()) then NONE

        else if want "int" then
            SOME (elit loc (INT (string_to_int !_tokstr)))
        else if want "true" then
            SOME (elit loc (BOOL true))
        else if want "false" then
            SOME (elit loc (BOOL false))
        else if want "string" then
            SOME (elit loc (STRING (unescape !_tokstr)))
        else if want "id" then
            SOME (evar loc !_tokstr)
        else if want "(" then
            SOME (case csv expr ")"
                  | [single] -> single
                  | list -> etup loc list)
        else if want "[" then
            # TODO: IMPROVE LOCATION OF CONS TO BE ON `[` AND `,`.
            let list = csv expr "]" in
            let result =
                foldr (fn hd tl -> econs (loc_of hd) hd tl)
                  (elit (get_loc ()) NIL)
                  list
            in SOME result
        else if want "!" then
            SOME (ederef loc (val_of (aexpr true)))
        else if want "fn" then
            SOME (fnexpr "->")
        else if required then
            error_next "need expression"
        else NONE

    and fnexpr delim =
        # Transform `fn pat -> body` to `fn new -> let pat = new in body`.
        let simplify_fn loc par body =
            case par
            | EVAR (_, id) -> efn loc id body
            | _ ->
                let new = evar (loc_of par) "\\NEW\\" in
                efn loc "\\NEW\\" (elet loc false [(par, new)] body)
        in

        if want delim then expr () else
        let loc = get_loc ()
        and par = val_of (aexpr true)
        and body = fnexpr delim
        in
        simplify_fn loc par body

    and rule first =
        if want "|" then
            let lhs = expr () in
            let rhs = prefix "->" expr in
            SOME (lhs, rhs)
        else if first then error_next "need rules"
        else NONE

    and dec first =
        if want "and" || first then
            let lhs = val_of (aexpr true) in
            let rhs = if want "=" then expr () else fnexpr "=" in
            SOME (lhs, rhs)
        else if first then error_next "need dec"
        else NONE

    and csv get delim =
        if want delim then [] else
        let item = get () in
        item : (if want "," then csv get delim else need delim; [])

    and sequence get =
        case get false
        | SOME item -> item : sequence get
        | NONE -> []

    and required_sequence get = val_of (get true) : sequence get

    in
    program ()

let check env expr =
    let _nongenerics = ref [] in

    let rec

    and check env expr =
        case expr
        | ELIT (_, val) ->
            type_val val

        | EVAR (_, id) ->
            for_option (lookup id env)
                (fresh !_nongenerics)
                (fn () -> fatal (loc_of expr) ("undefined: " ^ id))

        | ETUP (_, items) ->
            tuptype (map (check env) items)

        | ECONS (_, lhs, rhs) ->
            let lhs' = check env lhs in
            let rhs' = check env rhs in
            cons_type_check rhs lhs' rhs'

        | EDEREF (_, body) ->
            let body' = check env body in
            unify body (typecon (typevar ()) "ref") body'

        | EFN (_, par, body) ->
            with_nongenerics fn () ->
                let par' = typevar () in
                let local = define_nongeneric env par par' in
                let body' = check local body in
                fntype par' body'

        | EAPP (_, _, _) -> fatal (loc_of expr) "UNHANDLED"
        | EIF (_, _, _, _) -> fatal (loc_of expr) "UNHANDLED"
        | ELET (_, true, _, _) -> fatal (loc_of expr) "UNHANDLED"

        | ELET (_, false, decs, body) ->
            with_nongenerics fn () ->
                let env' = foldl check_dec env decs in
                check env' body

        | ECASE (_, _, _) -> fatal (loc_of expr) "UNHANDLED"
        | ETYPING (_, body, want) ->
            unify body want (check env body)

    and check_dec env (lhs, rhs) =
        with_nongenerics fn () ->
            let _local = ref env in
            let rhs' = check env rhs in
            let lhs' = check_pat _local lhs in
            unify rhs lhs' rhs';
            !_local

    and check_pat _env expr =
        case expr
        | ELIT (_, val) ->
            type_val val

        | EVAR (_, "_") ->
            typevar () `tap fn type -> _nongenerics := type : !_nongenerics

        | EVAR (_, id) ->
            typevar () `tap ref_define_nongeneric _env id

        | ETUP (_, items) ->
            tuptype (map (check_pat _env) items)

        | ECONS (_, lhs, rhs) ->
            let lhs' = check_pat _env lhs in
            let rhs' = check_pat _env rhs in
            cons_type_check rhs lhs' rhs'

        | EDEREF (_, body) -> invalid_pattern expr
        | EFN (_, _, _) -> invalid_pattern expr
        | EAPP (_, _, _) -> invalid_pattern expr
        | EIF (_, _, _, _) -> invalid_pattern expr
        | ELET (_, _, _, _) -> invalid_pattern expr
        | ECASE (_, _, _) -> invalid_pattern expr
        | ETYPING (_, body, want) ->
            unify body want (check_pat _env body)

    and invalid_pattern offending_expr =
         fatal (loc_of offending_expr) "invalid pattern"

    and define_nongeneric env id type =
        _nongenerics := type : !_nongenerics;
        (id, type) : env

    and ref_define_nongeneric _env id type =
        _env := define_nongeneric !_env id type

    and with_nongenerics block =
        let old = !_nongenerics in
        block () `tap fn _ -> _nongenerics := old

    and type_val val =
        case val
        | NIL -> typecon (typevar ()) "list"
        | BOOL _ -> bool_type
        | INT _ -> int_type
        | STRING _ -> string_type

    # This does extra logic to print a nicer error.
    and cons_type_check offending_expr want got =
        case got
        | TYPECON (got', "list") ->
            unify offending_expr want got';
            typecon want "list"
        | _ ->
            # This always fails. It just prints in a nicer way.
            unify offending_expr (typecon want "list") got

    and unify offending_expr want got =
        if not (unifies want got) then
            let [want, got] = rename_type_vars [want, got] in
            let msg = join [
                "type mismatch",
                "\nwant: ", repr_type want,
                "\ngot:  ", repr_type got] in
            fatal (loc_of offending_expr) msg
        else prune want

    in check env expr


let filename = "test.ml"
let (SOME source) = read_file filename
let tokens = scan filename source
let (types, constrs, infixes, program) = parse tokens
let basis = basis_constrs
let type = check basis program
let main =
    let [type] = rename_type_vars [type] in
    pr ":: ";
    print (repr_type type)
