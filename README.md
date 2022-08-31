# Boot Language

## Grammar
```
program:    infix/datatype/let                          Whole program

infix:      'infixl'/'infixr' int id...                 Infix declarations
datatype:   'datatype' ['(' id ')'] id '=' ['|'] cdec   Datatype declarations
cdec:       id ['(' type ')'] ['|' cdec]                Constructor declarations
let:        'let' ['rec'] decs                          Value declaration
decs:       aexpr aexpr... '=' expr ['and' decs]        Value declarations

type:       (id / '(' type,... ')') id... ['->' type]   Type

expr:       'if' expr 'then' expr 'else' expr           Conditional
            'let' decs 'in' expr                        Local declarations
            'case' expr rules                           Case analysis
            iexpr                                       Infix expression
            expr ';' expr                               Sequence
            expr "::" type                              Type assertion
iexpr:      aexpr aexpr... [id iexpr]                   Infix expression
aexpr:      true/false/int/string                       Constants
            id                                          Variable reference
            '(' expr,... ')'                            Tuple
            '[' expr,... ']'                            List
            '!' aexpr                                   Dereferencing
            'fn' id id... '->' expr                     Function

rules:      '|' expr ['if' expr] '->' expr [rules]      Case analysis rule
```

## Semantics
- `let` does sequential binding not simultaneous
- `let rec` only defines functions
- Equality is checked by value not by reference except `ref`s
- Evaluation is left-to-right
- Datatype declarations are ignored other than defining constructors
- Valid patterns are:
    - literal constants
    - variables
    - tuples of patterns
    - lists of patterns
    - `x : y` where `x` and `y` are patterns
    - applications of constructors
