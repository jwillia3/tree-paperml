# Boot Language

## Grammar
```
program:    infix/datatype/let                          Whole program

infix:      'infixl'/'infixr' int id...                 Infix declarations
datatype:   'datatype' [id] id '=' ['|'] constrdecs     Datatype declarations
constrdecs: id ['(' ... ')'] ['|' constrdecs]           Constructor declarations
let:        'let' ['rec'] decs                          Value declaration
decs:       aexpr aexpr... '=' expr ['and' decs]        Value declarations

expr:       'if' expr 'then' expr 'else' expr           Conditional
            'let' decs 'in' expr                        Local declarations
            'case' expr ('|' expr '->' expr)...         Case analysis
            iexpr                                       Infix expression
            expr ';' expr                               Sequence
iexpr:      aexpr aexpr... [id iexpr]                   Infix expression
aexpr:      true/false/int/string                       Constants
            id                                          Variable reference
            '(' expr,... ')'                            Tuple
            '[' expr,... ']'                            List
            'ref' aexpr                                 Reference creation
            '!' aexpr                                   Dereferencing
            'fn' id id... '->' expr                     Function
```

## Semantics
- `let` does sequential binding not simultaneous
- `let rec` only defines functions
- Equality is checked by value not by reference except `ref`s
- Evaluation order is undefined
- Datatype declarations are ignored other than defining constructors
- Valid patterns are:
    - literal constants
    - variables
    - tuples of patterns
    - lists of patterns
    - `x : y` where `x` and `y` are patterns
    - applications of constructors
