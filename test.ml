infixr 9 .
infixl 7 * / rem
infixl 6 + - ^
infixr 5 : ++
infixl 4 == <> < > <= >=
infixr 3 &&
infixr 2 ||
infixr 1 := +=
infixl 0 <<
infixr 0 $

datatype just_checking = JUST_CHECKING
datatype (a) qwert = Q (a->int)

let _ = 0
