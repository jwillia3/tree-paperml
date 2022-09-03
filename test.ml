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

let _ =
(
    let f x = 1
    and g x = (f, f)
    in g
) :: just_checking
