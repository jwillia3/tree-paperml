infixr 9 .
infixl 7 * / rem
infixl 6 + - ^
infixr 5 :
infixl 4 == <> < > <= >=
infixr 3 &&
infixr 2 ||
infixr 1 :=
infixl 0 <<
infixr 0 $

datatype a option = NONE | SOME (x)

let print x = pr x; pr "\n"; x

let not x = if x then false else true
let const x _ = x
let identity x = x
let flip f x y = f y x
let negate x = 0 - x
let $ f x = f x
let << x f = f x

#
#   Option
#

let map_option f x =
    case x
    | SOME val ->   SOME (f val)
    | NONE ->       NONE

let fold_option f x =
    case x
    | SOME _ -> f x
    | NONE ->   NONE

let filter_option accept x = fold_option (fn val -> if accept val then x else NONE)

let val_of x = let (SOME val) = x in val

let app_option f x =
    case x
    | SOME val ->   f x; ()
    | _ ->          ()


#
#   Strings
#

let ^ x y = join [x, y]

let islower c = let x = ord c in x >= 97 && x <= 122
let isupper c = let x = ord c in x >= 65 && x <= 90
let isdigit c = let x = ord c in x >= 48 && x <= 57
let isalpha c = let x = ord c in x >= 97 && x <= 122 || x >= 65 && x <= 90
let isalnum c =
    let x = ord c in
    x >= 97 && x <= 122 || x >= 65 && x <= 90 || x >= 48 && x <= 57
let has_string big small = find_string big 0 small <> -1
let doesnt_have_string big small = find_string big 0 small == -1


#
#   Lists
#

let hd xs = let (x:_) = xs in x
let tl xs = let (_:x) = xs in x
let reverse xs =
    let rec loop xs out =
        case xs
        | (x:xs) -> loop xs (x:out)
        | []     -> out
    in loop xs []
let map f xs =
    let rec loop xs out =
        case xs
        | (x:xs) ->     loop xs (f x : out)
        | [] ->         reverse out
    in loop xs []

let foldl combine start_value xs =
    let rec loop xs out =
        case xs
        | (x:xs) ->     loop xs (combine out x)
        | [] ->         out
    in loop xs start_value

let foldl_self combine xs = let (x:xs) = xs in foldl combine x xs

let foldr combine start_value xs =
    foldl (flip combine) start_value (reverse xs)

let foldr_self combine xs =
    let (x:xs) = reverse xs in
    foldl (flip combine) x xs

let filter accept xs =
    foldr (fn new rest -> if accept new then new:rest else rest) [] xs

let any accept xs =
    let rec loop xs =
        case xs
        | (x:xs) ->     accept x || loop xs
        | [] ->         false
    in loop xs

let all accept xs =
    let rec loop xs =
        case xs
        | (x:xs) ->     accept x && loop xs
        | [] ->         true
    in loop xs

let none accept xs = not (any accept xs)
