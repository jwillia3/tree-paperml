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

datatype (a) ref = ref (a)
datatype (a) option = NONE | SOME (x)

let print x = pr x; pr "\n"; x

let cons x y = x : y

let not x = if x then false else true
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
let const x _ = x
let identity x = x
let flip f x y = f y x
let negate x = 0 - x
let . f g x = f (g x)
let $ f x = f x
let << x f = f x

let += rx n = rx := !rx + n

let fst (x, _) = x
let snd (_, x) = x


#
#   Option
#

let map_option if_some option =
    case option
    | SOME val ->   SOME (if_some val)
    | NONE ->       NONE

let fold_option if_some option =
    case option
    | SOME val ->   if_some option
    | NONE ->       NONE

let for_option option if_some if_none =
    case option
    | SOME val -> if_some val
    | NONE -> if_none ()

let on_none if_none option =
    case option
    | SOME _ ->     option
    | NONE ->       if_none ()

let filter_option accept x = fold_option (fn val -> if accept val then x else NONE)

let val_of (SOME val) = val

let app_option f x =
    case x
    | SOME val ->   f x; ()
    | _ ->          ()

let is_none x = case x | NONE -> true | SOME _ -> false
let is_some x = case x | NONE -> false | SOME _ -> true


#
#   Strings
#

let ^ x y = join [x, y]

let isspace c = c == " " || c == "\n" || c == "\t" || c == "\r"
let islower c = let x = ord c in x >= 97 && x <= 122
let isupper c = let x = ord c in x >= 65 && x <= 90
let isdigit c = let x = ord c in x >= 48 && x <= 57
let isalpha c = let x = ord c in x >= 97 && x <= 122 || x >= 65 && x <= 90
let isalnum c =
    let x = ord c in
    x >= 97 && x <= 122 || x >= 65 && x <= 90 || x >= 48 && x <= 57
let has_string big small = is_some (find_string big 0 small)
let doesnt_have_string big small = is_none (find_string big 0 small)


#
#   Lists
#

let hd (x : _) = x

let tl (_ : x) = x

let length list =
    let rec loop list out =
        case list
        | _ : rest -> loop rest (out + 1)
        | [] -> out
    in loop list 0

let reverse list =
    let rec loop list out =
        case list
        | x : rest -> loop rest (x : out)
        | [] -> out
    in loop list []

let rec same_length left right =
    case (left, right)
    | (_ : left', _ : right') -> same_length left' right'
    | ([], []) -> true
    | ([], _) -> false
    | (_, []) -> false

let zip left right =
    let rec loop left right out =
        case (left, right)
        | (x : left', y : right') -> loop left' right' ((x, y) : out)
        | ([], []) -> out
        | (_ : _, []) -> out
        | ([], _ : _) -> out
    in reverse (loop left right [])

let map transform list =
    let rec loop list out =
        case list
        | x : rest -> loop rest (transform x : out)
        | [] -> reverse out
    in loop list []

let mapi transform list =
    let rec loop list i out =
        case list
        | x : rest -> loop rest (i + 1) (transform i x : out)
        | [] -> reverse out
    in loop list 0 []

let app action list =
    let rec loop list =
        case list
        | x : rest -> action x; loop rest
        | [] -> ()
    in loop list

let appi action list =
    let rec loop i list =
        case list
        | x : rest -> action i x; loop rest
        | [] -> ()
    in loop 0 list

let foldl combine first_value list =
    let rec loop list out =
        case list
        | x : rest -> loop rest (combine out x)
        | [] -> out
    in loop list first_value

let foldl_self combine (first : rest) = foldl combine first rest

let foldr combine start_value list =
    foldl (flip combine) start_value (reverse list)

let foldr_self combine list =
    let (rightmost : rest) = reverse list in
    foldl (flip combine) rightmost rest

let append before_list after_list = foldr (:) after_list before_list

let ++ left right = append left right

let flatten list_of_lists = foldr append [] list_of_lists

let flat_map transform list = flatten (map transform list)

let sum list_of_ints = foldl (+) 0 list_of_ints

let replicate size value =
    let rec loop n out = if n < 1 then out else loop (n - 1) (value : out)
    in loop size []

let filter true_for list =
    foldr (fn new rest -> if true_for new then new : rest else rest) [] list

let any true_for list =
    let rec loop list =
        case list
        | (x : rest) -> true_for x || loop rest
        | [] -> false
    in loop list

let all true_for list =
    let rec loop list =
        case list
        | (x : rest) -> true_for x && loop rest
        | [] -> true
    in loop list

let none true_for list = not (any true_for list)

let contains list item = any (== item) list

let find true_for list =
    let rec loop list =
        case list
        | (x : rest) -> if true_for x then SOME x else loop rest
        | [] -> NONE
    in loop list

let assoc key alist =
    let rec loop list =
        case list
        | ((k, v) : rest) -> if k == key then SOME (hd list) else loop rest
        | [] -> NONE
    in loop alist

let lookup key alist = map_option snd (assoc key alist)

let rec int_to_string n =
    if n == 0 then "0" else
    if n < 0 then "-" ^ int_to_string (0 - n) else
    let rec loop n out =
        if n == 0 then join out else
        loop (n / 10) ("0123456789" `char_at (n rem 10) : out)
    in loop n []

let rec string_to_int str =
    let rec loop i out =
        if i >= size str then
            out
        else
            let c = str `char_at i in
            if not (isdigit c) then
                out
            else
                loop (i + 1) (out * 10 + ord c - 48)
    in
    if str `starts_with "-" then
        0 - loop 1 0
    else
        loop 0 0

let unescape str =
    let rec loop i out =
        if i < size str then
            case find_string str i "\\"
            | NONE ->   substr str i -1 : out
            | SOME j -> let x = substr str i j
                        and e = if j + 1 < size str
                                then str `char_at (j + 1)
                                else ""
                        and c = case e
                                | "a" ->    "\a"
                                | "b" ->    "\b"
                                | "e" ->    "\e"
                                | "n" ->    "\n"
                                | "r" ->    "\r"
                                | "t" ->    "\t"
                                | c ->      c
                        in loop (j + 2) (c : x : out)
        else
            out
    in join (reverse (loop 0 []))

let escape str =
    let rec
    and find_escape i =
        if i < size str then
            case str `char_at i
            | "\a" ->       (i, "\\a")
            | "\b" ->       (i, "\\b")
            | "\e" ->       (i, "\\e")
            | "\n" ->       (i, "\\n")
            | "\t" ->       (i, "\\t")
            | "\"" ->       (i, "\\\"")
            | "\\" ->       (i, "\\\\")
            | _ ->          find_escape (i + 1)
        else
            (i, "")

    and loop i out =
        case find_escape i
        | (j, "") ->    substr str i j : out
        | (j, repl) ->  loop (j + 1) (repl : substr str i j : out)
    in join (reverse (loop 0 []))

let separate_map separator transform list =
    case list
    | [] -> []
    | [x] -> [transform x]
    | x : rest ->
        transform x : flat_map (fn i -> [separator, transform i]) rest

let separate separator list =
    separate_map identity separator list
