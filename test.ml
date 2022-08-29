let main =
print $ foldl_self (+) [1,2,3,4]
;print (

    case SOME 1
    | NONE -> "BAD"
    | SOME x -> ["GOOD", x]
)
; print (let [NONE] = [NONE] in "OK")
