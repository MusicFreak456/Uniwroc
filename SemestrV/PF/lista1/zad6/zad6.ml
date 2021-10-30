let print_bool_endl x = string_of_bool x |> print_endline

let ctrue x y = x

let cfalse x y = y

(* let cand a b = fun x y -> b (a x y) y *)

let cand a b = b a cfalse

let cor a b = b ctrue a

let cbool_of_bool b = if b then ctrue else cfalse

let bool_of_cbool cbool = cbool true false

let _ = 
    assert (
        cor ctrue ctrue true false = true &&
        cor ctrue cfalse true false = true &&
        cor cfalse ctrue true false = true &&
        cor cfalse cfalse true false = false &&
        cand ctrue ctrue true false = true &&
        cand ctrue cfalse true false = false &&
        cand cfalse ctrue true false = false &&
        cand cfalse cfalse true false = false
    ) 

let _ = 
    assert (
        (cbool_of_bool true) true false = true &&
        (cbool_of_bool false) true false = false &&
        (bool_of_cbool ctrue) = true &&
        (bool_of_cbool cfalse) = false
    )