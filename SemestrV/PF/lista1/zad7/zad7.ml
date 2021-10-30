
let print_bool_endl x = string_of_bool x |> print_endline

let ctrue x y = x

let cfalse x y = y

let cand a b  = b a cfalse

let cor a b = b ctrue a

let cnot a = a cfalse ctrue

let cbool_of_bool b = if b then ctrue else cfalse

let bool_of_cbool cbool = cbool true false

(* ************************************************ *)

let zero f x = x

let succ pred = fun f x -> f (pred f x)

let add a b = fun f x -> a f (b f x) 

let mul a b = fun f x -> a (b f) x

let is_zero cnum = cnum (fun x -> cfalse) ctrue

let rec cnum_of_int x = if x = 0 then zero else succ (cnum_of_int (x - 1))

let int_of_cnum cnum = cnum (fun x -> x + 1) 0

(* ***********************Testy******************** *)

let _ = assert (
    is_zero zero true false = true &&
    int_of_cnum (succ zero) = 1 &&
    is_zero (cnum_of_int 0) true false = true &&
    is_zero (cnum_of_int 5) true false = false &&
    int_of_cnum (cnum_of_int 5) = 5 &&
    int_of_cnum (add (cnum_of_int 3) (cnum_of_int 4)) = 3 + 4 &&
    (int_of_cnum (add (cnum_of_int 2) (cnum_of_int 2)) = 5) = false &&
    int_of_cnum (mul (cnum_of_int 3) (cnum_of_int 4)) = 4 * 3
)