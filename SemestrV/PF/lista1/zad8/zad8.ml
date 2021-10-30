type cbool = { cbool : 'a. 'a -> 'a -> 'a }


(* let ctrue x y = x *)
let ctrue = { cbool = fun x y -> x }

(* let cfalse x y = y *)
let cfalse = { cbool = fun x y -> y }

(* let cand a b  = a b cfalse *)

let cand a b = a.cbool b cfalse

(* let cor a b = a ctrue b *)
let cor a b = a.cbool ctrue b

(* let cnot a = a cfalse ctrue *)
let cnot a = a.cbool cfalse ctrue

let cbool_of_bool b = if b then ctrue else cfalse

let bool_of_cbool cbool = cbool.cbool true false


(* ************************************************ *)
type cnum = { cnum : 'a. ('a -> 'a) -> 'a -> 'a }

let zero = { cnum = fun f x -> x }

let succ pred = { cnum = fun f x -> f (pred.cnum f x) }

let add a b = { cnum = fun f x -> a.cnum f (b.cnum f x) }

let mul a b = { cnum = fun f x -> a.cnum (b.cnum f) x }

let is_zero cnum = cnum.cnum (fun x -> cfalse) ctrue 

let rec cnum_of_int x = if x = 0 then zero else succ (cnum_of_int (x - 1))

let int_of_cnum cnum = cnum.cnum (fun x -> x + 1) 0 

(* ************************************************ *)

let _ = 
  assert (
      (cor ctrue ctrue).cbool true false = true &&
      (cor ctrue cfalse).cbool true false = true &&
      (cor cfalse ctrue).cbool true false = true &&
      (cor cfalse cfalse).cbool true false = false &&
      (cand ctrue ctrue).cbool true false = true &&
      (cand ctrue cfalse).cbool true false = false &&
      (cand cfalse ctrue).cbool true false = false &&
      (cand cfalse cfalse).cbool true false = false
  ) 

let _ = 
  assert (
      (cbool_of_bool true).cbool true false = true &&
      (cbool_of_bool false).cbool true false = false &&
      (bool_of_cbool ctrue) = true &&
      (bool_of_cbool cfalse) = false
  )


let _ = assert (
    (is_zero zero).cbool true false = true &&
    int_of_cnum (succ zero) = 1 &&
    (is_zero (cnum_of_int 0)).cbool true false = true &&
    (is_zero (cnum_of_int 5)).cbool true false = false &&
    int_of_cnum (cnum_of_int 5) = 5 &&
    int_of_cnum (add (cnum_of_int 3) (cnum_of_int 4)) = 3 + 4 &&
    (int_of_cnum (add (cnum_of_int 2) (cnum_of_int 2)) = 5) = false &&
    int_of_cnum (mul (cnum_of_int 3) (cnum_of_int 4)) = 3 * 4
)