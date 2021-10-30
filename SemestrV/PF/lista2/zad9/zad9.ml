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

let exp a b = { cnum = fun f x -> (a.cnum b.cnum) f x }

(* 
  (two three)
  => (λf.λx (f (f x))) three
  => λx. (three (three x))
  => λx. (three ((λg.λy. (g (g (g y)))) x))
  => λx. (three (λy. (x (x (x y)))) )
  => λx. (λg.λz. (g (g (g z))) (λy. x(x(x(y)))) )
  => λx.λz. ((λy. x(x(x(y))))
              ((λy. x(x(x(y))))
                ((λy. x(x(x(y)))) z)) )
  => λx.λz. (x (x (x (x (x (x (x (x (x z)))))))))
*)

let is_zero cnum = cnum.cnum (fun x -> cfalse) ctrue 

let rec cnum_of_int x = if x = 0 then zero else succ (cnum_of_int (x - 1))

let int_of_cnum cnum = cnum.cnum (fun x -> x + 1) 0 

(* ************************************************ *)
type 'a clist = { clist : 'z. ('a -> 'z -> 'z) -> 'z -> 'z }

let cnil = { clist = fun f x -> x  }

let ccons a cxs = { clist = fun f x -> f a (cxs.clist f x) }

let map g cxs = { clist = fun f x -> cxs.clist (fun a z -> f (g a) z) x }

let append cxs cys = { clist = fun f x -> cxs.clist f (cys.clist f x) }

let clist_to_list clist = clist.clist (fun a z -> a :: z) []

let clist_of_list xs = List.fold_right (fun x acc -> ccons x acc) xs cnil

let prod cxs cys = { clist = 
  fun f x -> 
    cxs.clist (fun a pxs -> cys.clist (fun b pys -> (f (a,b) pys)) pxs ) x }

let prod_list cxs cys = { clist = 
  fun f x -> 
    cxs.clist (fun a pxs -> cys.clist (fun b pys -> (f (ccons a b) pys)) pxs ) x }

let pow clist cnum = 
  (cnum.cnum
    (fun acc -> prod_list clist acc)
    (clist_of_list [cnil]))

(* let exp2 cxs = { clist = 
  fun f x -> 
    cxs.clist (fun a pxs -> cxs.clist (fun b pys -> (f [a;b] pys)) pxs ) x }

let exp2 cxs = { clist = 
  fun f x -> 
    cxs.clist (fun a pxs -> cxs.clist (fun b pys -> (f [a;b] pys)) pxs ) x } *)

(* let prod cxs cys = { clist = 
  fun f x -> 
    cxs.clist (fun a pxs -> 
      (cys.clist (fun b pys -> 
        (cys.clist (fun c pzs -> f (a,b) pzs) pys)) pxs)) x } *)

let _ = assert (clist_to_list cnil = [])
let _ = assert (clist_to_list (ccons 42 cnil) = [42])
let _ = assert (clist_to_list (map (( * ) 2) (ccons 4 (ccons 2 cnil))) = [8;4])
let _ = assert (clist_to_list (append (ccons 4 (ccons 2 cnil)) (ccons 7 cnil)) = [4;2;7])
let _ = assert (clist_to_list (clist_of_list [1,2,5]) = [1,2,5])