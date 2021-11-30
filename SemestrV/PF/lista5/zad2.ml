
(* Typy rekurencyjne *)

type ('a, 'b) recFun = { func : ('a, 'b) recFun -> ('a -> 'b) }

let fix f x =
  let y = { func = fun y x -> f (y.func y) x } in
  f (y.func y) x

let fib_f fib n =
  if n <= 1 then n
  else fib (n-1) + fib (n-2)
  
let fib = fix fib_f

(* Mutowalny stan *)

let fix f x =
  let fix_ref0 = ref (fun f x -> failwith "") in
  let fix_ref1 = ref (fun f x -> f (!fix_ref0 f) x) in
  fix_ref0 := !fix_ref1;
  !fix_ref1 f x

let fib_f fib n =
  if n <= 1 then n
  else fib (n-1) + fib (n-2)

let fib = fix fib_f
