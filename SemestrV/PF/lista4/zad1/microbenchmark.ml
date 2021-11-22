open Zad1
open Zad1old

module type Nlist = sig
  type 'a nlist
  val cons : 'a -> 'a nlist -> 'a nlist
  val tail : 'a nlist -> 'a nlist
  val empty_nlist : 'a nlist
end

let time f (module M : Nlist) x= 
  let t = Sys.time() in
  let fx = f (module M : Nlist) x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx

let benchmark (module M : Nlist) n = 
  let rec benchmark_aux n size nlist = 
    if n = 0 then ()
    else let x = Random.int (Int.shift_left 1 20) in
      if x mod 2 = 0 || size = 0 then 
        benchmark_aux (n - 1) (size + 1) (M.cons x nlist)
      else 
        benchmark_aux (n - 1) (size - 1) (M.tail nlist)
  in benchmark_aux n 1 (M.cons 1 M.empty_nlist)

let _ = 
  Random.self_init ();
  let state = Random.get_state () in
    time benchmark (module Zad1old) 100000;
    Random.set_state state;
    time benchmark (module Zad1old) 100000;
    Random.set_state state;
    time benchmark (module Zad1) 100000;
    Random.set_state state;
    time benchmark (module Zad1) 100000;