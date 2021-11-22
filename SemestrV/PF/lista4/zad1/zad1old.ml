type 'a nlist =
| Nil
| Zero of ('a * 'a) nlist
| One  of 'a * ('a * 'a) nlist

let rec cons : 'a. 'a -> 'a nlist -> 'a nlist =
  fun x xs ->
  match xs with
  | Nil           -> One(x, Nil)
  | Zero xs       -> One(x, xs)
  | One(y, xs)    -> Zero(cons (x, y) xs)

let rec view : 'a. 'a nlist -> ('a * 'a nlist) option =
  function
  | Nil           -> None
  | Zero xs       ->
    begin match view xs with 
    | None             -> None
    | Some((x, y), xs) -> Some(x, One(y, xs))
    end
  | One(x, xs)    -> Some(x, Zero xs)

let rec nth : 'a. 'a nlist -> int -> 'a =
  fun xs n ->
  match xs with
  | Nil -> raise Not_found
  | Zero xs ->
    let (x, y) = nth xs (n / 2) in
    if n mod 2 = 0 then x
    else y
  | One(x, xs) ->
    if n = 0 then x
    else nth (Zero xs) (n-1)

let head nlist = 
  match view nlist with
  | None -> failwith "head applied to empty list"
  | Some(hd,_) -> hd

let tail nlist = 
  match view nlist with
  | None -> failwith "tail applied to empty list"
  | Some(_,tl) -> tl

let nlist_of_list list = 
  List.fold_right (fun x acc -> cons x acc) list Nil

let rec list_of_nlist nlist =
  match view nlist with
  | None -> []
  | Some(head, tail) -> 
    List.cons head (list_of_nlist tail)

let empty_nlist = nlist_of_list [] 
let test_nlist  = nlist_of_list [1;2;3;4] 

let _ = assert( empty_nlist |> list_of_nlist = [] )
let _ = assert( test_nlist  |> list_of_nlist = [1;2;3;4] )
let _ = assert( test_nlist  |> head          = 1 )
let _ = assert( test_nlist  |> tail |> head  = 2 )
let _ = assert( nth test_nlist 0 = 1 )
let _ = assert( nth test_nlist 3 = 4 )

let time f x= 
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx

(* let benchmark n = 
  let rec benchmark_aux n size nlist = 
    if n = 0 then ()
    else let x = Random.int (Int.shift_left 1 20) in
      if x mod 2 = 0 || size = 0 then 
        benchmark_aux (n - 1) (size + 1) (cons x nlist)
      else 
        benchmark_aux (n - 1) (size - 1) (tail nlist)
  in benchmark_aux n 1 (cons 1 empty_nlist)

let _ = 
  Random.init 42;
  time benchmark 100000000 *)

(* cezary@cezary-MSI$ ocamlc zad1old.ml                                                                                                                       ~/Uniwroc/SemestrV/PF/lista4/zad1
cezary@cezary-MSI$ ./a.out                                                                                                                                 ~/Uniwroc/SemestrV/PF/lista4/zad1
Execution time: 15.649800s
cezary@cezary-MSI$ ./a.out                                                                                                                                 ~/Uniwroc/SemestrV/PF/lista4/zad1
Execution time: 15.910877s
cezary@cezary-MSI$ ./a.out                                                                                                                                 ~/Uniwroc/SemestrV/PF/lista4/zad1
Execution time: 16.595481s
cezary@cezary-MSI$ ./a.out                                                                                                                                 ~/Uniwroc/SemestrV/PF/lista4/zad1
Execution time: 16.399116s *)

let benchmark n = 
  let rec add n nlist =
    match n with 
    | 0 -> nlist
    | x -> add (n - 1) (cons x nlist)
  in let rec add_remove n is_add nlist =
    match n with
    | 0 -> ()
    | x -> 
      if not is_add then
        add_remove (n - 1) true (tail nlist)
      else
        add_remove (n - 1) false (cons x nlist)
  in 
  let list = add (Int.shift_left 1 15) empty_nlist in
  add_remove n false list 

let _ = time benchmark 100000