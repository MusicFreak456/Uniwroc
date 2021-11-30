type 'a my_lazy = 'a lazy_meta ref
and 'a lazy_meta = 
| CurrentlyBeingComputed
| Value       of 'a
| Computation of (unit -> 'a)

let force lz =
  match !lz with
  | CurrentlyBeingComputed -> failwith "Value cannot be produced"
  | Value x -> x
  | Computation f -> 
    lz := CurrentlyBeingComputed;
    let value = f () in
    lz := Value value;
    value

let rec fix f = 
  let rec lazy_comp = 
    ref (Computation (fun () -> f lazy_comp)) 
  in lazy_comp

type 'a llist = 'a node my_lazy
and 'a node =
| Nill
| Cons of 'a * 'a llist

let rec nats_from n = 
  fix (fun ignore -> Cons(n, nats_from (n+1)))

let rec nth n xs = 
  match n, force xs with
  | _, Nill -> failwith "Out of bounds"
  | 0, Cons(x, _) -> x
  | _, Cons(_, xs) -> nth (n-1) xs
