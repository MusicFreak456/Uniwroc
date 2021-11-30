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
| Nil
| Cons of 'a * 'a llist

let rec nats_from n = 
  fix (fun ignore -> Cons(n, nats_from (n+1)))

let rec nth n xs = 
  match n, force xs with
  | _, Nil         -> failwith "Out of bounds"
  | 0, Cons(x, _)  -> x
  | _, Cons(_, xs) -> nth (n-1) xs

let rec take_while p xs =
  fix begin fun ignore ->
    match force xs with
    | Cons(x, xs) when p x -> Cons(x, take_while p xs)
    | _ -> Nil
  end

let rec to_list llist = 
  match force llist with
  | Nil -> []
  | Cons(x, xs) -> x :: to_list xs

let rec forall p xs =
  match force xs with
  | Nil         -> true
  | Cons(x, xs) -> p x && forall p xs

let rec filter p xs =
  fix begin fun ignore -> 
    match force xs with
    | Nil -> Nil
    | Cons(x, xs) when p x -> Cons(x, filter p xs)
    | Cons(_, xs) -> force (filter p xs)
  end

let rec primes =
  fix begin fun primes -> 
    let is_prime n =  (
      primes 
      |> take_while (fun p -> p * p <= n)
      |> forall (fun p -> n mod p <> 0) )
    in Cons(2, filter is_prime (nats_from 3))
  end