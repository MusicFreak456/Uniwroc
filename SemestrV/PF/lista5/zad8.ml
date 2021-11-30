type _ fin_type =
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type

let rec all_values : type a. a fin_type -> a Seq.t =
  fun t -> 
  match t with
  | Unit -> Seq.return ()
  | Bool -> Seq.cons true (Seq.return false)
  | Pair(left, right) ->
    let lvalues = all_values left in
    let rvalues = all_values right in
    let rec lazy_product s1 s2 = fun () ->
      match s1 () with
      | Seq.Nil -> Seq.Nil
      | Seq.Cons(x, xs) -> 
        begin match s2 () with
        | Seq.Nil -> lazy_product xs rvalues ()
        | Seq.Cons(y, ys) -> Seq.Cons( (x, y), lazy_product s1 ys)
        end
    in lazy_product lvalues rvalues

let list_of_seq seq =
  Seq.fold_left (fun acc x -> x :: acc) [] seq