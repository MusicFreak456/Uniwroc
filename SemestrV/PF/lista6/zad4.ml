let rec fold_left_cps f acc xs cont =
    match xs with
    | []    -> cont(acc)
    | x::xs -> f acc x (fun res ->
      fold_left_cps f res xs cont)

let fold_left f acc xs =
  fold_left_cps (fun acc x cont -> cont(f acc x)) acc xs (fun res -> res)