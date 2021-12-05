let rec fold_left_cps f acc xs cont =
  match xs with
  | []    -> cont(acc)
  | x::xs -> f acc x (fun res ->
    fold_left_cps f res xs cont)

let fold_left f acc xs =
fold_left_cps (fun acc x cont -> cont(f acc x)) acc xs (fun res -> res)

let for_all p xs =
  let accumulate _ x cont = 
    if p x then cont(true)
    else false
  in fold_left_cps accumulate true xs (fun res -> res)

let mult_list xs =
  let accumulate acc x cont =
    if x = 0 then 0
    else cont(acc * x)
  in fold_left_cps accumulate 1 xs (fun res -> res)

let sorted xs =
  let accumulate acc x cont =
    let (max,_) = acc in
    if x < max then (x,false)
    else cont( (x,true) )
  in match xs with
  | [] -> true
  | x::xs ->
    let (_,res) = fold_left_cps accumulate (x, true) xs (fun res -> res) in
    res