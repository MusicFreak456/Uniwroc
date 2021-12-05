let for_all p xs =
  let exception Result of bool in
  let accumulate _ x =
    if p x then true
    else raise (Result false)
  in 
  try List.fold_left accumulate true xs
  with exn ->
    match exn with
    | Result res -> res
    | _          -> raise exn

let mult_list xs =
  let exception Result of int in
  let accumulate acc x =
    if x = 0 then raise (Result 0)
    else acc * x
  in 
  try List.fold_left accumulate 1 xs 
  with exn ->
    match exn with
    | Result res -> res
    | _          -> raise exn

let sorted xs =
  let exception Result of bool in
  let accumulate acc x =
    if acc <= x then x
    else raise (Result false)
  in try match xs with
  | []    -> true
  | x::xs -> 
    let _ = List.fold_left accumulate x xs in
    true
  with exn ->
    match exn with
    | Result res -> res
    | _          -> raise exn