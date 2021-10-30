
let rec perm_insert xs =
  let rec insert elem xs = 
    match xs with
    | [] -> [[elem]]
    | x::ys -> (elem::xs) :: (List.map (fun zs -> x::zs) (insert elem ys))
  in match xs with
  | [] -> [[]]
  | x::xs -> List.fold_right 
              (fun list acc -> (insert x list) @ acc) (perm_insert xs) []

let rec perm_choose xs =
  let rec rm elem xs = 
    match xs with
    | [] -> []
    | x::xs -> if x <> elem then x :: (rm elem xs) else xs
  in match xs with
  | [] -> [[]]
  | xs -> List.fold_right
    (fun x acc -> (List.map (fun xs -> x::xs) (perm_choose (rm x xs) )) @ acc ) xs []
