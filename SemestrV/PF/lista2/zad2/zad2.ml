let rec sublists_bad xs = 
  match xs with
  | [] -> [ [] ]
  | x::xs -> 
    let xs_subl = sublists_bad xs in
      List.append (List.map (fun ys -> x::ys) xs_subl) xs_subl 
      
let rec sublists xs =
  let rec append_map f xs ys = 
    match xs with 
    | [] -> ys 
    | x::xs -> (f x) :: append_map f xs ys 
  in match xs with
  | [] -> [[]]
  | x::xs -> 
    let xs_subl = sublists xs in
      append_map (fun ys -> x::ys) xs_subl xs_subl