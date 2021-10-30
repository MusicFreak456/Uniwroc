let suffixes xs = 
  List.fold_right 
    (fun x acc -> (x :: List.hd acc) :: acc) 
    xs [[]]

let prefixes xs =
  List.fold_right
    (fun x acc -> [] :: List.map (fun xy -> x :: xy) acc)
    xs 
    [[]]