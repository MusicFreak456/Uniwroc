

let length xs = List.fold_left (fun acc _ -> acc + 1) 0 xs

let rev xs = List.fold_left (fun acc x -> x :: acc) [] xs

let map f xs = List.fold_right (fun x acc -> f x :: acc ) xs []

let append xs ys = List.fold_right (fun x acc -> x :: acc) xs ys

let rev_append xs ys = List.fold_left (fun acc x -> x :: acc) ys xs

let filter p xs = List.fold_right (fun x acc -> if p x then x :: acc else acc) xs []

let rev_map f xs = List.fold_left (fun acc x -> f x :: acc) [] xs

let test_list = [6;2;9;0;1;2;5]
let _ = assert (length test_list = List.length test_list)
let _ = assert (rev test_list = List.rev test_list)
let _ = assert (map (( * ) 2) test_list = List.map (( * ) 2) test_list)
let _ = assert (append [2;1;3] test_list = List.append [2;1;3] test_list)
let _ = assert (rev_append [2;1;3] test_list = List.rev_append [2;1;3] test_list)
let _ = assert (filter (fun x -> x mod 2 = 0) test_list = List.filter (fun x -> x mod 2 = 0) test_list)
let _ = assert (rev_map (( * ) 2) test_list = List.rev_map (( * ) 2) test_list )
