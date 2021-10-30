
let head s = s 0

let tail s = fun x -> s (x+1)

let add c s = fun n -> (c + (s n))

let map f s = fun n -> f(s n)

let map2 f s1 s2 = fun n -> (f (s1 n) (s2 n))

let replace n a s = fun x -> if x = n then a else (s x)

let take_every n s = fun x -> (s (x * n))

(* Testowanie implementacji *)

(* let print_int_endl x = string_of_int x |> print_endline
let rec natural_stream n = n *)

(* let _ = natural_stream 5 |> print_int_endl *)

(* let _ = head (tail (tail natural_stream)) |> print_int_endl *)

(* let _ = (add 2 natural_stream) 3 |> print_int_endl *)

(* let _ = (map (fun x -> x * 2) natural_stream) 3 |> print_int_endl *)

(* let _ = (map2 (+) natural_stream natural_stream) 2 |> print_int_endl *)

(* let new_stream = replace 2 5 natural_stream
let _ = new_stream 2 |> print_int_endl
let _ = new_stream 3 |> print_int_endl *)

(* let _ = (take_every 3 natural_stream) 2 |> print_int_endl *)