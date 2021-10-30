let print_int_endl x = string_of_int x |> print_endline

let rec natural_stream n = n

let rec scan f a s = fun n ->
    if n = 0 
        then f a (s 0)
        else f ((scan f a s) (n-1)) (s n)

let _ = scan (+) 0 natural_stream 3 |> print_int_endl