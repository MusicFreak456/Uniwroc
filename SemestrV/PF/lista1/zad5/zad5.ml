
let rec natural_stream n = n

let rec tabulate s ?(b=0) e =
  if e < b 
    then []
    else (s b) :: (tabulate s ~b:(b+1) e) 


let rec print_list l = match l with
    | [] -> ()
    | h::t -> h |> string_of_int |> print_endline; print_list t

let _ = tabulate natural_stream 5 |> print_list