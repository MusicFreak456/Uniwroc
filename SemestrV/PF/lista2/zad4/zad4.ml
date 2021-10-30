
let rec merge cmp xs ys =
  match xs, ys with
  | [],xs | xs,[] -> xs
  | (x::xs_tail as xs), (y::ys_tail as ys) -> 
      if cmp x y 
        then x :: merge cmp xs_tail ys 
        else y :: merge cmp xs ys_tail

let merge_tailrec cmp xs ys =
  let rec rev_append xs ys = 
    match xs with
    | [] -> ys
    | x::xs -> rev_append xs (x :: ys)
  and merge_aux xs ys acc =
    match xs, ys with
    | [],xs | xs,[] -> rev_append acc xs
    | (x::xs_tail as xs), y::ys when cmp y x-> 
        merge_aux xs ys (y :: acc)
    | x::xs, (y::ys_tail as ys) ->
        merge_aux xs ys (x :: acc)
  in merge_aux xs ys []

(* let n_list n =
  let rec n_list_aux acc n =
    match n with
    | 0 -> acc
    | _ -> n_list_aux (n::acc) (n-1)
  in n_list_aux [] n *)

let halve xs = 
  let rec halve_aux slow fast acc =
    match slow,fast with
    | xs,[] | xs,[_] -> (List.rev acc, xs)
    | x::xs, _::_::ys -> halve_aux xs ys (x :: acc)
    | [],_ -> assert false
  in halve_aux xs xs []

let rec mergesort cmp xs = 
  match xs with
  | [] -> []
  | [x] as singleton -> singleton
  | xs -> 
      let (fh, sh) = halve xs 
        in merge_tailrec cmp (mergesort cmp fh) (mergesort cmp sh)


let _ = assert (merge ( <= ) [] [] = [])
let _ = assert (merge ( <= ) [] [1] = [1])
let _ = assert (merge ( <= ) [1] [] = [1])
let _ = assert (merge ( <= ) [1] [1] = [1; 1])
let _ = assert (merge ( <= ) [1] [2] = [1; 2])
let _ = assert (merge ( <= ) [2] [1] = [1; 2])
let _ = assert (merge ( <= ) [1] [2; 3] = [1; 2; 3])
let _ = assert (merge ( <= ) [2; 3] [1] = [1; 2; 3])
let _ = assert (merge ( <= ) [1; 2; 3] [4; 5; 6] = [1; 2; 3; 4; 5; 6])
let _ = assert (merge ( <= ) [4; 5; 6] [1; 2; 3] = [1; 2; 3; 4; 5; 6])
let _ = assert (merge ( <= ) [1; 3; 7] [2; 4; 5; 6] = [1; 2; 3; 4; 5; 6; 7])
let _ = assert (merge ( <= ) [4] [1; 2; 3; 5; 6] = [1; 2; 3; 4; 5; 6])
let _ = assert (merge ( <= ) [4] [1; 2; 3; 4; 5; 6] = [1; 2; 3; 4; 4; 5; 6])
let _ = assert (merge ( <= ) [2; 5] [1; 4] = [1; 2; 4; 5])
let _ = assert (merge ( <= ) [1; 2; 3] [4; 5; 6; 7] = [1; 2; 3; 4; 5; 6; 7])
let _ = assert (merge ( <= ) [4; 5; 6; 7] [1; 2; 3] = [1; 2; 3; 4; 5; 6; 7])
let _ = assert (merge ( <= ) [1; 2; 5] [3; 4; 5] = [1; 2; 3; 4; 5; 5])
let _ = assert (merge_tailrec ( <= ) [] [] = [])
let _ = assert (merge_tailrec ( <= ) [] [1] = [1])
let _ = assert (merge_tailrec ( <= ) [1] [] = [1])
let _ = assert (merge_tailrec ( <= ) [1] [1] = [1; 1])
let _ = assert (merge_tailrec ( <= ) [1] [2] = [1; 2])
let _ = assert (merge_tailrec ( <= ) [2] [1] = [1; 2])
let _ = assert (merge_tailrec ( <= ) [1] [2; 3] = [1; 2; 3])
let _ = assert (merge_tailrec ( <= ) [2; 3] [1] = [1; 2; 3])
let _ = assert (merge_tailrec ( <= ) [1; 2; 3] [4; 5; 6] = [1; 2; 3; 4; 5; 6])
let _ = assert (merge_tailrec ( <= ) [4; 5; 6] [1; 2; 3] = [1; 2; 3; 4; 5; 6])
let _ = assert (merge_tailrec ( <= ) [1; 3; 7] [2; 4; 5; 6] = [1; 2; 3; 4; 5; 6; 7])
let _ = assert (merge_tailrec ( <= ) [4] [1; 2; 3; 5; 6] = [1; 2; 3; 4; 5; 6])
let _ = assert (merge_tailrec ( <= ) [4] [1; 2; 3; 4; 5; 6] = [1; 2; 3; 4; 4; 5; 6])
let _ = assert (merge_tailrec ( <= ) [2; 5] [1; 4] = [1; 2; 4; 5])
let _ = assert (merge_tailrec ( <= ) [1; 2; 3] [4; 5; 6; 7] = [1; 2; 3; 4; 5; 6; 7])
let _ = assert (merge_tailrec ( <= ) [4; 5; 6; 7] [1; 2; 3] = [1; 2; 3; 4; 5; 6; 7])
let _ = assert (merge_tailrec ( <= ) [1; 2; 5] [3; 4; 5] = [1; 2; 3; 4; 5; 5])
let _ = assert (halve [] = ([], []))
let _ = assert (halve [1] = ([], [1]))
let _ = assert (halve [1; 2] = ([1], [2]))
let _ = assert (halve [1; 2; 3] = ([1], [2; 3]))
let _ = assert (halve [1; 2; 3; 4; 5; 6] = ([1; 2; 3], [4; 5; 6]))
let _ = assert (halve [1; 2; 3; 4; 5; 6; 7] = ([1; 2; 3], [4; 5; 6; 7]))
let _ = assert (halve [1; 6; 7; 3; 2; 1; 7] = ([1; 6; 7], [3; 2; 1; 7]))
let _ = assert (mergesort ( <= ) [] = [])
let _ = assert (mergesort ( <= ) [1] = [1])
let _ = assert (mergesort ( <= ) [1; 2] = [1; 2])
let _ = assert (mergesort ( <= ) [2; 1] = [1; 2])
let _ = assert (mergesort ( <= ) [1; 2; 3] = [1; 2; 3])
let _ = assert (mergesort ( <= ) [3; 2; 1] = [1; 2; 3])
let _ = assert (mergesort ( <= ) [2; 3; 1] = [1; 2; 3])
let _ = assert (mergesort ( <= ) [1; 6; 7; 3; 2; 1; 7] = [1; 1; 2; 3; 6; 7; 7])