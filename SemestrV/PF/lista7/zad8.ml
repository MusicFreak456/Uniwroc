type symbol = string
type 'v term =
| Var of 'v
| Sym of symbol * 'v term list

let return : 'a -> 'a term = 
  fun a -> Var a

let rec bind : 'a term -> ('a -> 'b term) -> 'b term =
  fun t f ->
    match t with
    | Var a      -> f a
    | Sym(s, tx) -> Sym(s, List.map (fun t -> bind t f) tx)