type ('a, 'b) format = 'b continuation -> 'a continuation
and 'a continuation = (string -> 'a)

(* 
 * 'a -- typ który produkuje format np int -> 'a
 * 'b -- typ zwracany przez kontynuację przyjmowaną za argument 
 *)

let lit str1 = 
  fun cont str -> cont(str ^ str1)

let int cont =
  fun str x -> cont(str ^ string_of_int x) 

let str cont =
  fun str x -> cont(str ^ x)

let ksprintf form cont = form cont ""

let (^^) f1 f2 = fun cont -> f1 (f2 cont)

let sprintf form = ksprintf form (fun x -> x)

let _ = sprintf (int) 5 
let _ = sprintf (lit "test " ^^ int ^^ lit " test") 5
let _ = sprintf (str ^^ lit " test") "test"