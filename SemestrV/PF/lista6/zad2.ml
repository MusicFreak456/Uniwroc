type (_,_)format =
| Lit  : string  -> ('a, 'a) format
| Int  : (int    -> 'a, 'a) format
| Str  : (string -> 'a, 'a) format
| Cat  : ('a, 'b) format * ('b, 'c) format -> ('a, 'c) format
type 'a continuation = (string -> 'a)

let (^^) l r = Cat(l,r)

let ksprintf form cont =
  let rec ksprintf_match 
   : type a b. (a, b) format -> b continuation -> a continuation = 
   fun form cont str_beg ->
   begin match form with
   | Lit str     -> cont(str_beg ^ str)
   | Int         -> fun n   -> cont(str_beg ^ string_of_int n)
   | Str         -> fun str -> cont(str_beg ^ str)
   | Cat(f1,f2)  -> (ksprintf_match f1 (ksprintf_match f2 cont)) str_beg
   end
  in ksprintf_match form cont ""

let kprintf form cont =
  let rec kprintf_match
   : type a b. (a, b) format -> (unit -> b) -> a =
   fun form cont ->
    begin match form with
    | Lit str    -> cont(print_string str)
    | Int        -> fun n   -> cont(print_int n)
    | Str        -> fun str -> cont(print_string str) ; 
    | Cat(f1,f2) -> kprintf_match f1 (fun _ -> (kprintf_match f2 cont))
    end
  in kprintf_match form (fun _ -> cont)

let sprintf form = ksprintf form (fun x -> x) 

let printf form = kprintf form ()
