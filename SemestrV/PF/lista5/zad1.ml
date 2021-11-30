let rec fix f x = f (fix f) x

let fib_f fib n =
  if n <= 1 then n
  else fib (n-1) + fib (n-2)

let fib = fix fib_f

let fact_f fact n = 
  if n <= 1 then 1
  else n * fact (n - 1)

let fact = fix fact_f

let rec fix_with_limit n f x = 
  if n = 0 then failwith "Recursion depth exceeded"
  else f (fix_with_limit (n-1) f) x

let limited_fact = fix_with_limit 5 fact_f

let fix_memo f = 
  let memory = Hashtbl.create 100 in 
  let rec fix f x =
    begin match Hashtbl.find_opt memory x with 
    | Some(x) -> x
    | None -> 
      let res = f (fix f) x in
        Hashtbl.add memory x res;
        res
    end
  in fix f
      
let memo_fib = fix_memo fib_f

let _ =
  let start = Sys.time () in
  let _ = memo_fib 4096 in
  Printf.printf "fib memo 4096: %f\n" (Sys.time () -. start)

let _ =
  let start = Sys.time () in
  let _ = memo_fib 4096 in
  Printf.printf "fib memo 4096: %f\n" (Sys.time () -. start)