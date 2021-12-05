open PpoProc

let rec echo k = 
  recv (fun received ->
  send received (fun () ->
  echo k))

let rec map f k = 
  recv (fun received ->
  send (f received) (fun () ->
  map f k))

let rec filter p k =
  recv (fun received -> 
  if p received then send received (fun () -> filter p k)
  else filter p k)

let rec nats_from n k=
  send n (fun () ->
  nats_from (n + 1) k)

let rec sieve k =
  recv (fun received ->
  send received (fun () ->
  (filter (fun x -> x mod received <> 0) >|> sieve) k
  ))