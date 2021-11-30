let (next, reset) = 
  let counter = ref 0 in
  let next () =
    incr counter;
    !counter
  and reset () =
    counter := 0
  in (next, reset)