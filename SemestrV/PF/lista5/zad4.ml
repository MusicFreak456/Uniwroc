type 'a dllist = 'a dllist_data lazy_t
and 'a dllist_data = { 
  prev : 'a dllist; 
  elem : 'a; 
  next : 'a dllist
}

let prev dlist =
  let {prev; elem; next} = Lazy.force dlist 
  in prev

let next dlist =
  let {prev; elem; next} = Lazy.force dlist 
  in next

let elem dlist =
  let {prev; elem; next} = Lazy.force dlist in
  elem

let d = 
  let rec list = lazy { prev = list; elem = 1; next = list } in
  list

let of_list list =  
  begin match list with
  | [] -> failwith "dllist cannot be created from an empty list"
  | x::xs -> 
    let rec head = lazy {
      prev = mk_prev head (List.rev xs); 
      elem = x; 
      next = mk_next_node head xs
    }
    and length = List.length xs 
    and last_calc_next = ref head 
    and next_cnt       = ref 0
    and last_calc_prev = ref head
    and prev_cnt       = ref 0
    and mk_next_node prev xs =
      if !next_cnt + !prev_cnt = length then !last_calc_prev else
      begin match xs with 
      | [] -> assert false
      | x::xs ->
        let rec node = lazy {
          prev = prev; 
          elem = x; 
          next = mk_next_node node xs
        } in
        last_calc_next := node;
        incr next_cnt;
        node
      end
    and mk_prev next xs = 
      if !prev_cnt + !next_cnt = length then !last_calc_next else
      begin match xs with
      | [] -> assert false
      | x::xs ->
        let rec node = lazy {
          prev = mk_prev node xs;
          elem = x;
          next = next
        } in
        last_calc_prev := node;
        incr prev_cnt;
        node
      end
    in head
  end

(* let _ =
  for i = 0 to 400 do
    Unix.sleepf 0.01;
    i |> string_of_int |> print_endline;
    assert ((d |> prev |> next) == d)
  done *)