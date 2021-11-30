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

let integers =
  let rec init = lazy {
    prev = mk_prev init 0;
    elem = 0;
    next = mk_next init 0;
  } 
  and mk_next prev_node prev_x =
    let rec next_node = lazy {
      prev = prev_node;
      elem = prev_x + 1;
      next = mk_next next_node (prev_x + 1)
    } 
    in next_node
  and mk_prev next_node next_x =
    let rec prev_node = lazy {
      prev = mk_prev prev_node (next_x - 1);
      elem = (next_x - 1);
      next = next_node
    } 
    in prev_node
  in init