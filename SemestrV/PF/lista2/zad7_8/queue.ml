type 'a t =
  | Leaf
  | Node of 'a t * 'a * int * 'a t

let empty_queue = Leaf

let smart_constructor label lt rt = 
  match lt,rt with
  | Leaf, Leaf -> Node(Leaf, label, 1, Leaf)
  | Leaf, (Node _ as tree) | (Node _ as tree), Leaf -> Node(tree, label, 1, Leaf)
  | (Node (_,_,lh,_) as left),(Node (_,_,rh,_) as right)-> 
      if rh <= lh 
        then Node(left , label, rh + 1, right)
        else Node(right, label, lh + 1, left)

let rec merge lt rt = 
  match lt,rt with
  | Leaf, Leaf -> Leaf
  | Leaf, (Node _ as tree) | (Node _ as tree), Leaf -> tree
  | (Node (ll,la,lh,lr) as left),(Node (rl,ra,rh,rr) as right) ->
      if la < ra 
        then smart_constructor la ll (merge lr right)
        else smart_constructor ra rl (merge rr left)

let insert l t = merge t (smart_constructor l Leaf Leaf)

let delete_min t = 
  match t with
  | Leaf -> None
  | Node(l,a,_,r) -> Some(a, merge l r) 

(* let sort xs = 
  let rec insert_xs xs = 
    match xs with
    | [] -> empty_queue
    | x::xs -> insert x (insert_xs xs)
  and construct_result q =
    let x = delete_min q in
      match x with
      | None -> []
      | Some(y,t) -> y :: construct_result t
  in construct_result (insert_xs xs) *)