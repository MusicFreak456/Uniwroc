let is_generated (type t) (module P : Perm.S with type t = t) p g = 
  let module S = Set.Make(P) in 
  let gen_set = S.of_list g  in
  let x_0 = S.add P.id gen_set 
  and next prev = 
    let prev_inv = S.map (fun x -> P.invert x) prev
    and prev_com = 
      S.fold (fun q acc -> S.union (S.map (fun x -> P.compose q x) prev) acc) prev S.empty
    in S.union prev (S.union prev_inv prev_com)
  in let rec is_generated_aux prev_set next_set =
    if S.mem p next_set then true
    else if S.compare prev_set next_set = 0 then false
    else is_generated_aux next_set (next next_set)
  in if S.mem p x_0 then true else is_generated_aux x_0 (next x_0)

module P = Perm.Make (Int)

let _ = assert (is_generated (module P) P.id [P.id] = true)

let _ =
  assert (
    is_generated
      (module P)
      (P.compose (P.swap 1 2) (P.swap 1 3))
      [P.swap 1 2; P.swap 1 3]
    = true)

let _ =
  assert (
    is_generated (module P) (P.compose (P.swap 1 2) (P.swap 1 3)) [P.swap 1 2]
    = false)

let _ =
  assert (
    is_generated
      (module P)
      (P.compose (P.swap 1 2) (P.swap 1 3) |> P.invert)
      [P.swap 1 2; P.swap 1 3]
    = true)

let _ =
  assert (
    is_generated
      (module P)
      (P.compose (P.swap 1 2) (P.swap 1 3)
      |> P.invert
      |> P.compose (P.swap 2 5)
      |> P.compose (P.swap 5 2)
      |> P.invert)
      [P.swap 1 2; P.swap 1 3; P.swap 2 5]
    = true)

let _ =
  assert (is_generated (module P) (P.swap 5 6) [P.swap 1 2; P.swap 1 3] = false)

let _ =
  assert (
    is_generated
      (module P)
      (P.compose (P.swap 1 2) (P.swap 1 3)
      |> P.invert
      |> P.compose (P.swap 2 5)
      |> P.compose (P.swap 5 2)
      |> P.invert)
      [P.swap 1 2; P.swap 1 3]
    = true)