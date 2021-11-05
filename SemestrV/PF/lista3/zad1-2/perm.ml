module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type t
  (** permutacja jako funkcja *)
  val apply : t -> key -> key
  (** permutacja identycznościowa *)
  val id : t
  (** permutacja odwrotna *)
  val invert : t -> t
  (** permutacja która tylko zamienia dwa elementy miejscami *)
  val swap : key -> key -> t
  (** złożenie permutacji (jako złożenie funkcji) *)
  val compose : t -> t -> t
  (** porównywanie permutacji *)
  val compare : t -> t -> int
end


module Make(Key : OrderedType) = struct

  module M = Map.Make(Key) 
  type key = Key.t
  type mapping = Key.t M.t
  type t = mapping * mapping

  let id = (M.empty, M.empty)

  let apply perm k = 
    let (p,i) = perm in 
      let value = M.find_opt k p in
        match value with
        | None -> k
        | Some(x) -> x
  
  let invert perm = 
    let (p,i) = perm in (i,p)

  let swap a b  =
    if Key.compare a b == 0 
      then (M.empty, M.empty)
      else 
        let f = M.add b a (M.add a b M.empty) 
        in (f,f)

  let compose perm1 perm2 = 
    let perm_bind p1 p2 k a b =
      match a,b with
      | _,None -> M.find_opt k p1
      | _,Some(a) -> 
        let search_res = M.find_opt a p1 in
        begin match search_res with
        | None -> Some(a)
        | Some(b) when Key.compare b k = 0 -> None
        | Some(b) -> Some(b)
        end
    and (p1,i1) = perm1 
    and (p2,i2) = perm2 in
      ( M.merge (perm_bind p1 p2) p1 p2,
        M.merge (perm_bind i2 i1) i2 i1 )

  let compare perm1 perm2 =
    let (p1,_) = perm1
    and (p2,_) = perm2 in
    M.compare Key.compare p1 p2
end