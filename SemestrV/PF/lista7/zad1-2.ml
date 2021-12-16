module type RandomMonad = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val random : int t
end

module RS : sig include RandomMonad val run : int -> 'a t -> 'a end = struct
  type 'a t = int -> 'a * int

  let return : 'a -> 'a t = fun a seed -> (a, seed)

  let bind : 'a t -> ('a -> 'b t) -> 'b t = 
    fun computation continuation seed ->
      let (a, seed) = computation seed in
      continuation a seed

  let good_mod x y = ((x mod y) + y) mod y;;

  let next seed = 
    let b_i a_i = 16807 * (good_mod a_i 127773) - 2836 * (a_i / 127773) in
    let next = b_i seed in
      if next > 0 then next
      else next + 2147483647

  let random : int t = fun seed -> (seed, next seed)

  let run : int -> 'a t -> 'a = 
    fun seed computation -> fst (computation (seed |> next |> next))
end

module Shuffle(R : RandomMonad) : sig
  val shuffle : 'a list -> 'a list R.t
end = struct
  let shuffle : 'a list -> 'a list R.t = fun xs ->
    let (let*) = R.bind in
    let rec prioritize xs =
      match xs with
      | [] -> R.return []
      | x::xs -> 
        let* p = prioritize xs in
        let* rand = R.random in
        R.return ((x, rand)::p) 
    in
    let* prioritized = prioritize xs in
    let cmp a b = Int.compare (snd a) (snd b) in
    let sorted = List.sort cmp prioritized in
    R.return (List.map fst sorted)
end

include Shuffle(RS)