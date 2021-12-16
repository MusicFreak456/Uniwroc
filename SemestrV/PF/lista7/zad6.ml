module SBT(State : sig type t end) : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fail : 'a t
  val flip : bool t
  val get : State.t t
  val put : State.t -> unit t
  val run : State.t -> 'a t -> 'a Seq.t
end = struct
  type 'a t = State.t -> ('a * State.t) Seq.t

  let return a s = Seq.return (a,s)
  let bind : 'a t -> ('a -> 'b t) -> 'b t = 
    fun m f s -> Seq.flat_map (fun (a,s) -> f a s) (m s)

  let fail s = Seq.empty

  let flip s = List.to_seq [(true, s); (false,s)]

  let get s = return s s 

  let put s _ = return () s

  let run s m = Seq.map fst (m s)
end