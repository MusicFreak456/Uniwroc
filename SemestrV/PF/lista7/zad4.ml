module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Err : sig
  include Monad
  val fail : 'a t
  val run : 'a t -> 'a option
  val catch : 'a t -> (unit -> 'a t) -> 'a t
end = struct
  type 'a ans = 'a option
  type 'a t = { run : 'r. ('a -> 'r ans) -> 'r ans }

  let return a = { run = fun cont -> cont a }  
  let fail = { run = fun cont -> None }
  let run m = m.run (fun x -> Some x)

  let bind m f =
    { run = fun cont -> m.run (fun x -> (f x).run cont) }

  let catch : 'a t -> (unit -> 'a t) -> 'a t =
    fun m f ->
      match run m with
      | None   -> f ()
      | Some x -> return x
end

module BT : sig
  include Monad
  val run : 'a t -> 'a Seq.t
  val fail : 'a t
  val flip : bool t
end = struct
  type 'a ans = 'a Seq.t
  type 'a t = { run : 'r. ('a -> 'r ans) -> 'r ans }

  let return : 'a -> 'a t = fun a -> { run = fun cont -> cont a }
  let fail = { run = fun cont -> Seq.empty }

  let run c = c.run (fun x -> Seq.return x)

  let flip  = {run = fun cont -> Seq.append (cont true) (cont false)}

  let bind m f =
    { run = fun cont -> m.run (fun x -> (f x).run cont) }
end

module St(State : sig type t end) : sig
  include Monad
  val get : State.t t
  val set : State.t -> unit t
  (* val run : State.t -> 'a t -> 'a *)
end = struct 
  type 'a ans = State.t -> 'a * State.t
  type 'a t = { run : 'r. ('a -> 'r ans) -> 'r ans }

  
  let return : 'a -> 'a t = fun a -> { run = fun cont -> cont a }
  let bind m f =
    { run = fun cont -> m.run (fun x -> (f x).run cont) }
    
  let get : State.t t = { run = (fun cont s -> cont s s) }
  let set : State.t -> unit t = fun s -> { run = fun cont _ -> cont () s }

  let run : State.t -> 'a t -> 'a = 
    fun s m -> m.run (fun x s' -> (x,s')) s |> fst

end