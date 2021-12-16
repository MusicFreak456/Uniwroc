module IdMonad : sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end = struct
  type 'a t = 'a

  let return x = x

  let bind x c = c x
end

module LazyMonad : sig
  type 'a t 

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val force : 'a t -> 'a
end = struct 
  type 'a t = unit -> 'a 

  let return x () = x

  let bind x c () = c (x ()) ()

  let force c = c ()
end

let (let*) = IdMonad.bind
let (let+) = LazyMonad.bind

let illegal = 
  let+ x = LazyMonad.return 10 in
  let+ y = LazyMonad.return 0  in
  LazyMonad.return (x / y) 