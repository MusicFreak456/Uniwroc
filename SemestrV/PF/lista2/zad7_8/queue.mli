type 'a t = private 
| Leaf 
| Node of 'a t * 'a * int * 'a t

val empty_queue : 'a t
val merge : 'a t -> 'a t -> 'a t
val insert : 'a -> 'a t -> 'a t
val delete_min : 'a t -> ('a * 'a t) option