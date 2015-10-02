type 'a t

val make : node_capacity:int -> 'a t

val length : 'a t -> int

val to_array : 'a t -> 'a array

val iter : ('a -> unit)  -> 'a t -> unit

val get : 'a t -> int -> 'a

val set : 'a t -> int -> 'a -> unit

val add : 'a t ->  'a -> unit

val insert : 'a t -> int -> 'a -> unit

val remove_at : 'a t -> int -> unit
