type 'a t

val make : int -> 'a t

val length : 'a t -> int

val iter : ('a -> unit)  -> 'a t -> unit

val nth : 'a t -> int -> 'a

val add : 'a t ->  'a -> unit

val insert : 'a t -> int -> 'a -> unit

val remove_at : 'a t -> int -> unit
