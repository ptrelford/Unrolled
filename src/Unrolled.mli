(** Unrolled linked list implementation. *)

type 'a t

(** Makes a new collection with the specified node capacity *)
val make : node_capacity:int -> 'a t

(** Returns the length of the collection *)
val length : 'a t -> int

(** Returns a copy of the collection as an array *)
val to_array : 'a t -> 'a array

(** Iterates each item in the collection applying the given function *)
val iter : ('a -> unit)  -> 'a t -> unit

(** Returns the index of the first item that satisfies the specified predicate *)
val findi : ('a -> bool) -> 'a t -> int

(** Returns the item at the specified index *)
val get : 'a t -> int -> 'a

(** Sets the item at the specified index  *)
val set : 'a t -> int -> 'a -> unit

(** Adds an item to the end of the collection *)
val add : 'a t ->  'a -> unit

(** Inserts an item at the specified index in the collection *)
val insert : 'a t -> int -> 'a -> unit

(** Deletes the item at the specified index from the collection *)
val delete : 'a t -> int -> unit
