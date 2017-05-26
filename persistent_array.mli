(** This file provides a persistent array data structure *)


(** The type of a persistent array containing objects of type 'a *)
type 'a t


(** init n f returns an array of size n where rank i maps to f i *)
val init : int -> (int -> 'a) -> 'a t

(** get a i returns the i-th element of a *)
val get : 'a t -> int -> 'a
(** set a i x returns a new array where i maps to x *)
val set : 'a t -> int -> 'a -> 'a t

(** fold_left f a b folds the persistent array b as if it were a real array*)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** filter f a creates a list with all the elements of a that satisfy f*)
val filter : ('a -> bool) -> 'a t -> 'a list
(** Same as Array.map for persistent array, with real arrays in return*)
val map : ('a -> 'b) -> 'a t -> 'b array
