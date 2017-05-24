(** This file provides a persistent array data structure *)


(** The type of a persistent array containing objects of type 'a *)
type 'a t


(** init n f returns an array of size n where rank i maps to f i *)
val init : int -> (int -> 'a) -> 'a t

(** get a i returns the i-th element of a *)
val get : 'a t -> int -> 'a
(** set a i x returns a new array where i maps to x *)
val set : 'a t -> int -> 'a -> 'a t
