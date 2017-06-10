(** The type of a proof-union-find structure.
    This structure is meant to be used in parallel to a explain-union-find structure.
    It allows for storing the proof elements that have already been outputted.
*)
type t


val create : int -> t
val reset: t -> unit

val find : t -> int -> int
(* Highest node in the parallel proof forest, but when restricting to the class of the argument *)
val highest_node : t -> int -> int

val union : t -> int -> int -> unit
