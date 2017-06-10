type t

val create : int -> t

val union : t -> int -> int -> unit
val find : t -> int -> int

(** Returns the parent in the proof forest *)
val proof_parent : t -> int -> int
(** Akin to find, but in the proof forest *)
val highest_node : t -> int -> int
(** Nearest common ancestor in the proof forest, might raise Not_found *)
val nearest_common_ancestor : t -> int -> int -> int

(** explain t i j returns a minimal subset of union operations that entails i~j.
    The order is not preserved, i.e. the variables may not be in the same order
    as the one union was called with.
    This function raise Not_found if i!~j in t.
*)
val explain : t -> int -> int -> (int * int) list


val print_classes : Format.formatter -> t -> unit
