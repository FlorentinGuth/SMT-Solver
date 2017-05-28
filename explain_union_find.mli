type t


val create : int -> t

val union : t -> int -> int -> unit
val find : t -> int -> int

(** explain t i j returns a minimal subset of union operations that entails i~j.
    The order is not preserved, i.e. the variables may not be in the same order
    as the one union was called with.
    This function raise Not_found if i!~j in t.
*)
val explain : t -> int -> int -> (int * int) list
