(** The types of the formula used by the Model Checker *)

type var = int
type rel = Eq | Neq
type atom = rel * var * var  (** A relation between two variables *)
type formula = atom list     (** Represents a conjunction *)

type model = {
  nb_var : int;              (** Variables in the formula range from 0 to nb_vars - 1 *)
  f      : formula;
}


val print_rel     : Format.formatter -> rel     -> unit
val print_atom    : Format.formatter -> atom    -> unit
val print_formula : Format.formatter -> formula -> unit
val print_model   : Format.formatter -> model   -> unit


(** Returns true if the model is satisfiable, otherwise false *)
val check : model -> bool
