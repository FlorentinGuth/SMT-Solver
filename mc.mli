(** The types of the formula used by the Model Checker *)

type var = int
type rel = Eq | Neq
type atom = rel * var * var  (** A relation between two variables *)
type formula = atom list     (** Represents a conjunction *)

type model = {
  nb_var : int;              (** Variables in the formula range from 0 to nb_vars - 1 *)
  f      : formula;
}


(** Returns true if the model is satisfiable, otherwise false *)
val check : model -> bool
