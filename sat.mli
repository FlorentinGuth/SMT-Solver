(** The types of the CNF formula used by the SAT solver *)

type var     = int          (** A boolean variable *)
type neg     = Neg | NoNeg  (** Whether the variable is negated *)
type atom    = neg * var
type clause  = atom   list  (** Represents a disjunction *)
type formula = clause list  (** Represents a conjunction *)
type cnf = {
  nb_var : int;             (** Variables in the formula range from 0 to nb_vars - 1 *)
  nb_cl  : int;             (** The number of clauses in the formula *)
  f      : formula;
}

type model = bool array     (** An assignment of all variables *)
type pseudo_model = (bool option) array

val print_var     : Format.formatter -> var     -> unit
val print_neg     : Format.formatter -> neg     -> unit
val print_atom    : Format.formatter -> atom    -> unit
val print_clause  : Format.formatter -> clause  -> unit
val print_formula : Format.formatter -> formula -> unit
val print_cnf     : Format.formatter -> cnf     -> unit

val print_model   : Format.formatter -> model   -> unit

val test_model    : cnf -> model -> bool

(** This function solves the given CNF, returning a model if satisfiable or None otherwise *)
val solve : cnf -> (pseudo_model -> formula) -> model option
