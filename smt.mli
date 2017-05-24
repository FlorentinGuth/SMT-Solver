module SAT : module type of Sat
module MC  : module type of Mc


(** The types of the formula used by the SMT solver *)

type var = int
type rel = MC.rel
type atom = rel * var * var
type clause  = atom   list  (* Represents a disjunction *)
type formula = clause list  (* Represents a conjunction *)

type cnf = {
  nb_var : int;             (* Variables in the formula range from 0 to nb_vars - 1 *)
  nb_cl  : int;             (* The number of clauses in the formula *)
  f      : formula;
}


val print_rel     : Format.formatter -> rel     -> unit
val print_atom    : Format.formatter -> atom    -> unit
val print_clause  : Format.formatter -> clause  -> unit
val print_formula : Format.formatter -> formula -> unit
val print_cnf     : Format.formatter -> cnf     -> unit


(** Returns true if the CNF formula is satisfiable, otherwise false *)
val satisfiable : cnf -> bool
