module SAT = Sat
module MC  = Mc
module IMC = Incremental_mc


(** The types of the formula used by the SMT solver *)

type var     = IMC.var
type app     = IMC.app
type literal = IMC.literal
type eq      = IMC.eq

type rel     = MC.rel
type atom    = rel * eq
type clause  = atom   list     (* Represents a disjunction *)
type formula = clause list     (* Represents a conjunction *)

type cnf = {
  nb_var      : int;                     (* Variables in the formula range from 0 to nb_vars - 1 *)
  nb_real_var : int;                     (* Number of real variables, for outputting solution *)
  (* nb_cl       : int;                     (\* The number of clauses in the formula *\) *)
  f           : formula;
  var_of_app  : (app,var) Hashtbl.t;     (* Maps (f,x) to the variable f(x) *)
  app_of_var  : app option array;
}


val print_cnf : Format.formatter -> cnf -> unit


(** Returns true if the CNF formula is satisfiable, otherwise false *)
val satisfiable : cnf -> bool
