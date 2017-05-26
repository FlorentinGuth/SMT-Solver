module SAT = Sat
module MC  = Mc


(** The types of the formula used by the SMT solver *)

type var     = MC.var
type func    = MC.func
type lit     = MC.lit
type literal = MC.literal

type rel     = MC.rel
type atom    = MC.clause
type clause  = atom   list     (* Represents a disjunction *)
type formula = clause list     (* Represents a conjunction *)

type cnf = {
  (* nb_var   : int;              (\* Variables in the formula range from 0 to nb_vars - 1 *\) *)
  (* nb_func  : int;              (\* Same as above for functions *\) *)
  nb_lit   : int;              (* Same as above for literals *)
  (* nb_cl    : int;              (\* The number of clauses in the formula *\) *)
  f        : formula;
  literals : literal array;    (* Maps a literal ident to the literal *)
}


val print_cnf : Format.formatter -> cnf -> unit


(** Returns true if the CNF formula is satisfiable, otherwise false *)
val satisfiable : cnf -> bool
