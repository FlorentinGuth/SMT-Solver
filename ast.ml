type var = int
type rel = Eq | Neq
type atom = rel * var * var
type clause  = atom   list  (* Represents a disjunction *)
type formula = clause list  (* Represents a conjunction *)

type cnf = {
  nb_var : int;  (* Variables in the formula range from 0 to nb_vars - 1 *)
  nb_cl  : int;
  f      : formula;
}
