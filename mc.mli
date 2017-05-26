(** The types of the formula used by the Model Checker *)

type var     = int
type func    = string
type lit     = int              (* The ident of a literal *)

type literal =
  | Var  of var
  | Func of func * (lit list)

type rel     = Eq | Neq
type clause  = rel * lit * lit
type formula = clause list      (* Represents a conjunction *)

type model = {
  (* nb_var   : int;               (\* Variables in the formula range from 0 to nb_vars - 1 *\) *)
  (* nb_func  : int;               (\* Same as above for functions *\) *)
  nb_lit   : int;               (* Same as above for literals *)
  (* nb_cl    : int;               (\* The number of clauses in the formula *\) *)
  f        : formula;
  literals : literal array;     (* Maps a literal ident to the literal *)
}


val print_model : Format.formatter -> model -> unit


(** Returns true if the model is satisfiable, otherwise false *)
val check : model -> bool
