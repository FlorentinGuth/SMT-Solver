(** The type of the internal state of the Model Checker *)
type t

(** The types of the formula used by the Model Checker *)
type var = int
type app = var * var
(** A literal can only be of two types:
    - either a variable (which can actually be a function!)
    - the application of a unique function (the application, noted 'f' here) to two variables
    Moreover, a clause is either a=b, a<>b or f(a,b)=c
    For instance, g(a,h(b),b)=b is replaced by f(f(f(g,a),f(h,b)),b)=b (currifying)
    and then by f(g,a)=c /\ f(h,b)=d /\ f(c,d)=e /\ f(e,b)=b           (flattening)
*)
type literal =
  | Var of var
  | App of app

type eq = literal * var


(** Creates a new incremental Model Checker structure.
    Its argument is the number of variables.
*)
val create : int -> t

(** Performs the equality a=b or f(a1,a2)=b on the structure *)
val merge : t -> eq -> unit

val are_congruent : t -> var -> var -> bool

(** Returns a list of merges that entails the equality between the two variables.
    Raises Not_found if the two variables are not equal
*)
val explain : t -> var -> var -> eq list


(* For the ones not wanting to be incremental *)
type model = {
  nb_var : int;
  nb_real_var : int;
  eqs    : eq list;
  neqs   : eq list;
  var_of_app : (app,var) Hashtbl.t;
}

(** Checks a model.
    Returns None if the model is satisfiable, and an explanation otherwise.
*)
val check : model -> (Mc.rel * eq) list option
