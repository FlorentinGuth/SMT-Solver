(** The type of the internal state of the Model Checker *)
type t

(** The types of the formula used by the Model Checker *)
type var     = int
(** A literal can only be of two types:
    - either a variable (which can actually be a function!)
    - the application of a unique function (@) to two variables
    Moreover, a clause is either a=b, a<>b or f(a,b)=c
    For instance, g(a,h(b),b)=b is replaced by f(f(f(g,a),f(h,b)),b)=b (currifying)
    and then by f(g,a)=c /\ f(h,b)=d /\ f(c,d)=e /\ f(e,b)=b (flattening)
*)
type literal =
  | Var of var
  | App of var * var


(** Creates a new incremental Model Checker structure.
    Its argument is the number of variables.
*)
val create : int -> t

(** Performs the equality a=b or f(a1,a2)=b on the structure *)
val merge : t -> literal -> var -> unit

val are_congruent : t -> var -> var -> bool

val explain : t -> var -> var -> (literal * var) list
