open Format
open Printer

module UF = Persistent_union_find


type var = int
type rel = Eq | Neq
type atom = rel * var * var  (** A relation between two variables *)
type formula = atom list     (** Represents a conjunction *)

type model = {
  nb_var : int;              (** Variables in the formula range from 0 to nb_vars - 1 *)
  f      : formula;
}


let print_rel fmt rel =
  fprintf fmt "%s" (match rel with Eq -> "=" | Neq -> "!=")
let print_atom fmt (rel,v1,v2) =
  fprintf fmt "%d%a%d" v1 print_rel rel v2
let print_formula fmt f =
  print_list print_atom "/\\" fmt f
let print_model fmt m =
  print_formula fmt m.f


(** We check a model the following way:
    - first, we use a union find structure in which every variable is alone,
    - then, we consider all equalities and merge the adequate classes,
    - last, if there is an inequality between two variables of the same class,
      then the model is not satisfiable, otherwise it is *)

let rec check_aux t f neqs =
  match f with
  | [] ->
    List.for_all (fun (i, j) -> UF.find t i <> UF.find t j) neqs

  | (Eq, i, j) :: tl ->
    check_aux (UF.union t i j) tl neqs

  | (Neq, i, j) :: tl ->
    check_aux t tl ((i, j) :: neqs)

let check model =
  check_aux (UF.create model.nb_var) model.f []
