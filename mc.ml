module UF = Persistent_union_find


type var = int
type rel = Eq | Neq
type atom = rel * var * var  (** A relation between two variables *)
type formula = atom list     (** Represents a conjunction *)

type model = {
  nb_var : int;              (** Variables in the formula range from 0 to nb_vars - 1 *)
  f      : formula;
}


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
