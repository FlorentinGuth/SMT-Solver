open Format
open Printer


type var     = int          (** A boolean variable *)
type clause  = var    list  (** Represents a disjunction *)
type formula = clause list  (** Represents a conjunction *)
type cnf = {
  nb_var : int;             (** Variables in the formula range from 0 to nb_vars - 1 *)
  nb_cl  : int;             (** The number of clauses in the formula *)
  f      : formula;
}

type model = bool array     (** An assignment of all variables *)


let print_var fmt var =
  fprintf fmt "%d" var
let print_clause fmt clause =
  print_list print_var "\\/" fmt clause
let print_formula fmt f =
  print_list print_clause " /\\ " fmt f
let print_cnf fmt cnf =
  print_formula fmt cnf.f

let print_model fmt m =
  print_list (fun fmt b -> fprintf fmt "%s" (if b then "T" else "F")) ";" fmt (Array.to_list m)


(** Dummy solver: we test all possible assignments *)

(** Returns the list of all possible assignments, under a list form *)
let rec generate_models n =
  if n = 0 then
    [[]]
  else
    List.flatten (List.map (fun a -> [true :: a; false :: a]) (generate_models (n-1)))

(** Test an assignment on the CNF *)
let test_model cnf (m : model) =
  List.fold_left (fun b cl -> b && (List.fold_left (fun b v -> b || m.(v)) false cl)) true cnf.f

(** Try all models *)
let rec try_all cnf = function
  | [] -> None
  | m :: ms -> if test_model cnf m then Some m else try_all cnf ms

let solve cnf =
  try_all cnf (List.map Array.of_list (generate_models cnf.nb_var))
