open Format
open Printer


module SAT = Sat
module MC  = Mc


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


let print_rel fmt rel =
  fprintf fmt "%s" (match rel with
                     | MC.Eq -> "="
                     | MC.Neq -> "!=")
let print_atom fmt atom =
  let (rel, v1, v2) = atom in
  fprintf fmt "%d%a%d" v1 print_rel rel v2
let print_clause fmt clause =
  print_list print_atom "\\/" fmt clause
let print_formula fmt f =
  print_list print_clause " /\\ " fmt f
let print_cnf fmt cnf =
  print_formula fmt cnf.f


type conv_list  = (atom * SAT.var) list  (** Maps atoms to a SAT variable, which must range from 0 to ... *)
type conv_array = atom array             (** SAT variables are indexes *)
let conv_list_to_array (l : conv_list) nb_var =
  let a = Array.make nb_var (MC.Eq, 0, 0) in
  List.iter (fun (atom, v) -> a.(v) <- atom) l;
  (a : conv_array)


let rec clause_to_sat (cl : clause) (var : SAT.var) (acc : SAT.clause) (table : conv_list) =
  match cl with
  | [] -> (var, acc, table)
  | a :: tl -> clause_to_sat tl (var + 1) (var :: acc) ((a, var) :: table)

let rec formula_to_sat (f : formula) (var : SAT.var) (acc : SAT.formula) (table : conv_list) =
  match f with
  | [] -> (var, acc, conv_list_to_array table var)
  | cl :: tl -> let (var, cl_sat, table) = clause_to_sat cl var [] table in
    formula_to_sat tl var (cl_sat :: acc) table


(** Converts a formula to a boolean CNF for the SAT solver *)
let cnf_to_sat cnf =
  let (nb_var, f, conv) = formula_to_sat cnf.f 0 [] [] in
  (SAT.{ nb_var; nb_cl = cnf.nb_cl; f }, conv)


let neg_rel = function
  | MC. Eq -> MC.Neq
  | MC.Neq -> MC. Eq

let neg_atom (a : atom) =
  let (rel, v1, v2) = a in
  ((neg_rel rel, v1, v2) : atom)

(** Converts a SAT model to an equality model for the Model Checker *)
let model_to_mc (m : SAT.model) (conv : conv_array) =
  let nb_var = Array.length m in
  let f = ref ([] : MC.formula) in
  for i = 0 to nb_var - 1 do
    let a = if m.(i) then conv.(i) else neg_atom conv.(i) in
    f := a :: !f;
  done;
  MC.{ nb_var; f = !f }


(** Add the negation of the model to the formula *)
let add_model_neg cnf (m : MC.model) =
  { nb_var = cnf.nb_var;
    nb_cl = cnf.nb_cl + 1;
    f = (List.map neg_atom m.MC.f) :: cnf.f; }


let rec satisfiable cnf =
  print_stdout "CNF: %a\n" print_cnf cnf;
  let (sat_cnf, conv) = cnf_to_sat cnf in
  print_stdout "Calling SAT solver on %a\n" SAT.print_cnf sat_cnf;
  match SAT.solve sat_cnf with
  | None -> print_stdout "Unsatisfiable SAT formula\n"; false
  | Some m -> print_stdout "SAT found model %a\n" SAT.print_model m;
    let m_mc = model_to_mc m conv in
    print_stdout "Calling Model Checker on %a\n" MC.print_model m_mc;
    if MC.check m_mc then begin
      print_stdout "Model Checker validated the model\n";
      true
    end else begin
      print_stdout "Model Checker invalidated the model, adding negation\n";
      satisfiable (add_model_neg cnf m_mc)
    end
