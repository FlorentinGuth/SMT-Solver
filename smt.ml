open Format
open Printer

module SAT = Sat
module MC  = Mc


type var     = int
type rel     = MC.rel
type atom    = rel * var * var
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


(** Conversion to SAT formula:
    - Each SAT variable corresponds to an equality between a pair of variables
    - An inequality is then the negation of a variable
    - We detect that i=j is the same as j=i,
    - We replace i=i by true and i!=i by false,
*)

(** The type of the structure memorizing the conversion from SMT atoms to SAT atoms:
    conv.(max i j).(min i j) is the SAT variable associated to i=j *)
type smt_sat_conv = {
  array: SAT.var option array array;
  mutable free_var: SAT.var;         (* The variables from 0 to free_var - 1 are used *)
}

let create_conv nb_var =
  { array = Array.init nb_var (fun i -> Array.make i None); free_var = 0 }

type sat_atom =
  | ATrue
  | AFalse
  | Atom of SAT.atom
let atom_to_sat conv ((rel, v1, v2) : atom) =
  if v1 = v2 then
    match rel with
    | MC.Eq -> ATrue
    | MC.Neq -> AFalse
  else
    let (vmin, vmax) = (min v1 v2, max v1 v2) in
    let sat_var = match conv.array.(vmax).(vmin) with
      | Some v -> v
      | None -> let v = conv.free_var in
        conv.free_var <- v + 1;
        conv.array.(vmax).(vmin) <- Some v;
        v
    in
    Atom ((match rel with MC.Eq -> SAT.NoNeg | MC.Neq -> SAT.Neg), sat_var)

type sat_clause =
  | CTrue
  | CFalse
  | Clause of SAT.clause
let clause_to_sat conv (cl : clause) =
  let rec aux cl acc =
    match cl with
    | [] ->
      if acc = [] then
        CFalse (* Only false atoms or empty clause *)
      else
        Clause acc

    | a :: tl -> begin
        match atom_to_sat conv a with
        | ATrue  -> CTrue             (* The clause will be always true *)
        | AFalse -> aux tl acc        (* We ignore the atom *)
        | Atom a -> aux tl (a :: acc)
      end
  in
  aux cl []

type sat_formula =
  | FTrue
  | FFalse
  | Formula of SAT.formula
let formula_to_sat conv (f : formula) =
  let rec aux f acc =
    match f with
    | [] ->
      if acc = [] then
        FTrue
      else
        Formula acc

    | cl :: tl -> begin
        match clause_to_sat conv cl with
        | CTrue -> aux tl acc  (* Ignore the always-true clause *)
        | CFalse -> FFalse
        | Clause cl -> aux tl (cl :: acc)
      end
  in
  aux f []

let cnf_to_sat cnf =
  let conv = create_conv cnf.nb_var in
  match formula_to_sat conv cnf.f with
  | Formula f -> (SAT.{ nb_var = conv.free_var; nb_cl = List.length f; f }, conv)
  (* TODO: Deal with the particular cases without calling the SAT solver, we can answer right away *)
  | FTrue ->  (SAT.{ nb_var = 0; nb_cl = 0; f = [  ] }, create_conv cnf.nb_var)
  | FFalse -> (SAT.{ nb_var = 0; nb_cl = 1; f = [[]] }, create_conv cnf.nb_var)



(** Converts a SAT model to an equality model for the Model Checker *)
let model_to_mc (m : SAT.model) conv =
  (* First, invert the conv array: maps a SAT var to a pair (i,j) (atom i=j) *)
  let nb_var = conv.free_var in
  let inv_array = Array.make nb_var (0, 0) in
  for i = 0 to Array.length conv.array - 1 do
    for j = 0 to i - 1 do
      match conv.array.(i).(j) with
      | None -> ()
      | Some v -> inv_array.(v) <- (i, j)
    done;
  done;

  let f = ref ([] : MC.formula) in
  for v = 0 to nb_var - 1 do
    let a = match (m.(v), inv_array.(v)) with
      | (true,  (i, j)) -> (MC.Eq,  i, j)
      | (false, (i, j)) -> (MC.Neq, i, j)
    in
    f := a :: !f;
  done;
  MC.{ nb_var; f = !f }



(** Add the negation of the model to the formula *)

let neg_rel = function
  | MC. Eq -> MC.Neq
  | MC.Neq -> MC. Eq

let neg_atom (a : atom) =
  let (rel, v1, v2) = a in
  ((neg_rel rel, v1, v2) : atom)

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
