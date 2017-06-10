open Format
open Printer

module SAT = Sat
module MC  = Mc
module IMC = Incremental_mc


type var     = IMC.var
type app     = IMC.app
type literal = IMC.literal
type eq      = IMC.eq

type rel     = MC.rel
type atom    = rel * eq
type clause  = atom   list     (* Represents a disjunction *)
type formula = clause list     (* Represents a conjunction *)

type cnf = {
  nb_var   : int;              (* Variables in the formula range from 0 to nb_vars - 1 *)
  nb_real_var: int;
  (* nb_cl    : int;              (\* The number of clauses in the formula *\) *)
  f        : formula;
  var_of_app: (app,var) Hashtbl.t;
  app_of_var: app option array;
}


let print_cnf fmt cnf =
  let print_var fmt v =
    fprintf fmt "%d" (v+1)
  in
  let print_literal fmt lit =
    match lit with
    | IMC.Var v -> print_var fmt v
    | IMC.App (f,x) -> fprintf fmt "%a(%a)" print_var f print_var x
  in
  let print_rel fmt rel =
    fprintf fmt "%s" (match rel with MC.Eq -> eq_str | MC.Neq -> neq_str)
  in
  let print_atom fmt (rel,(l,v)) =
    fprintf fmt "%a%a%a" print_literal l print_rel rel print_var v
  in
  let print_clause fmt cl =
    print_list print_atom or_str fmt cl
  in
  let print_formula fmt f =
    print_list print_clause (" " ^ and_str ^ " ") fmt f
  in
  print_formula fmt cnf.f


(** Conversion to SAT formula:
    - Each SAT variable corresponds to an equality between a pair of variables
    - An inequality is then the negation of a variable
    - We detect that i=j is the same as j=i,
    - We replace i=i by true and i!=i by false,
*)

(** The type of the structure memorizing the conversion from SMT atoms to SAT atoms:
    conv.(max i j).(min i j) is the SAT variable associated to i=j.
    Besides, conv.(i).(i) is the SAT variable associated to f(x)=i where i is var_of_app(f,x).
*)
type smt_sat_conv = {
  array: SAT.var option array array;
  mutable free_var: SAT.var;         (* The variables from 0 to free_var - 1 are used *)
}

let create_conv nb_var =
  { array = Array.init nb_var (fun i -> Array.make (i+1) None); free_var = 0 }

type sat_atom =
  | ATrue
  | AFalse
  | Atom of SAT.atom
let atom_to_sat conv var_of_app ((rel,(l,v2)) : atom) =
  let (v1, eliminate_i_i) = match l with
    | IMC.Var v1 -> (v1, true)
    | IMC.App a  -> (Hashtbl.find var_of_app a, false)
  in
  if v1 = v2 && eliminate_i_i then
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
let clause_to_sat conv var_of_app (cl : clause) =
  let rec aux cl acc =
    match cl with
    | [] ->
      if acc = [] then
        CFalse (* Only false atoms or empty clause *)
      else
        Clause acc

    | a :: tl -> begin
        match atom_to_sat conv var_of_app a with
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
let formula_to_sat conv var_of_app (f : formula) =
  let rec aux f acc =
    match f with
    | [] ->
      if acc = [] then
        FTrue
      else
        Formula acc

    | cl :: tl -> begin
        match clause_to_sat conv var_of_app cl with
        | CTrue -> aux tl acc  (* Ignore the always-true clause *)
        | CFalse -> FFalse
        | Clause cl -> aux tl (cl :: acc)
      end
  in
  aux f []

let cnf_to_sat cnf =
  let conv = create_conv cnf.nb_var in
  match formula_to_sat conv cnf.var_of_app cnf.f with
  | Formula f -> (SAT.{ nb_var = conv.free_var; nb_cl = List.length f; f }, conv)
  (* TODO: Deal with the particular cases without calling the SAT solver, we can answer right away *)
  | FTrue ->  (SAT.{ nb_var = 0; nb_cl = 0; f = [  ] }, create_conv cnf.nb_var)
  | FFalse -> (SAT.{ nb_var = 0; nb_cl = 1; f = [[]] }, create_conv cnf.nb_var)



let of_some = function
  | None -> assert false
  | Some x -> x

(** Converts a SAT pseudo_model to an equality model for the Model Checker *)
let model_to_mc (m : SAT.pseudo_model) conv cnf =
  (* First, invert the conv array: maps a SAT var to a pair (i,j) (atom i=j) *)
  let nb_var_sat = conv.free_var in
  let nb_var_mc  = Array.length conv.array in
  let inv_array = Array.make nb_var_sat (IMC.Var 0, 0) in
  for i = 0 to nb_var_mc - 1 do
    for
j = 0 to i - 1 do
      match conv.array.(i).(j) with
      | None -> ()
      | Some v -> inv_array.(v) <- (IMC.Var i, j)
    done;
    match conv.array.(i).(i) with
    | None -> ()
    | Some v -> inv_array.(v) <- (IMC.App (of_some cnf.app_of_var.(i)), i)
  done;

  let eqs = ref [] in
  let neqs = ref [] in
  for v = 0 to nb_var_sat - 1 do
    match (m.(v), inv_array.(v)) with
    | (Some true,  (i, j)) ->  eqs := (i, j) :: ! eqs
    | (Some false, (i, j)) -> neqs := (i, j) :: !neqs
    | (None, _) -> () (* Ignoring unaffected variables *)
  done;
  IMC.{ nb_var = nb_var_mc; nb_real_var = cnf.nb_real_var;
        eqs = !eqs; neqs = !neqs;
        var_of_app = cnf.var_of_app }



(** Add the negation of the model to the formula *)

let neg_rel = function
  | MC. Eq -> MC.Neq
  | MC.Neq -> MC. Eq

let neg_atom (a : atom) =
  let (rel, eq) = a in
  ((neg_rel rel, eq) : atom)


let satisfiable cnf =
  (* print_stdout "CNF: %a\n" print_cnf cnf; *)
  let (sat_cnf, conv) = cnf_to_sat cnf in
  let check pseudo_model =
    (* print_stdout "SAT solver found model %a\n" SAT.print_pseudo_model pseudo_model; *)
    (* We keep the old cnf and conv since we did not introduce any new variables *)
    let m_mc = model_to_mc pseudo_model conv cnf in
    (* print_stdout "Calling Model Checker on %a\n" IMC.print_model m_mc; *)
    match IMC.check m_mc with
    | None ->
      (* print_stdout "Model Checker validated the model\n"; *)
      []

    | Some l ->
      (* print_stdout "Model Checker invalidated the model, adding negation\n"; *)
      let neg_expl = List.map neg_atom l in
      let sat_clause = match clause_to_sat conv cnf.var_of_app neg_expl with
        | CTrue | CFalse -> assert false
        | Clause cl -> cl in
      (* print_stdout "Adding SAT clause %a\n" SAT.print_clause sat_clause; *)
      sat_clause
  in
  (* print_stdout "Calling SAT solver on %a\n" SAT.print_cnf sat_cnf; *)
  match SAT.solve sat_cnf check with
  | None ->
    (* print_stdout "Unsatisfiable SAT formula\n"; *)
    false
  | Some m ->
    (* print_stdout "SAT found model %a\n" SAT.print_model m; *)
    assert (SAT.test_model sat_cnf m);
    true
