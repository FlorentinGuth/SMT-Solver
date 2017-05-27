open Format
open Printer


type var     = int          (** A boolean variable *)
type neg     = Neg | NoNeg  (** Whether the variable is negated *)
type atom    = neg * var
type clause  = atom list    (** Represents a disjunction *)
type formula_sat = (bool * clause) list
(** Represents a conjunction.
 *  The bool stands for wether the clause is satisfied or not *)
type formula = clause list

type cnf = {
  nb_var : int;             (** Variables in the formula_sat range from 0 to nb_vars - 1 *)
  nb_cl  : int;             (** The number of clauses in the formula_sat *)
  f      : formula;
}
type literal = {
  var    : var;
  neg    : neg;
  defined: bool;
  guessed: bool;
}

module PA = Persistent_array

(** A guess model a persistent array of assignments of the variables, 
 *  0 meaning assigned to false, 1, assigned to true and 2, unassigned *)
type guess_model = literal PA.t

type model = bool array     (** An assignment of all variables *)


let print_var fmt var =
  fprintf fmt "%d" var
let print_neg fmt neg =
  fprintf fmt "%s" (match neg with Neg -> "!" | NoNeg -> "")
let print_atom fmt (n,v) =
  fprintf fmt "%a%a" print_neg n print_var v
let print_clause fmt clause =
  print_list print_atom "\\/" fmt clause
let print_formula fmt f =
  print_list print_clause " /\\ " fmt f
let print_cnf fmt cnf =
  print_formula fmt cnf.f

let print_model fmt m =
  print_list (fun fmt b -> fprintf fmt "%s" (if b then "T" else "F")) ";"
    fmt (Array.to_list m)

let print_guess_model fmt m =
  print_list (fun fmt l
               -> fprintf fmt "%d:%s%s" l.var (if not l.defined then "x" else
                                      if l.neg = Neg then "F" else "T")
                   (if l.guessed then "?" else "")) " " fmt (PA.to_list m)

let print_formula_sat fmt (f : formula_sat) =
  print_list (fun fmt (b,_) -> fprintf fmt "%s" (if b then "Y" else "N")) "-"
    fmt f


(** Dummy solver: we test all possible assignments *)

(** Returns the list of all possible assignments, under a list form *)
let rec generate_models n =
  if n = 0 then
    [[]]
  else
    List.flatten (List.map (fun a -> [true :: a; false :: a]) (generate_models (n-1)))

(** Test an assignment on the CNF *)
let test_model cnf (m : model) =
  List.fold_left
    (fun b cl ->
       b &&
       (List.fold_left
          (fun b (n,v) ->
             b || (match n with Neg -> not m.(v) | NoNeg -> m.(v)))
          false cl))
    true cnf.f

(** Try all models *)
let rec try_all cnf = function
  | [] -> None
  | m :: ms -> if test_model cnf m then Some m else try_all cnf ms

let solve_all cnf =
  try_all cnf (List.map Array.of_list (generate_models cnf.nb_var))


(** DPLL solver *)

let to_simple_model (m : guess_model) =
  (PA.map (fun x -> x.neg = NoNeg) m : model)

let inverse_neg = function
  | Neg -> NoNeg
  | NoNeg -> Neg

let rec model_implies (m : guess_model) = function
  | (n,v)::l -> let x = PA.get m v in
    if x.defined && x.neg = n then true else model_implies m l
  | ([] : clause) -> false

let rec model_implies_not (m : guess_model) = function
  | (n,v)::l -> let x = PA.get m v in
    if x.defined && x.neg <> n then model_implies_not m l else false
  | ([] : clause) -> true

let rec check_model (m : guess_model) ((fail, unknown) as p) = function
  | ([] : clause) -> (true, unknown)
  | (n,v)::l -> match PA.get m v with
    | { defined = false; _ } -> check_model m ((fail : bool), true) l
    | { neg = n'; _ } -> if n = n' then (false, false) else check_model m p l

let unit (m : guess_model) (c : clause) =
  let find_lit (stop, check, lit, prec) ((n,v) : atom) =
    (* First, check if multiple undefined literals were not found before *)
    if stop then (stop, check, lit, prec) else
      (* Then check if current literal is defined in the model *)
    if (PA.get m v).defined then (stop, check, lit, (n,v)::prec) else
      (* Then, check if no other literal was defined before *)
    if check then (true, check, lit, prec) else (stop, true, (n,v), prec) in
  let (stop, check, (n,v), prec) =
    List.fold_left find_lit (false, false, (NoNeg, 0), []) c in
  if stop || not check || not (model_implies_not m prec) then (m, false) else
    ((PA.set m v {var = v; neg = n; defined = true; guessed = false}
      : guess_model), true)

let decide (m : guess_model) ((n,v) : atom) =
  (PA.set m v {var = v; neg = n; defined = true; guessed = true} : guess_model)

let fail (m : guess_model) (f : formula_sat) =
  List.fold_left (fun x c -> if x || fst c then x else model_implies_not m
                       (snd c)) false f

let success (m : guess_model) (f : formula_sat) =
  List.fold_left (fun x c -> if x && not (fst c) then model_implies m (snd c)
                   else x) true f

let success_fail (m : guess_model) (f : formula_sat) =
  let prune (f, (stop, valid)) c =
    if stop then (f, (stop, valid)) else
    if fst c then (c::f, (stop, valid)) else
      let (fail, unknown) = check_model m (false, false) (snd c) in
      let check = not (fail || unknown) in
      ( ((check, snd c)::f : formula_sat),
         ((if (not unknown) && fail then true else false), valid && check)) in
  List.fold_left prune ([], (false, true)) f


let best_literal (m : guess_model) =
  let l = PA.filter (fun x -> not x.defined) m in
  if l = [] then None else
    let r = Random.int (List.length l) in
    let x = List.nth l r in
    Some ((x.neg, x.var) : atom)

let extract_atom (l : clause) =
  List.fold_left (fun d x ->
      if l = [x] then d else (x, List.filter (fun y -> y<>x) l)::d) [] l

let unit_deal (m : guess_model) (f : formula_sat) =
  let update (m,(f : formula_sat)) (b,c) =
    if b then (m,((b,c)::f : formula_sat)) else
      let (m, modif) = unit m c in
      if modif then (m, (true,c)::f) else (m, (b,c)::f)
  in
  List.fold_left update (m, []) f

let dpll (m : guess_model) (f : formula_sat) =
  let rec step (m : guess_model) (f : formula_sat) =
    let (m,f) = unit_deal m f in
    let (f, (stop, valid)) = success_fail m f in
    if stop then None else if valid then Some m else
      match best_literal m with
      | None -> None
      | Some l
        -> match step (decide m l) f with
        | Some _ as x -> x
        | None
          -> step (PA.set m (snd l) {var = (snd l); neg = inverse_neg (fst l);
                                     defined = true; guessed = false}) f
  in
  step m f


let solve cnf =
  let make_model cnf =
    PA.init cnf.nb_var (fun i-> {var = i; neg = NoNeg;
                                 defined = false; guessed = false}) in
  let initialize f =
    List.fold_left (fun l x -> (false,x)::l) [] f in
  match dpll (make_model cnf) (initialize cnf.f) with
  | None -> None
  | Some m -> let model = to_simple_model m in assert (test_model cnf model);
    Some model
