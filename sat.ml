open Format
open Printer


type var     = int          (** A boolean variable *)
type neg     = Neg | NoNeg  (** Whether the variable is negated *)
type atom    = neg * var
type clause  = atom list    (** Represents a disjunction *)
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
type pseudo_model = (bool option) array
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

let to_pseudo_model (m : guess_model) =
  PA.map (function
       | { defined = false; _ } -> None
       | { neg; _ } -> Some (neg = NoNeg) ) m

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

let fail (m : guess_model) (f : formula) =
  List.fold_left (fun x c -> if x then x else model_implies_not m c) false f

let success (m : guess_model) (f : formula) =
  List.fold_left (fun x c -> if x then model_implies m c else x) true f

let success_fail (m : guess_model) (f : formula) =
  let prune (f, stop) c =
    if stop then (f, stop) else
      let (fail, unknown) = check_model m (false, false) c in
      let check = not (fail || unknown) in
      ( (if check then f (* clause is satisfied *) else c::f : formula),
        (if (not unknown) && fail then true else false)) in
  List.fold_left prune ([], false) f

(* This function assumes that no clause left is valid *)
let first_literal (m : guess_model) (c : clause) =
  List.fold_left (fun x ((_,v) as lit) ->
      if not (PA.get m v).defined then lit else x) (NoNeg, -1) c

let rand_literal (m : guess_model) =
  let (l,size) = PA.fold_left (fun (l,s) x -> match x with
      | { defined = true; _ } -> (l,s)
      | { var = v; _ } -> (v::l, s+1) ) ([],0) m in
  let r = Random.int size in
  (((if Random.bool () then NoNeg else Neg), List.nth l r) : atom)

let extract_atom (l : clause) =
  List.fold_left (fun d x ->
      if l = [x] then d else (x, List.filter (fun y -> y<>x) l)::d) [] l

let unit_deal (m : guess_model) (f : formula) =
  let update (m,(f : formula)) c =
    let (m, modif) = unit m c in
    if modif then (m, f) else (m, c::f)
  in
  List.fold_left update (m, []) f

let dpll (m : guess_model) (f : formula) check =
  let rec step (m : guess_model) (f : formula) k =
    let (m,f) = unit_deal m f in
    let (f, stop) = success_fail m f in
    if stop then k (None, []) else
      let f = if f = [] then check (to_pseudo_model m) else f in
      match f with
      | [] -> k (Some m, [])
      (*| c::f' -> let l = first_literal m c in step (decide m l) f' (function*)
      | _ as f' -> let l = rand_literal m in step (decide m l) f' (function
          | (Some _, _) as x -> x (* Model found! *)
          | (None, f'') (* Bactrack *)
            -> step (PA.set m (snd l) {var = (snd l); neg = inverse_neg (fst l);
                                       defined = true; guessed = false})
                 (List.rev_append f'' f) k)
  in
  step m f (fun x-> x)


let solve cnf check =
  let make_model cnf =
    PA.init cnf.nb_var (fun i-> {var = i; neg = NoNeg;
                                 defined = false; guessed = false}) in
  match fst (dpll (make_model cnf) cnf.f check) with
  | None -> None
  | Some m -> let model = to_simple_model m in assert (test_model cnf model);
    Some model
