open Format
open Printer


type var     = int          (** A boolean variable *)
type neg     = Neg | NoNeg  (** Whether the variable is negated or not *)
type atom    = neg * var
type clause  = atom list    (** Represents a disjunction *)
type formula = clause list

type cnf = {
  nb_var : int; (** Variables in the formula_sat range from 0 to nb_vars - 1 *)
  nb_cl  : int; (** The number of clauses in the formula_sat *)
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
 * Type literal contains informations *)
type guess_model = literal PA.t
(** A pseudo model is an array of bool option representing the assignments of
 * all literals, if they are assigned (or None elsehow). 
 * It is used by the smt part to indicates which clauses are to be added*)
type pseudo_model = (bool option) array
(** A model is an assignment of all variables *)
type model = bool array

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

(** to_simple_model converts type guess_model into type model *)
let to_simple_model (m : guess_model) =
  (PA.map (fun x -> x.neg = NoNeg) m : model)

(** to_pseudo_model converts type guess_model into type pseudo_model*)
let to_pseudo_model (m : guess_model) =
  ((PA.map (function
       | { defined = false; _ } -> None
       | { neg; _ } -> Some (neg = NoNeg) ) m) : pseudo_model)

let inverse_neg = function
  | Neg -> NoNeg
  | NoNeg -> Neg

(** model_implies m c checks wether the given model implies clause c *)
let rec model_implies (m : guess_model) = function
  | (n,v)::l -> let x = PA.get m v in
    if x.defined && x.neg = n then true else model_implies m l
  | ([] : clause) -> false

(** model_implies m c checks wether the given model implies that clause c must 
 * be invalid *)
let rec model_implies_not (m : guess_model) = function
  | (n,v)::l -> let x = PA.get m v in
    if x.defined && x.neg <> n then model_implies_not m l else false
  | ([] : clause) -> true

(** decide m a sets the new value of a literal in the model*)
let decide (m : guess_model) ((n,v) : atom) =
  (PA.set m v {var = v; neg = n; defined = true; guessed = true} : guess_model)

(** fail m f is true if the formula cannot be satisfied by the model *)
let fail (m : guess_model) (f : formula) =
  List.fold_left (fun x c -> if x then x else model_implies_not m c) false f

(** success m f is true if the model satisfies the formula *)
let success (m : guess_model) (f : formula) =
  List.fold_left (fun x c -> if x then model_implies m c else x) true f

(** success_fail m f checks the formula can be validated, in which case the
 * returned boolean is false; elsehow, it is set to true.
 * In the process, it removes from the formula the already validated clauses,
 * so that if the returned formula is empty, it means the model validates it. *)
let success_fail (m : guess_model) (f : formula) =
  let rec check_model ((fail, unknown) as p) = function
  | ([] : clause) -> (true, unknown)
  | (n,v)::l -> match PA.get m v with
    | { defined = false; _ } -> check_model ((fail : bool), true) l
    | { neg = n'; _ } -> if n = n' then (false, false) else check_model p l in

  let prune (f, stop) c =
    if stop then (f, stop) else
      let (fail, unknown) = check_model (false, false) c in
      let check = not (fail || unknown) in
      ( (if check then f (* clause is satisfied *) else c::f : formula),
        (if (not unknown) && fail then true else false)) in

  List.fold_left prune ([], false) f

(** success_fail_prune m f does the same thing as success_fail, except that 
 * it also removes from each clause the already not validated literals,
 * so that if a clause is empty, it is invalid.
 * This method is in practice less efficient than only using success_fail *)
let success_fail_prune (m : guess_model) (f : formula) =
  let rec check_model_prune (((c:clause), valid) as p) = function
  | ([] : clause) -> p
  | ((n,v) as x)::l -> match PA.get m v with
    | { defined = false; _ } -> check_model_prune (x::c, valid) l
    | { neg = n'; _ } -> if n = n' then (c, true)
      else check_model_prune p l in

  let prune (m, f, stop) c =
    if stop then (m, f, stop) else
      let (c, valid) = check_model_prune ([], false) c in
      let valid, m = match c with
        | [(n,v)] -> (true, PA.set m v { var = v; neg = n; defined = true;
                                         guessed = false })
        | _ -> (valid, m) in
      (m, (if valid then f (* clause is satisfied *) else c::f : formula),
       (if c = [] && not valid (* Failure *) then true else false)) in

  List.fold_left prune (m, [], false) f

(** first_literal m c assigns a value to the first non assigned literal in c
 *  so that c becomes a valid clause.
 *  This function assumes that no clause left is valid *)
let first_literal (m : guess_model) (c : clause) =
  List.fold_left (fun x ((_,v) as lit) ->
      if not (PA.get m v).defined then lit else x) (NoNeg, -1) c

(** rand_literal m assigns a random value to a random non assigned literal *)
let rand_literal (m : guess_model) =
  let (l,size) = PA.fold_left (fun (l,s) x -> match x with
      | { defined = true; _ } -> (l,s)
      | { var = v; _ } -> (v::l, s+1) ) ([],0) m in
  let r = Random.int size in
  (((if Random.bool () then NoNeg else Neg), List.nth l r) : atom)

(** extract_atom [a;b;c] is [(c,[a;b]) ; (b,[a;c]) ; (a,[b;c])] for instance *)
let extract_atom (l : clause) =
  List.fold_left (fun d x ->
      if l = [x] then d else (x, List.filter (fun y -> y<>x) l)::d) [] l

(** unit m c applies the unit rule to c. The returned boolean indicates wether
 * a modification was carried out or not. *)
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

(** unit_deal m f applies the unit rule everywhere it is possible *)
let unit_deal (m : guess_model) (f : formula) =
  let update (m,(f : formula)) c =
    let (m, modif) = unit m c in
    if modif then (m, f) else (m, c::f)
  in
  List.fold_left update (m, []) f

(** dpll is a wrapper to the CPS function step *)
let dpll (m : guess_model) (f : formula) check count nb_var =

  (**
   * counter, set_count, divide_count and max_array are used to carry out the
   * VSIDS decision heuristic
  *)

  let counter = ref 1 in

  let set_count f =
    let rec set_count_aux = function
      | [] -> ()
      | (n,v)::l -> let (pos,neg) = count.(v) in
        count.(v) <- (if n = Neg then (pos,neg+1) else (pos+1,neg));
        set_count_aux l in
    List.iter set_count_aux f in
  set_count f;

  let divide_count m =
    Array.iteri (fun i (p,n) -> count.(i) <- (p/m, n/m)) count in

  let max_array (m : guess_model) =
    let rec foldi i_max max positive n =
      if n = -1 then (((if positive then NoNeg else Neg), i_max) : atom) else
        let (pos,neg) = count.(n) in
        let c = pos + neg in
        if max >= c || (PA.get m n).defined
        then foldi i_max max positive (n-1)
        else foldi n c (pos >= neg) (n-1) in
    foldi (-1) (-1) true nb_var in

  (**
   * step m f k applies all unit rules to m and f, then checks if the model:
   * - is not satisfiable -> it fails
   * - is satisfied       -> it calls the check function from the smt part.
   * If it is not stopped yet, that means the model does not validate the
   * formula, which may have been corrected by the call to check.
   * Then, it guesses the value of a non assigned literal and continues
   * recursively. If needed, it backtracks to the precedent guess.
   * The k parameter is the continuation (to make the function tail-recursive)
  *)
  let rec step (m : guess_model) (f : formula) k =
    if (!counter mod 16777216) = 0 then divide_count 8;
    counter := !counter + 1;
    let (m,f) = unit_deal m f in
    let (f, stop) = success_fail m f in
    if stop then k (None, []) else
      let (f,back) = begin
        if f = []
        then let g = check (to_pseudo_model m) in
          (if g = [] then (g, false)
           else (set_count g; (g, true)))
        else (f, false) end in
      if back then k (None, f) else
        match f with
      | [] -> k (Some m, []) (* A valid model was found! *)
      | _  -> let (l,f') = (max_array m, f) in
                (* if Random.bool () (* Two heuristics are used *)
          then (first_literal m c, f') (* - Validate one more clause *)
          else (rand_literal m, f) in  (* - Choose at random *) *)
        step (decide m l) f' (function
            | (Some _, _) as x -> x (* Model found! *)
            | (None, f'') -> (* Bactrack *)
              step (PA.set m (snd l) {var = (snd l); neg = inverse_neg (fst l);
                                      defined = true; guessed = false})
                (List.rev_append f'' f) k)
  in
  step m f (fun x-> x)

(** solve cnf check is the main function. It returns
 *  - None if cnf is not satisfiable
 *  - Some m if a model m was found that satisfies cnf, and that is valid modulo
 *    theories (as checked by the check function) 
 * In both cases, solve only returns when the problem is finished *)
let solve cnf check =
  let count = Array.init cnf.nb_var (fun _ -> (0,0)) in
  let make_model cnf =
    PA.init cnf.nb_var (fun i-> {var = i; neg = NoNeg;
                                 defined = false; guessed = false}) in
  match fst (dpll (make_model cnf) cnf.f check count (cnf.nb_var-1)) with
  | None -> None
  | Some m -> let model = to_simple_model m in Some model
