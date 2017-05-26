open Format
open Printer

module UF = Union_find


type var     = int
type func    = string
type lit     = int              (* The ident of a literal *)

type literal =
  | Var  of var
  | Func of func * (lit list)

type rel     = Eq | Neq
type clause  = rel * lit * lit
type formula = clause list      (* Represents a conjunction *)

type model = {
  (* nb_var   : int;               (\* Variables in the formula range from 0 to nb_vars - 1 *\) *)
  (* nb_func  : int;               (\* Same as above for functions *\) *)
  nb_lit   : int;               (* Same as above for literals *)
  (* nb_cl    : int;               (\* The number of clauses in the formula *\) *)
  f        : formula;
  literals : literal array;     (* Maps a literal ident to the literal *)
}


let print_model fmt m =
  let rec print_lit fmt lit =
    match m.literals.(lit) with
    | Var v -> fprintf fmt "%d" v
    | Func (f, l) -> fprintf fmt "%s(%a)" f print_lit_list l
  and print_lit_list fmt l =
    print_list print_lit "," fmt l
  in
  let print_rel fmt rel =
    fprintf fmt "%s" (match rel with Eq -> eq_str | Neq -> neq_str)
  in
  let print_atom fmt (rel,l1,l2) =
    fprintf fmt "%a%a%a" print_lit l1 print_rel rel print_lit l2
  in
  let print_formula fmt f =
    print_list print_atom and_str fmt f
  in
  (* let print_int fmt i = *)
  (*   fprintf fmt "%d" i *)
  (* in *)
  (* let print_literals fmt literals = *)
  (*   Array.iteri (fun lit literal -> *)
  (*       fprintf fmt "%d -> " lit; *)
  (*       match literal with *)
  (*       | Var v -> fprintf fmt "%d@." v *)
  (*       | Func (f, l) -> fprintf fmt "%s(%a)@." f (print_list print_int ",") l *)
  (*     ) literals *)
  (* in *)
  print_formula fmt m.f
  (* print_literals fmt m.literals *)


(** We check a model the following way:
    - first, we use a union find structure in which every variable is alone,
    - then, we consider all equalities and merge the adequate classes,
    - then comes the inference part: if x equals y then so does f(x) and f(y),
    - last, if there is an inequality between two variables of the same class,
      then the model is not satisfiable, otherwise it is *)

type graph = {
  size : int;
  succ : int list array;  (* Maps a node to its successors, i.e. its arguments *)
  pred : int list array;  (* Maps a node to its predecessors, i.e. the literals in which it appears *)
}
let create_graph n =
  { size = n; succ = Array.make n []; pred = Array.make n [] }
let add_edge g x y =
  g.succ.(x) <- y :: g.succ.(x);
  g.pred.(y) <- x :: g.pred.(y)
let graph_of_literals nb_lit literals =
  let g = create_graph nb_lit in
  for lit = 0 to nb_lit - 1 do
    match literals.(lit) with
    | Var v -> ()
    | Func (f, l) -> List.iter (add_edge g lit) l
  done;
  g
(** Returns a permutation of the nodes, where x is before f(x) *)
let topological_sort g =
  let seen = Array.make g.size false in
  let order = ref [] in
  let rec dfs s =
    if not seen.(s) then begin
      seen.(s) <- true;
      List.iter dfs g.succ.(s);
      order := s :: !order
    end
  in
  for s = 0 to g.size - 1 do
    dfs s
  done;
  List.rev !order

(** Checks if s1 and s2 are congruent, that is all their sons are equal *)
let congruent uf g literals s1 s2 =
  if UF.find uf s1 = UF.find uf s2 then false else
    match (literals.(s1), literals.(s2)) with
    | (Func (f1, _), Func (f2, _)) when f1 = f2 ->
      begin try
          List.for_all2 (fun lit1 lit2 -> UF.find uf lit1 = UF.find uf lit2) g.succ.(s1) g.succ.(s2)
        with Invalid_argument _ -> false (* Not same outdegree *)
      end
    | _ -> false

let check model =
  let g = graph_of_literals model.nb_lit model.literals in
  let topo = Array.of_list (topological_sort g) in
  let uf = UF.create model.nb_lit in
  let rec check_aux f neqs =
    match f with
    | (Eq, i, j) :: tl ->
      (* We add the equality to the DAG *)
      UF.union uf i j;
      check_aux tl neqs

    | (Neq, i, j) :: tl ->
      (* We add the inequality to our to-do-check-list *)
      check_aux tl ((i, j) :: neqs)

    | [] ->
      (* Let's infer all possible equalities *)
      (* TODO: do this with DST algorithm *)
      let i = ref 0 in
      let j = ref 0 in
      while !i < model.nb_lit do
        while !j < !i do
          if congruent uf g model.literals topo.(!i) topo.(!j) then begin
            UF.union uf topo.(!i) topo.(!j);
            i := !j;
            j := 0;
          end else
            incr j
        done;
        incr i;
        j := 0;
      done;
      (* Let's check all inequalities *)
      List.for_all (fun (i, j) -> UF.find uf i <> UF.find uf j) neqs
  in
  check_aux model.f []
