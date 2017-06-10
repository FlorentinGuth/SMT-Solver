module EUF = Explain_union_find
module PUF = Proof_union_find


type var = int
type app = var * var
type literal =
  | Var of var
  | App of app

type eq     = literal * var
type var_eq = var * var
type app_eq = app * var

type pending =
  | Vars of var_eq           (* a=b *)
  | Apps of app_eq * app_eq  (* f(a1,a2)=a and f(b1,b2)=b where a1=b1 and a2=b2 *)

type t = {
  mutable pending: pending list;
  (* use.(a) is a list of equations in which a is the representative of one of the arguments *)
  use: app_eq list array;
  (* lookup.(b).(c) is f(a1,a2)=a iff b and c are the representatives of a1 and a2 *)
  lookup: (var * var, app_eq) Hashtbl.t;
  euf: EUF.t;
  puf: PUF.t;
  edges_label : (var * var, app_eq * app_eq) Hashtbl.t;
}


(* TODO avoid initialization cost? *)
let create n =
  {
    pending = [];
    use     = Array.make n [];
    lookup  = Hashtbl.create n;
    euf = EUF.create n;
    puf = PUF.create n;
    edges_label = Hashtbl.create n;
  }


let find_label t (a,b) =
  try
    Hashtbl.find t.edges_label (a,b)
  with Not_found ->
    Hashtbl.find t.edges_label (b,a)


let rec propagate t =
  match t.pending with
  | [] -> ()
  | (Vars (a, b) as eq) :: tl | (Apps ((_, a), (_, b)) as eq) :: tl ->
    t.pending <- tl;
    let old_repr_a = EUF.find t.euf a in
    let old_repr_b = EUF.find t.euf b in
    if old_repr_a <> old_repr_b then begin
      EUF.union t.euf a b;
      let () = match eq with
        | Vars _ -> ()
        | Apps (e1,e2) -> Hashtbl.add t.edges_label (a,b) (e1,e2)
      in
      let a' = EUF.find t.euf a in
      let b' = EUF.find t.euf b in
      let (a,b,old_repr_a,old_repr_b,a',b') = if b' = old_repr_a
        then (b,a,old_repr_b,old_repr_a,a',b')
        else (a,b,old_repr_a,old_repr_b,b',a')
      in
      List.iter (fun app_eq ->
                  let ((c1,c2),_c) = app_eq in  (* f(c1,c2)=c *)
                  let c1' = EUF.find t.euf c1 in
                  let c2' = EUF.find t.euf c2 in
                  try
                    let app_eq2 = Hashtbl.find t.lookup (c1',c2') in
                    t.pending <- (Apps (app_eq, app_eq2)) :: t.pending
                  with Not_found ->
                    Hashtbl.add t.lookup (c1',c2') app_eq;
                    t.use.(b') <- app_eq :: t.use.(b')
                ) t.use.(old_repr_a);
      t.use.(old_repr_a) <- [];
      propagate t
    end

let merge t (literal,a) =
  match literal with
  | Var b ->
    t.pending <- (Vars (a, b)) :: t.pending;
    propagate t

  | App (a1,a2) ->
    let a1' = EUF.find t.euf a1 in
    let a2' = EUF.find t.euf a2 in
    try
      let app_eq = Hashtbl.find t.lookup (a1',a2') in
      t.pending <- (Apps (((a1,a2),a), app_eq)) :: t.pending;
      propagate t
    with Not_found ->
      let app_eq = ((a1,a2),a) in
      Hashtbl.add t.lookup (a1',a2') app_eq;
      t.use.(a1') <- app_eq :: t.use.(a1');
      t.use.(a2') <- app_eq :: t.use.(a2')


let are_congruent t a b =
  EUF.find t.euf a = EUF.find t.euf b



let rec get_real_explanation t vars acc =
  try
    let (((a1,a2),a), ((b1,b2),b)) = find_label t vars in
    explain_aux t a1 b1 (explain_aux t a2 b2 ((App (a1,a2),a) :: (App (b1,b2),b) :: acc))
  with Not_found ->
    let (v1,v2) = vars in
    (Var v1,v2) :: acc

and explain_along_path t a c acc =
  (* c is an ancestor of a *)
  let rec aux a acc =
    let a = PUF.highest_node t.puf a in
    if a = c then acc else
      let b = EUF.proof_parent t.euf a in
      get_real_explanation t (a,b) (aux b acc)
  in
  aux a acc

and explain_aux t a b acc =
  let c = EUF.nearest_common_ancestor t.euf a b in
  explain_along_path t a c (explain_along_path t b c acc)

and explain t a b =
  let res = explain_aux t a b [] in
  PUF.reset t.puf;
  (* TODO: Elimination of redundant clauses *)
  res



let print_literal fmt = function
  | Var a -> Format.fprintf fmt "%d" a
  | App (a1,a2) -> Format.fprintf fmt "%d(%d)" a1 a2
let print_equality fmt (l,v) =
  Format.fprintf fmt "%a=%d" print_literal l v
let print_equality_list fmt l =
  Printer.print_list print_equality " " fmt l

let test () =
  (* 0:f 1:g 2:x 3:y 4:f(x) 5:g(y); x=y, f=g => f(x)=g(y) *)
  let t = create 6 in
  merge t ((Var 0),1);
  merge t ((Var 2),3);
  merge t ((App (0,2)),4);
  merge t ((App (1,3)),5);
  Printer.print_stdout "%a\n" print_equality_list (explain t 4 5)
(* let () = test () *)


type model = {
  nb_var : int;
  nb_real_var : int;
  eqs    : eq list;
  neqs   : eq list;
  var_of_app : (app,var) Hashtbl.t;
}

let check m =
  let t = create m.nb_var in
  List.iter (merge t) m.eqs;
  let rec check_neqs = function
    | [] ->
      Printer.print_stdout "Model valid: %a\n@." EUF.print_classes t.euf;
      None

    | neq :: neqs ->
      let (v1,v2) = match neq with
        | (Var v1,v2) -> (v1,v2)
        | (App a, v2) -> (Hashtbl.find m.var_of_app a,v2)
      in
      if are_congruent t v1 v2 then
        Some ((Mc.Neq,neq) :: (List.map (fun eq -> (Mc.Eq,eq)) (explain t v1 v2)))
      else
        check_neqs neqs
  in
  check_neqs m.neqs
