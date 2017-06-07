module UF = Explain_union_find


type var = int
type app = var * var
type literal =
  | Var of var
  | App of app

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
  uf: UF.t;
  edges_label : (var * var, app_eq * app_eq) Hashtbl.t;
}


(* TODO avoid initialization cost? *)
let create n =
  {
    pending = [];
    use     = Array.make n [];
    lookup  = Hashtbl.create n;
    uf      = UF.create n;
    edges_label = Hashtbl.create n;
  }


let rec propagate t =
  match t.pending with
  | [] -> ()
  | (Vars (a, b) as eq) :: tl | (Apps ((_, a), (_, b)) as eq) :: tl ->
    t.pending <- tl;
    let old_repr_a = UF.find t.uf a in
    let old_repr_b = UF.find t.uf b in
    if old_repr_a <> old_repr_b then begin
      UF.union t.uf a b;
      let () = match eq with
        | Vars _ -> ()
        | Apps (e1,e2) -> Hashtbl.add t.edges_label (a,b) (e1,e2)
      in
      let a' = UF.find t.uf a in
      let b' = UF.find t.uf b in
      let (a,b,old_repr_a,old_repr_b,a',b') = if b' = old_repr_a
        then (b,a,old_repr_b,old_repr_a,a',b')
        else (a,b,old_repr_a,old_repr_b,b',a')
      in
      List.iter (fun app_eq ->
                  let ((c1,c2),_c) = app_eq in  (* f(c1,c2)=c *)
                  let c1' = UF.find t.uf c1 in
                  let c2' = UF.find t.uf c2 in
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

let merge t literal a =
  match literal with
  | Var b ->
    t.pending <- (Vars (a, b)) :: t.pending;
    propagate t

  | App (a1,a2) ->
    let a1' = UF.find t.uf a1 in
    let a2' = UF.find t.uf a2 in
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
  UF.find t.uf a = UF.find t.uf b


let rec get_real_explanation t vars acc =
  try
    let (((a1,a2),_), ((b1,b2),_)) = Hashtbl.find t.edges_label vars in
    (explain t a1 b1) @ (explain t a2 b2)
  with Not_found ->
    let (v1,v2) = vars in
    [(Var v1,v2)]

and explain_along_path t a c =


and explain t a b =
  []
