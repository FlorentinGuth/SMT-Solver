%{
  open Smt

  let rec filter_some = function
    | [] -> []
    | None :: tl -> filter_some tl
    | Some x :: tl -> x :: filter_some tl

  type lit =
    | Var of int
    | Func of string
    | App of lit * lit

  let nb_var = ref 0
%}


%token P CNF
%token <int> INT
%token <string> FUNC
%token LPAR RPAR COMMA
%token EQ NEQ
%token NEWLINE
%token EOF


%start <Smt.cnf> file


%%

newline:
| nonempty_list(NEWLINE); { () }

var:
| v = INT; { if v = 0 then failwith "Incorrect variable 0" else
               (nb_var := max !nb_var v; let v = v - 1 in v) }

func:
| f = FUNC; { f }


lit:
| v = var; { Var v }
| f = func; LPAR; l = separated_list(COMMA, lit); RPAR;
  (* There can be 0 parameters, and f would be a var *)
  { let rec fold = function
      | []          -> assert false
      | [lit]       -> lit
      | lit :: lits -> App (lit,fold lits)
    in
    fold ((Func f) :: l)
  }

rel:
|  EQ; { MC. Eq }
| NEQ; { MC.Neq }

atom:
| i = lit; r = rel; j = lit; { (r, i, j) }

clause:
| cl = nonempty_list(atom); { cl }

formula:
| f = separated_nonempty_list(NEWLINE, option(clause));  (* We use options to read all newlines *)
  { filter_some f }

file:
| option(newline);
  P; CNF; _nb_var = INT; _nb_cl = INT;
  NEWLINE;
  f = formula;
  EOF;
  {
    (* We just ignore the number of variables and clauses *)
    let free_var = ref !nb_var in
    let var_of_func = Hashtbl.create 100 in  (* Maps a string to an int *)
    let var_of_app  = Hashtbl.create 100 in  (* Maps a (int,int) to an int *)
    let rec map_lit = function
      | Var v  ->
         v

      | Func f -> begin
          try
            Hashtbl.find var_of_func f
          with Not_found ->
            let v = !free_var in
            incr free_var;
            Hashtbl.add var_of_func f v;
            v
        end

      | App (l1,l2) ->
         let v1 = map_lit l1 in
         let v2 = map_lit l2 in
         try
           Hashtbl.find var_of_app (v1,v2)
         with Not_found ->
           let v = !free_var in
           incr free_var;
           Hashtbl.add var_of_app (v1,v2) v;
           v
    in
    let map_formula =
      List.map (List.map (fun (r,l1,l2) -> (r,(IMC.Var (map_lit l1),map_lit l2))))
    in
    let f = map_formula f in
    (* Now we need to add all the new equalities *)
    let f = Hashtbl.fold (fun (v1,v2) v f -> [(MC.Eq,(IMC.App(v1,v2),v))] :: f) var_of_app f in
    let app_of_var = Array.make !free_var None in
    Hashtbl.iter (fun a v -> app_of_var.(v) <- Some a) var_of_app;
    { nb_var = !free_var; nb_real_var = !nb_var; f; var_of_app; app_of_var }
  }
