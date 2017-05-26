%{
  open Smt

  let rec filter_some = function
    | [] -> []
    | None :: tl -> filter_some tl
    | Some x :: tl -> x :: filter_some tl

  let literals_tbl = Hashtbl.create 100 (* Maps a literal to its lit *)
  let free_lit = ref 0
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
               (let v = v - 1 in v) }

func:
| f = FUNC; { f }

literal:
| v = var; { MC.Var v }
| f = func; LPAR; l = separated_nonempty_list(COMMA, lit); RPAR;
  { MC.Func (f, l) }
lit:
| l = literal; { try
                   Hashtbl.find literals_tbl l
                 with Not_found ->
                   let lit = !free_lit in
                   incr free_lit;
                   Hashtbl.add literals_tbl l lit;
                   lit
               }

atom:
| i = lit;  EQ; j = lit; { (MC. Eq, i, j) }
| i = lit; NEQ; j = lit; { (MC.Neq, i, j) }

clause:
| cl = nonempty_list(atom); { cl }

formula:
| f = separated_nonempty_list(NEWLINE, option(clause));  (* We use options to read all newlines *)
  { filter_some f }

file:
| option(newline);
  P; CNF; nb_var = INT; nb_cl = INT;
  NEWLINE;
  f = formula;
  EOF;
  {
    (* We just ignore the number of variables and clauses, and recompute it *)
    let nb_lit = !free_lit in
    let literals = Array.make nb_lit (MC.Var 0) in
    Hashtbl.iter (fun literal lit -> literals.(lit) <- literal) literals_tbl;
    { nb_lit; f; literals }
  }
