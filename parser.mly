%{
  open Ast
%}


%token P CNF
%token <int> INT
%token EQ NEQ
%token NEWLINE
%token EOF


%start <Ast.cnf> file


%%

newline:
| nonempty_list(NEWLINE); { () }

var:
| v = INT; { if v = 0 then failwith "Incorrect variable 0" else v - 1 }

atom:
| i = var;  EQ; j = var; {  Eq, i, j }
| i = var; NEQ; j = var; { Neq, i, j }

clause:
| cl = nonempty_list(atom); { cl }

formula:
| f = separated_nonempty_list(newline, clause); { f }

file:
| P; CNF; nb_var = INT; nb_cl = INT; newline;
  f = formula;
  list(NEWLINE); EOF;
  {
    if List.length f <> nb_cl
    then failwith "Incorrect number of clauses"
    else if List.exists (List.exists (fun (_,i,j) -> i >= nb_var || j >= nb_var)) f
    then failwith "Incorrect number of variables"
    else { nb_var; nb_cl; f; }
  }
