module SMT = Smt

let solve file =
  Random.self_init ();
  let ch = open_in file in
  let buf = Lexing.from_channel ch in
  let cnf = Parser.file Lexer.token buf in
  if SMT.satisfiable cnf then print_endline "Satisfiable" else print_endline "Not satisfiable"

let () =
  Arg.parse
    []
    solve
    "Usage: solve formula.cnfuf"


(*
open Printer
module PA = Persistent_array
type froude = {i : int; b : bool}

let rec listof m =
  if m = 0 then [0] else m :: (listof (m-1))

let () =
  let len = 5 in
  let disp a =
    List.iter (fun x -> print_stdout "%i%s, "
                  x.i (if x.b then "T" else "F")) (PA.to_list a);
    print_stdout "@." in
  let a = PA.init len (fun x -> {i = x; b = false}) in
  let c = List.fold_left (fun b i -> (disp b; PA.set b i {i = i; b = true});
                         ) a (listof (len-1)) in
  print_stdout "Printing@.";
  disp c; disp a;
  let d = PA.set a 0 {i = 6; b = true} in
  disp d; disp a
*)
