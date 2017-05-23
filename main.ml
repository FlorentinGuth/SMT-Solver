module SMT = Smt

let solve file =
  let ch = open_in file in
  let buf = Lexing.from_channel ch in
  let cnf = Parser.file Lexer.token buf in
  if SMT.satisfiable cnf then print_endline "Satisfiable" else print_endline "Not satisfiable"

let () =
  Arg.parse
    []
    solve
    "Usage: solve formula.cnfuf"
