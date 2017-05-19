let solve file =
  let ch = open_in file in
  let buf = Lexing.from_channel ch in
  let cnf = Parser.file Lexer.token buf in
  print_endline "OK"


let () =
  Arg.parse
    []
    solve
    "Usage: solve formula.cnfuf"
