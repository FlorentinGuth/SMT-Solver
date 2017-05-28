module SMT = Smt

let test () =
  let module UF = Explain_union_find in
  let t = UF.create 5 in
  UF.union t 0 1;
  UF.union t 2 3;
  UF.union t 1 3;
  let explain (i,j) = Printf.printf"explain %d %d: " i j;
    List.iter(fun(i,j)->Printf.printf"%d=%d "i j)(UF.explain t i j);Printf.printf"\n" in
  let rec zip u = function
    | [] -> []
    | x :: xs -> (List.map (fun y -> (y, x)) u) @ (zip u xs)
  in
  let v = [0;1;2;3] in
  List.iter explain (zip v v)

let solve file =
  test ();
  Random.self_init ();
  (* Random.init 1; *)
  let ch = open_in file in
  let buf = Lexing.from_channel ch in
  let cnf = Parser.file Lexer.token buf in
  if SMT.satisfiable cnf then print_endline "Satisfiable" else print_endline "Not satisfiable"

let () =
  Arg.parse
    []
    solve
    "Usage: solve formula.cnfuf"
