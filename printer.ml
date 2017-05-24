open Format


let print_stdout thing =
  Format.fprintf Format.std_formatter thing

let print_list printer sep =
  let rec list_printer fmt l =
    match l with
    | [] -> ()
    | [x] -> printer fmt x
    | x :: xs -> fprintf fmt "%a%s%a" printer x sep list_printer xs
  in
  list_printer
