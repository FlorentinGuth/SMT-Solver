{
open Parser
}


let epsilon = ""
let newline = "\n" | "\r\n"
let separator = [' ' '\t']
let digit = ['0'-'9']
let integer = (digit+)
let letter = ['a'-'z' 'A'-'Z']
let ident = (letter+)


rule token = parse
  | separator    { token lexbuf }
  | newline      { Lexing.new_line lexbuf; NEWLINE }
  | ident as s   { match s with
        | "p"   -> P
        | "cnf" -> CNF
        | "c"   -> comment lexbuf
        | f     -> FUNC f
      }
  | integer as n { INT (int_of_string n) }
  | "="          { EQ }
  | "!=" | "<>"  { NEQ }
  | "("          { LPAR }
  | ")"          { RPAR }
  | ","          { COMMA }
  | eof          { EOF }

and comment = parse
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | eof     { EOF }
  | _       { comment lexbuf }
