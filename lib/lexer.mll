{
    open Parser

    exception SyntaxError of string
}

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t' '\r' '\n']+
(* let newline = '\r' | '\n' | "\r\n" *)
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white    { read lexbuf }
  (* | newline  { new_line lexbuf; read lexbuf } *)
  | float    { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | "def"    { DEF }
  | "extern" { EXTERN }
  | "end"    { END }
  | "+"      { ADD }
  | "*"      { MUL }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | ","      { COMMA }
  | id       { IDENT (Lexing.lexeme lexbuf) }
  | _ { raise ( SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
