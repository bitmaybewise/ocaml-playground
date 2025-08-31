{
  open Parser

  exception SyntaxError of string
}

let white = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let int = digit+

rule token = parse
  | white+     { token lexbuf }
  | newline    { Lexing.new_line lexbuf; token lexbuf }
  | int        { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '+'        { PLUS }
  | '*'        { TIMES }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | eof        { EOF }
  | _          { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
