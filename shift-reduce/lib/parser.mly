%{
  open Ast
%}

%token <int> INT
%token PLUS TIMES
%token LPAREN RPAREN
%token EOF

%start <Ast.expr> main

%%

main:
  | expr EOF { $1 }

(* This grammar has shift/reduce conflicts! *)
expr:
  | INT { Int $1 }
  | expr PLUS expr { Add ($1, $3) }
  | expr TIMES expr { Mul ($1, $3) }
  | LPAREN expr RPAREN { $2 }
