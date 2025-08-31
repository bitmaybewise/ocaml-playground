open Shift_reduce

let parse_string s =
  let lexbuf = Lexing.from_string s in
  try
    Ok (Parser.main Lexer.token lexbuf)
  with
  | Lexer.SyntaxError msg -> Error ("Lexer error: " ^ msg)
  | Parser.Error ->
    let pos = Lexing.lexeme_start_p lexbuf in
    Error (Printf.sprintf "Parser error at line %d, character %d"
           pos.pos_lnum (pos.pos_cnum - pos.pos_bol))

let test_expressions = [
  "2 + 3 * 4";
  "2 * 3 + 4";
  "(2 + 3) * 4";
  "2 + (3 * 4)";
  "1 + 2 + 3";
  "1 * 2 * 3";
]

let () =
  Printf.printf "=== Arithmetic Expression Parser ===\n\n";
  List.iter (fun expr ->
    Printf.printf "Input: %s\n" expr;
    match parse_string expr with
    | Ok ast ->
      Printf.printf "  Parsed as: %s\n" (Ast.string_of_expr ast);
      Printf.printf "  Evaluates to: %d\n" (Ast.eval ast);
    | Error msg ->
      Printf.printf "  Error: %s\n" msg;
    Printf.printf "\n"
  ) test_expressions
