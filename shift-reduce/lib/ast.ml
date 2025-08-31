type expr =
  | Int of int
  | Add of expr * expr
  | Mul of expr * expr

let rec string_of_expr = function
  | Int n -> string_of_int n
  | Add (e1, e2) -> Printf.sprintf "(%s + %s)" (string_of_expr e1) (string_of_expr e2)
  | Mul (e1, e2) -> Printf.sprintf "(%s * %s)" (string_of_expr e1) (string_of_expr e2)

let rec eval = function
  | Int n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Mul (e1, e2) -> eval e1 * eval e2
