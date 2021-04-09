module F = Format

(* practice *)
let rec interp (e : Ast.ae) : int = 
  match e with
  | Ast.Num int_var -> int_var
  | Ast.Add (expr1, expr2) -> interp expr1 + interp expr2
  | Ast.Sub (expr1, expr2) -> interp expr1 - interp expr2