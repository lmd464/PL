module F = Format

(* practice *)
let rec interp (s : Store.t) (e : Ast.vae) : int = 
  match e with
  | Ast.Num int_var -> int_var
  | Ast.Add (expr1, expr2) -> (interp s expr1) + (interp s expr2)
  | Ast.Sub (expr1, expr2) -> (interp s expr1) - (interp s expr2)
  | Ast.Id id -> (Store.find id s)
  | Ast.LetIn (id, expr1, expr2) -> let s' = (Store.insert id (interp s expr1) s) in
                                    interp s' expr2