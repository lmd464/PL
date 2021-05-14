module F = Format

(* practice & homework *)
let rec interp_e (s : Store.t) (e : Ast.expr) : Store.value = 
  match e with
  | Ast.Num int_var -> Store.NumV int_var
  | Ast.Add (expr1, expr2) -> let expr1_exam = interp_e s expr1 in
                              let expr2_exam = interp_e s expr2 in
                              begin
                                match (expr1_exam, expr2_exam) with
                                | (Store.NumV e1_val, Store.NumV e2_val) -> NumV (e1_val + e2_val)
                                | _ -> failwith (F.asprintf "Invalid addition : %a + %a" Ast.pp_e expr1 Ast.pp_e expr2)
                              end
                              
  | Ast.Sub (expr1, expr2) -> let expr1_exam = interp_e s expr1 in
                              let expr2_exam = interp_e s expr2 in
                              begin
                                match (expr1_exam, expr2_exam) with
                                | (Store.NumV e1_val, Store.NumV e2_val) -> NumV (e1_val - e2_val)
                                | _ -> failwith (F.asprintf "Invalid subtraction : %a - %a" Ast.pp_e expr1 Ast.pp_e expr2)
                              end

  | Ast.Id id -> (Store.find id s)
  | Ast.LetIn (id, expr1, expr2) -> let s' = (Store.insert id (interp_e s expr1) s) in
                                    interp_e s' expr2
  
  | Ast.Fun (func_arg_name, func_body) -> Store.ClosureV (func_arg_name, func_body, s)

  | Ast.App (func, func_arg) -> let func_closure = interp_e s func in
                                begin
                                  match func_closure with
                                  | Store.ClosureV (func_arg_name, func_body, func_store) -> let interpreted_arg = interp_e s func_arg in   (* 기존의 Store에서 인자를 계산 *)
                                                                                             (* pair = (func_arg_name, interpreted_arg) *)
                                                                                             (* 함수 시점의 store에, 계산한 인자를 저장 *)
                                                                                             let new_store = Store.insert func_arg_name interpreted_arg func_store in  
                                                                                             interp_e new_store func_body
                                  | _ -> failwith (F.asprintf "Not a function : %a" Ast.pp_e func)
                                end

  | Ast.Lt (lhs, rhs) -> let lhs_interp = interp_e s lhs in
                         let rhs_interp = interp_e s rhs in
                         begin
                          match (lhs_interp, rhs_interp) with
                          | (Store.NumV l_nv, Store.NumV r_nv) -> if l_nv < r_nv then
                                                                      interp_e s (Ast.Fun ("x", (Ast.Fun ("y", (Ast.Id "x")))))
                                                                  else 
                                                                      interp_e s (Ast.Fun ("x", (Ast.Fun ("y", (Ast.Id "y")))))
                          | _ -> failwith (F.asprintf "Invalid less-than : %a < %a" Ast.pp_e lhs Ast.pp_e rhs)
                         end
  
  

(* practice & homework *)
let interp (p : Ast.fvae) : Store.value = 
  match p with
  | Ast.Prog expr -> let store_init = Store.empty in
                     interp_e store_init expr
