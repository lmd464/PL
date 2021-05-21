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

  (* 얼린건지 확인하고 녹이기 필요 - FreezedV 매칭 *)
  | Ast.Id id -> let found_ident = Store.find id s in
                 begin
                  match found_ident with
                  | Store.FreezedV (frozen_func_arg, frozen_func_store) -> interp_e frozen_func_store frozen_func_arg
                  | _ -> found_ident
                 end


  | Ast.LetIn (id, expr1, expr2) -> let s' = (Store.insert id (interp_e s expr1) s) in
                                    interp_e s' expr2
  
  | Ast.RLetIn (x, e1, e2) -> begin
                                match interp_e s e1 with
                                | Store.ClosureV (x', e, s') -> let rec s'' = (x, (Store.ClosureV (x', e, s''))) :: s' in
                                                                interp_e s'' e2
                                | _ -> failwith (F.asprintf "Not a function : %a" Ast.pp_e e1)
                              end


  | Ast.Fun (func_arg_name, func_body) -> Store.ClosureV (func_arg_name, func_body, s)

  (* 함수의 Store에 인자를 넣을 때, interp해서 넣지 않고 - 해당 시점의 Store와 함께 그대로 FreezedV 형태로 넣는다. *)
  (* 이는 나중에 Id에서 다시 분리되어, interp 되어 계산된다. *)
  | Ast.App (func, func_arg) -> let func_closure = interp_e s func in
                                begin
                                  match func_closure with
                                  | Store.ClosureV (func_arg_name, func_body, func_store) -> let freezed_arg = Store.FreezedV (func_arg, s) in
                                                                                             (* pair = (func_arg_name, interpreted_arg) *)
                                                                                             (* 함수 시점의 store에, 계산한 인자를 저장 *)
                                                                                             let new_func_store = Store.insert func_arg_name freezed_arg func_store in  
                                                                                             interp_e new_func_store func_body
                                                                                             
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
