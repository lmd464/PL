module F = Format

(* practice & homework *)
let rec interp_e (e : Ast.expr) (s : Store.t) : Store.value = 
  match e with
  | Ast.Num int_var -> Store.NumV int_var
  | Ast.Var var -> (Store.find var s)
  | Ast.Bool bool_var -> Store.BoolV bool_var

  (* 대상 : NumV *)
  | Ast.Add (expr1, expr2) -> let expr1_exam = interp_e expr1 s in
                              let expr2_exam = interp_e expr2 s in
                              begin
                                match (expr1_exam, expr2_exam) with
                                | (Store.NumV e1_val, Store.NumV e2_val) -> NumV (e1_val + e2_val)
                                | _ -> failwith (F.asprintf "Invalid addition : %a + %a" Ast.pp_e expr1 Ast.pp_e expr2)
                              end         
  | Ast.Sub (expr1, expr2) -> let expr1_exam = interp_e expr1 s in
                              let expr2_exam = interp_e expr2 s in
                              begin
                                match (expr1_exam, expr2_exam) with
                                | (Store.NumV e1_val, Store.NumV e2_val) -> NumV (e1_val - e2_val)
                                | _ -> failwith (F.asprintf "Invalid subtraction : %a - %a" Ast.pp_e expr1 Ast.pp_e expr2)
                              end
  
  (* 대상 : NumV *)
  | Ast.Lt (lhs, rhs) -> let lhs_interp = interp_e lhs s in
                         let rhs_interp = interp_e rhs s in
                         begin
                          match (lhs_interp, rhs_interp) with
                          | (Store.NumV l_n, Store.NumV r_n) -> if l_n < r_n then
                                                                      interp_e (Ast.Bool true) s
                                                                  else 
                                                                      interp_e (Ast.Bool false) s
                                                                      
                          | _ -> failwith (F.asprintf "Invalid less-than : %a < %a" Ast.pp_e lhs Ast.pp_e rhs)
                         end

  | Ast.Gt (lhs, rhs) -> let lhs_interp = interp_e lhs s in
                         let rhs_interp = interp_e rhs s in
                         begin
                          match (lhs_interp, rhs_interp) with
                          | (Store.NumV l_n, Store.NumV r_n) -> if l_n > r_n then
                                                                      interp_e (Ast.Bool true) s
                                                                  else 
                                                                      interp_e (Ast.Bool false) s
                                                                      
                          | _ -> failwith (F.asprintf "Invalid greater-than : %a > %a" Ast.pp_e lhs Ast.pp_e rhs)
                         end


  (* 대상 : NumV, BoolV *)        
  | Ast.Eq (lhs, rhs) -> let lhs_interp = interp_e lhs s in
                         let rhs_interp = interp_e rhs s in
                         begin
                          match (lhs_interp, rhs_interp) with
                          | (Store.NumV l_n, Store.NumV r_n) -> if l_n = r_n then
                                                                      interp_e (Ast.Bool true) s
                                                                  else 
                                                                      interp_e (Ast.Bool false) s
                          | (Store.BoolV l_b, Store.BoolV r_b) -> if l_b = r_b then
                                                                      interp_e (Ast.Bool true) s
                                                                  else 
                                                                      interp_e (Ast.Bool false) s
                          | _ -> failwith (F.asprintf "Invalid equal-to : %a = %a" Ast.pp_e lhs Ast.pp_e rhs)
                         end
  
  (* 대상 : BoolV *)
  | Ast.And (lhs, rhs) -> let lhs_interp = interp_e lhs s in
                         let rhs_interp = interp_e rhs s in
                         begin
                          match (lhs_interp, rhs_interp) with
                          | (Store.BoolV l_b, Store.BoolV r_b) -> let result = l_b && r_b in
                                                                  interp_e (Ast.Bool result) s
                                                                  
                                                                      
                          | _ -> failwith (F.asprintf "Invalid logical-and : %a && %a" Ast.pp_e lhs Ast.pp_e rhs)
                         end

  | Ast.Or (lhs, rhs) -> let lhs_interp = interp_e lhs s in
                         let rhs_interp = interp_e rhs s in
                         begin
                          match (lhs_interp, rhs_interp) with
                          | (Store.BoolV l_b, Store.BoolV r_b) -> let result = l_b || r_b in
                                                                  interp_e (Ast.Bool result) s
                                                                  
                                                                      
                          | _ -> failwith (F.asprintf "Invalid logical-or : %a || %a" Ast.pp_e lhs Ast.pp_e rhs)
                         end
  

(* practice & homework *)
let rec interp_s (stmt : Ast.stmt) (s : Store.t) : Store.t = 
  match stmt with
  | Ast.AssignStmt (var_name, var_expr) -> Store.insert var_name (interp_e var_expr s) s

  | Ast.IfStmt (cond, stmt_list_true, stmt_list_false) -> let cond_interp = interp_e cond s in
                                         begin
                                          match cond_interp with
                                          | Store.BoolV condition -> let rec iter stmt_list_temp store_temp = 
                                                                      begin
                                                                        match stmt_list_temp with
                                                                        | head :: res -> let s' = interp_s head store_temp in
                                                                                         iter res s'
                                                                        | [] -> store_temp
                                                                      end
                                                                     in
                                                                     if condition = true then
                                                                        iter stmt_list_true s
                                                                     else
                                                                      begin
                                                                        match stmt_list_false with
                                                                        | None -> s
                                                                        | Some stmt_list_false_t -> iter stmt_list_false_t s
                                                                     end
                                          
                                          | _ -> failwith (F.asprintf "Not a bool value : %a" Ast.pp_e cond)
                                         end

(* practice & homework *)
let interp (p : Ast.program) : Store.t = 
  match p with
  | Ast.Program stmt_list -> let store_init = Store.empty in
                             let rec iter stmt_list_temp store_temp = begin
                                                                        match stmt_list_temp with
                                                                        | head :: res -> let s' = interp_s head store_temp in
                                                                                         iter res s'
                                                                        | [] -> store_temp
                                                                      end
                             in
                             iter stmt_list store_init

