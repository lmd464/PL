module F = Format

(* practice & homework *)
let rec interp_e (e : Ast.expr) ((env, mem) : Env.t * Mem.t) : Mem.value = 
  match e with
  | Ast.Num int_var -> Mem.NumV int_var
  | Ast.Bool bool_var -> Mem.BoolV bool_var

  | Ast.Var var_name -> Mem.find (Env.find var_name env) mem (* 변수명 -> 주소 -> 값 *)
  | Ast.Ref var_name -> Mem.AddressV (Env.find var_name env) (* 변수명 -> 주소 *)

  | Ast.Deref var_name -> let var_addr = Env.find var_name env in (* 변수가 존재하고 *)
                          let var_val = Mem.find var_addr mem in  (* 변수값이 존재하며 *)
                          begin
                            match var_val with
                            | Mem.AddressV addr -> Mem.find addr mem  (* 변수값이 주소 *)
                            | _ -> failwith (F.asprintf "Not a memory address : %a" Ast.pp_e e)
                          end

  (* 대상 : NumV *)
  | Ast.Add (expr1, expr2) -> let expr1_exam = interp_e expr1 (env, mem) in
                              let expr2_exam = interp_e expr2 (env, mem) in
                              begin
                                match (expr1_exam, expr2_exam) with
                                | (Mem.NumV e1_val, Mem.NumV e2_val) -> NumV (e1_val + e2_val)
                                | _ -> failwith (F.asprintf "Invalid addition : %a + %a" Ast.pp_e expr1 Ast.pp_e expr2)
                              end         
  | Ast.Sub (expr1, expr2) -> let expr1_exam = interp_e expr1 (env, mem) in
                              let expr2_exam = interp_e expr2 (env, mem) in
                              begin
                                match (expr1_exam, expr2_exam) with
                                | (Mem.NumV e1_val, Mem.NumV e2_val) -> NumV (e1_val - e2_val)
                                | _ -> failwith (F.asprintf "Invalid subtraction : %a - %a" Ast.pp_e expr1 Ast.pp_e expr2)
                              end
  
  (* 대상 : NumV *)
  | Ast.Lt (lhs, rhs) -> let lhs_interp = interp_e lhs (env, mem) in
                         let rhs_interp = interp_e rhs (env, mem) in
                         begin
                          match (lhs_interp, rhs_interp) with
                          | (Mem.NumV l_n, Mem.NumV r_n) -> if l_n < r_n then
                                                                      interp_e (Ast.Bool true) (env, mem)
                                                                  else 
                                                                      interp_e (Ast.Bool false) (env, mem)
                                                                      
                          | _ -> failwith (F.asprintf "Invalid less-than : %a < %a" Ast.pp_e lhs Ast.pp_e rhs)
                         end

  | Ast.Gt (lhs, rhs) -> let lhs_interp = interp_e lhs (env, mem) in
                         let rhs_interp = interp_e rhs (env, mem) in
                         begin
                          match (lhs_interp, rhs_interp) with
                          | (Mem.NumV l_n, Mem.NumV r_n) -> if l_n > r_n then
                                                                      interp_e (Ast.Bool true) (env, mem)
                                                                  else 
                                                                      interp_e (Ast.Bool false) (env, mem)
                                                                      
                          | _ -> failwith (F.asprintf "Invalid greater-than : %a > %a" Ast.pp_e lhs Ast.pp_e rhs)
                         end


  (* 대상 : NumV, BoolV *)        
  | Ast.Eq (lhs, rhs) -> let lhs_interp = interp_e lhs (env, mem) in
                         let rhs_interp = interp_e rhs (env, mem) in
                         begin
                          match (lhs_interp, rhs_interp) with
                          | (Mem.NumV l_n, Mem.NumV r_n) -> if l_n = r_n then
                                                                      interp_e (Ast.Bool true) (env, mem)
                                                                  else 
                                                                      interp_e (Ast.Bool false) (env, mem)
                          | (Mem.BoolV l_b, Mem.BoolV r_b) -> if l_b = r_b then
                                                                      interp_e (Ast.Bool true) (env, mem)
                                                                  else 
                                                                      interp_e (Ast.Bool false) (env, mem)

                          | _ -> failwith (F.asprintf "Invalid equal-to : %a = %a" Ast.pp_e lhs Ast.pp_e rhs)
                         end
  
  (* 대상 : BoolV *)
  | Ast.And (lhs, rhs) -> let lhs_interp = interp_e lhs (env, mem) in
                         let rhs_interp = interp_e rhs (env, mem) in
                         begin
                          match (lhs_interp, rhs_interp) with
                          | (Mem.BoolV l_b, Mem.BoolV r_b) -> let result = l_b && r_b in
                                                                  interp_e (Ast.Bool result) (env, mem)   

                          | _ -> failwith (F.asprintf "Invalid logical-and : %a && %a" Ast.pp_e lhs Ast.pp_e rhs)
                         end

  | Ast.Or (lhs, rhs) -> let lhs_interp = interp_e lhs (env, mem) in
                         let rhs_interp = interp_e rhs (env, mem) in
                         begin
                          match (lhs_interp, rhs_interp) with
                          | (Mem.BoolV l_b, Mem. BoolV r_b) -> let result = l_b || r_b in
                                                                  interp_e (Ast.Bool result) (env, mem)

                          | _ -> failwith (F.asprintf "Invalid logical-or : %a || %a" Ast.pp_e lhs Ast.pp_e rhs)
                         end
  

(* practice & homework *)
let rec interp_s (stmt : Ast.stmt) ((env, mem) : Env.t * Mem.t) : Env.t * Mem.t = 
  match stmt with

  (* 변수명과 주소 연결 *)
  | Ast.VarDeclStmt var_name -> let existence = Env.mem var_name env in 
                                let new_addr = Env.new_address () in
                                if existence = false then (Env.insert var_name new_addr env), mem
                                else failwith (F.asprintf "%s is already declared." var_name)
  
  (* 주소와 값 연결 *)
  | Ast.StoreStmt (e1, e2) -> let addr_interp = interp_e e1 (env, mem) in
                              let val_interp = interp_e e2 (env, mem) in
                              begin
                                match (addr_interp, val_interp) with
                                | (Mem.AddressV addr, v) -> (env, (Mem.insert addr v mem))
                                | _ -> failwith (F.asprintf "Not a memory address : %a" Ast.pp_s stmt)
                              end


  | Ast.IfStmt (cond, stmt_list_true, stmt_list_false) -> let cond_interp = interp_e cond (env, mem) in
                                         begin
                                          match cond_interp with
                                          | Mem.BoolV condition -> let rec iter stmt_list_param env_param mem_param = 
                                                                      begin
                                                                        match stmt_list_param with
                                                                        | head :: res -> let (env', mem') = interp_s head (env_param, mem_param) in
                                                                                         iter res env' mem'
                                                                        | [] -> env_param, mem_param
                                                                      end
                                                                   in
                                                                   if condition = true then
                                                                      iter stmt_list_true env mem
                                                                   else
                                                                      begin
                                                                        match stmt_list_false with
                                                                        | None -> (env, mem)
                                                                        | Some stmt_list_false_t -> iter stmt_list_false_t env mem
                                                                      end
                                          
                                          | _ -> failwith (F.asprintf "Not a boolean : %a" Ast.pp_e cond)
                                         end

  | Ast.WhileStmt (cond, stmt_list) -> let cond_interp = interp_e cond (env, mem) in
                                        begin
                                          match cond_interp with
                                          | Mem.BoolV condition -> let rec iter stmt_list_param env_param mem_param = 
                                                                      begin
                                                                        match stmt_list_param with
                                                                        | head :: res -> let (env', mem') = interp_s head (env_param, mem_param) in
                                                                                         iter res env' mem'
                                                                        | [] -> env_param, mem_param
                                                                      end
                                                                   in
                                                                     if condition = true then
                                                                        let env_t, mem_t = (iter stmt_list env mem) in
                                                                        interp_s (Ast.WhileStmt (cond, stmt_list)) (env_t, mem_t)
                                                                     else
                                                                        (env, mem)

                                          | _ -> failwith (F.asprintf "Not a boolean : %a" Ast.pp_e cond)
                                        end

(* practice & homework *)
let interp (p : Ast.program) : Env.t * Mem.t = 
  match p with
  | Ast.Program stmt_list -> let env_init = Env.empty in
                             let mem_init = Mem.empty in
                             let rec iter stmt_list_param env_param mem_param = 
                                                      begin
                                                        match stmt_list_param with
                                                        | head :: res -> let (env', mem') = interp_s head (env_param, mem_param) in
                                                                         iter res env' mem'
                                                        | [] -> env_param, mem_param
                                                      end
                             in
                             iter stmt_list env_init mem_init

