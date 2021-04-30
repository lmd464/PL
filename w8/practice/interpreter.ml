module F = Format

(* 3. expr의 variant에 따라 연산을 수행 *)
let rec interp_e (fenv : FEnv.t) (s : Store.t) (e : Ast.expr) : int = 
	match e with
  | Ast.Num int_var -> int_var
  | Ast.Add (expr1, expr2) -> (interp_e fenv s expr1) + (interp_e fenv s expr2)
  | Ast.Sub (expr1, expr2) -> (interp_e fenv s expr1) - (interp_e fenv s expr2)
  | Ast.Id id -> (Store.find id s)
  | Ast.LetIn (id, expr1, expr2) -> let s' = (Store.insert id (interp_e fenv s expr1) s) in
                                    interp_e fenv s' expr2

  (* 함수에 대한 정보를 찾아오고 -> 인자를 계산하고 -> *)
  (* 함수 선언 내 파라미터를, 계산한 인자로 '새 추상 메모리' 에 맵핑하고 -> 이를 바탕으로 함수 선언 내 몸체를 계산 *)
  | Ast.FCall (funname, funinput) -> let fun_info = FEnv.find funname fenv in
                                     let fun_input_result = interp_e fenv s funinput in
                                     match fun_info with
                                     | (fun_info_arg, fun_info_body) -> let s_temp = (Store.insert fun_info_arg fun_input_result Store.empty) in
                                                      interp_e fenv s_temp fun_info_body

  
(* 2. fundef의 variant에 따라 연산을 수행 *)
(* 입력받은 FEnv 추상 메모리에 함수 정보를 추가하여 반환 *)
let interp_d (fd : Ast.fundef) (fenv : FEnv.t) : FEnv.t =
	match fd with
  | Ast.FDef (funname, funarg, funexpr) -> FEnv.insert funname funarg funexpr fenv


(* 1. f1vae의 variant에 따라 연산을 수행 *)
(* Store / FEnv 추상 메모리를 만들고, 이를 기반으로 Expression 해석 시작  *)
let interp (p : Ast.f1vae) : int = 
	match p with
  | Ast.Prog (fundef, expr) -> let s = Store.empty in
                               let fenv = interp_d fundef FEnv.empty in 
                               interp_e fenv s expr 

