module F = Format


let rec interp_e (fenv : FEnv.t) (s : Store.t) (e : Ast.expr) : int = 
  match e with
  | Ast.Num int_var -> int_var
  | Ast.Add (expr1, expr2) -> (interp_e fenv s expr1) + (interp_e fenv s expr2)
  | Ast.Sub (expr1, expr2) -> (interp_e fenv s expr1) - (interp_e fenv s expr2)
  | Ast.Id id -> (Store.find id s)
  | Ast.LetIn (id, expr1, expr2) -> let s' = (Store.insert id (interp_e fenv s expr1) s) in
                                    interp_e fenv s' expr2

  (* 함수에 선언된 파라미터, 몸체 정보를 찾아오고 -> 입력한 인자들을 전부 계산하고 -> *)
  (* 함수 선언 내 파라미터를, 계산한 인자로 추상 메모리에 맵핑하고 -> 이를 바탕으로 함수 선언 내 몸체를 계산 *)

  | Ast.FCall (funname, funinput) -> let fun_info = FEnv.find funname fenv in
                                     let rec calculate func_args = 
                                        match func_args with
                                        | head :: res -> (interp_e fenv s head) :: calculate res
                                        | [] -> []
                                     in
                                     let fun_input_results = calculate funinput in
                                     match fun_info with
                                     | (fun_info_args, fun_info_body) -> let arg_store = List.combine fun_info_args fun_input_results in
                                                                         interp_e fenv arg_store fun_info_body  
                                                                                                    


(* 2. fundef의 variant에 따라 연산을 수행 *)
(* 입력받은 FEnv 추상 메모리에 함수 정보를 추가하여 반환 *)
let interp_d (fenv : FEnv.t) (fd : Ast.fundef) = 
  match fd with
  | Ast.FDef (funname, funargs, funexpr) -> FEnv.insert funname funargs funexpr fenv



(* 1. f1vae의 variant에 따라 연산을 수행 *)
(* Store / FEnv 추상 메모리를 만들고, 이를 기반으로 Expression 해석 시작  *)
(* Defenition 리스트를 순회하며 interp_d를 통해 추가 *)
let interp (p : Ast.f1vae) : int = 
  match p with
  | Ast.Prog (fundefs, expr) -> let rec acc fundefs fenv = 
                                  match fundefs with
                                  | fundef :: res -> let fenv_temp = (interp_d fenv fundef) in (* 현재 def 저장 후 *)
                                                     acc res fenv_temp  (* 그 메모리에 대해 재수행 *)
                                  | [] -> fenv
                                in
                                let s = Store.empty in
                                let fenv = acc fundefs FEnv.empty in
                                interp_e fenv s expr 

