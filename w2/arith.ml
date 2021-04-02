type unop = Neg
type binop = Add | Sub | Mul | Div
type exp = 
            | Constant  of int 
            | Unary     of unop * exp
            | Binary    of exp * binop * exp
            
(* 
<< eval : 정수형 결과를 반환하는 함수 >>
1. exp 형태의 입력을 받아온다. 받아온 expression 의 형태에 따라 동작을 결정해야 함
2. (Pattern Matching) expression 안에서 unary / binary / const 분리
3. unary / binary 안에서 operator 분리
*)

let rec eval exp_input = 
  match exp_input with
  | Binary (exp_left, binary_operator, exp_right) -> 
        ( match binary_operator with
        | Add -> (eval exp_left) + (eval exp_right)
        | Sub -> (eval exp_left) - (eval exp_right)
        | Mul -> (eval exp_left) * (eval exp_right)
        | Div -> (eval exp_left) / (eval exp_right) )

  | Unary (unary_operator, exp) -> 
        ( match unary_operator with
        | Neg -> 0 - (eval exp) )

  | Constant integer -> integer

  (* let b = eval (Binary (Constant 5, Add, (Unary (Neg, (Constant 3)))))  (* b = 2 *) *)