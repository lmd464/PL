module F = Format

(* 함수 인자가 여러개 *)
type t = (string * (string list * Ast.expr)) list

let empty = []

let rec insert x plist body s = 
  match s with
  | (funname, (funargs, funbody)) :: res -> (if funname = x then (x, (plist, body)) :: res
                                            else (funname, (funargs, funbody)) :: insert x plist body res)
  | [] -> s @ ((x, (plist, body)) :: [])

let rec find x s = 
  match s with 
  | (funname, funresult) :: res -> (if funname = x then funresult
                                    else find x res)
  | [] -> failwith ("Free identifier " ^ x)



let pp fmt s = 
  let rec pp_impl fmt s = 
    match s with
    | [] -> F.fprintf fmt "]"
    | (x, (p, e)) :: t -> F.fprintf fmt "(%s, (%s, %a)) %a" x p Ast.pp_e e pp_impl t
  in
  F.fprintf fmt "[ %a" pp_impl s
