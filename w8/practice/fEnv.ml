module F = Format

type t = (string * (string * Ast.expr)) list

let empty = []

let rec insert x param body s = 
  match s with
  | (funname, (funarg, funbody)) :: res -> (if funname = x then (x, (param, body)) :: res
                                          else (funname, (funarg, funbody)) :: insert x param body res)
  | [] -> s @ ((x, (param, body)) :: [])


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
