module F = Format

type t = (string * int) list

let empty = []

let rec insert x n s =
  match s with
  | (k,v) :: res -> (if k = x then (x, n) :: res
                     else (k,v) :: insert x n res)
  | [] -> s @ ((x, n) :: [])


let rec find x s = 
  match s with
  | (k,v) :: res -> (if k = x then v
                     else find x res)
  | [] -> failwith ("Free identifier " ^ x)


let pp fmt s = 
  let rec pp_impl fmt s = 
    match s with
    | [] -> F.fprintf fmt "]"
    | (x, n) :: t -> F.fprintf fmt "(%s, %d) %a" x n pp_impl t
  in
  F.fprintf fmt "[ %a" pp_impl s
