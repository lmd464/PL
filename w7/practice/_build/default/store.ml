module F = Format

type t = (string * int) list

let empty = []

let rec insert x n s = 
  match s with
  | (k,v) :: res -> (if k = x then (x, n) :: res
                     else (k,v) :: insert x n res)
  | [] -> s @ ((x, n) :: [])


let rec find x (s:t) = 
  match s with
  | (k,v) :: res -> (if k = x then v
                     else find x res)
  | [] -> failwith ("Free identifier " ^ x)
