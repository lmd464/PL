module F = Format

type t = (string * string) list 

let empty = []

let rec add key value map = 
  match map with
  | (k,v) :: res -> (if k = key then (key, value) :: res
                     else (k,v) :: add key value res)
  | [] -> map @ ((key, value) :: [])

let rec find key map = 
  match map with
  | (k,v) :: res -> if k = key then v
                    else find key res
  | [] -> failwith "No such key exists"

let rec erase key map = 
  match map with
  | (k,v) :: res -> if k = key then res
                    else (k,v) :: (erase key res)
  | [] -> failwith "No such key exists"





let print_map fmt map = 
  let rec print_map_impl map = 
    match map with
    | [] -> ()
    | (k, v) :: t -> 
        let () = F.fprintf fmt "(%s, %s) " k v in
        print_map_impl t
  in
  let () = F.fprintf fmt "[ " in
  let () = print_map_impl map in
  F.fprintf fmt "]"
