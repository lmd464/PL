module F = Format

type t = int list

let empty = []


(* Practice1 *)
let push elem stack = 
  elem :: stack


(* Practice2 *)
(* stack의 형태에 따라 match ~ 비었을 때, 원소가 있을 때 *)
let pop stack = 
  match stack with
  | h::t -> (h, t)
  | [] -> failwith "Stack is empty"


  

let print_stack fmt stack = 
  let rec print_stack_impl stack = 
    match stack with
    | [] -> ()
    | h :: t -> 
        let () = F.fprintf fmt "%d " h in
        print_stack_impl t
  in
  let () = F.fprintf fmt "[ " in
  let () = print_stack_impl stack in
  F.fprintf fmt "]"
