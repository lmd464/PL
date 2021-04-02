module Unary = struct 

  type unary_number = Z | S of unary_number

  let rec add n m = 
    match n with 
    | S head tail -> S head (add head m)
    | Z -> m

  let rec count_func input_unary count = 
    match input_unary with
    | S exp -> count_func exp count + 1
    | Z -> count

end




(* test *)
let _ = 
  let open Unary in
  let test1 = add (S (S (S (S (S Z))))) (S Z) in
  let testprint1 = count_func test1 0 in
  Format.printf "count : %d \n" testprint1


