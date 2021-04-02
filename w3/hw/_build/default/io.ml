(* IO functions for test case *)

(* HW1 ~ split test *)
 let print_split_list splitted_lists = 
   match splitted_lists with
   | a_list, b_list -> let _ = Format.printf "[" in
                       let _ = List.iter(fun x -> Format.printf " %c " x) a_list in
                       let _ = Format.printf "] \n" in
                     
                       let _ = Format.printf "[" in
                       let _ = List.iter(fun y -> Format.printf " %c " y) b_list in
                       Format.printf "] \n"



 (* HW2 ~ combine test *)
 let rec print_integ_list integ_list_input = 
    match integ_list_input with
    | first_tuple :: others -> (match first_tuple with
                                | (left, right) -> let _ = Format.printf "( %c, " left in
                                                   let _ = Format.printf " %c ) " right in
                                                   print_integ_list others
                               )
    | [] -> Format.printf "\n"

