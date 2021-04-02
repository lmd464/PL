module F = Format 

module MyList = struct
  type intlist = IntNil | IntCons of int * intlist
  type floatlist = FloatNil | FloatCons of float * floatlist



  (*
  일반적인 경우를 먼저 생각하고, 재귀호출 인자를 고민한 후
  마지막에 탈출케이스 
  *)

  (* practice 1 *)
  let rec map_int_float f il = 
    match il with 
    | IntCons (head, tail) -> FloatCons ( (f head), (map_int_float f tail) )
    | IntNil -> FloatNil


  (* practice 2 *)
  let rec append_int il1 il2 = 
    match il1 with
    | IntCons (head, tail) -> IntCons ( head, (append_int tail il2) )
    | IntNil -> il2


    
  let rec print_int_list fmt il = 
    match il with
    | IntNil -> F.fprintf fmt "IntNil"
    | IntCons (h, t) -> 
        F.fprintf fmt "IntCons(%d, %a)" h print_int_list t


  let rec print_float_list fmt fl = 
    match fl with
    | FloatNil -> F.fprintf fmt "FloatNil"
    | FloatCons (h, t) -> 
        F.fprintf fmt "FloatCons(%f, %a)" h print_float_list t
end




(* test cases *)
let _ = 
  let open MyList in

  let a = map_int_float float_of_int IntNil in  (* a = FloatNil *)
  let _ = F.printf "a = %a\n" print_float_list a in

  let b = map_int_float float_of_int (IntCons (3, IntNil)) in  (* b = FloatCons (3.0, FloatNil) *)
  let _ = F.printf "b = %a\n" print_float_list b in

  let c = map_int_float (fun x -> 1.0 +. (float_of_int x)) (IntCons (1, IntCons (2, IntCons (3, IntNil)))) in  (* b = FloatCons (2.0, FloatCons (3.0, FloatCons (4.0, FloatNil))) *)
  let _ = F.printf "c = %a\n" print_float_list c in


  let d = append_int IntNil IntNil in (* d = IntNil *)
  let _ = F.printf "d = %a\n" print_int_list d in

  let e = append_int (IntCons (1, (IntCons (2, IntNil)))) (IntCons (3, (IntCons (4, IntNil)))) in (* e = IntCons (1, (IntCons (2, IntCons (3, IntCons (4, IntNil))))) *)
  let _ = F.printf "e = %a\n" print_int_list e in

  let f = append_int (IntCons (1, IntNil)) (IntCons (2, IntCons (3, (IntCons (4, IntNil))))) in (* e = IntCons (1, (IntCons (2, IntCons (3, IntCons (4, IntNil))))) *)
  F.printf "f = %a\n" print_int_list f

