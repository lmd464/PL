module F = Format

type t = Int of int

type state = 
  | S0
  | S1
  | S2

let char_to_int c = (int_of_char c) - 48 

(* lex : char list -> t *)
let lex chars = 
  let rec lex_impl state chars v = 
    match state with
    | S0 -> 
          begin
              match chars with
              | h :: t ->
                begin
                    match h with 
                    | '0' .. '9' -> lex_impl S2 t (v * 10 + (char_to_int h))
                    | '-' -> let res = lex_impl S1 t 0 in
                                ( match res with
                                | Int r -> Int (-1 * r) )
                                
                    | _ -> failwith "Not a valid natural number"
                end
              | [] -> failwith "Not a valid natural number"
          end

    | S1 -> 
          begin
              match chars with
              | h :: t ->
                begin
                   match h with 
                    | '0' .. '9' -> lex_impl S2 t (v * 10 + (char_to_int h))
                    | _ -> failwith "Not a valid natural number"
                 end
              | [] -> failwith "Not a valid natural number"
           end

    | S2 ->
          begin
              match chars with
              | h :: t ->
                begin 
                  match h with 
                    | '0' .. '9' -> lex_impl S2 t (v * 10 + (char_to_int h))
                    | _ -> failwith "Not a valid natural number"
                end
              | [] -> Int v   (* 종료 상태 *)
          end


    in
    lex_impl S0 chars 0

      

let pp fmt v = 
  match v with
  | Int i -> F.fprintf fmt "Int %d" i
