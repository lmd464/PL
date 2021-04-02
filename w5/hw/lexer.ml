module F = Format

type t = 
  | Int of int
  | Var of string

type state = 
  | S0
  | S1
  | S2  (* 종료상태 ~ Integer *)
  | S3  (* 종료상태 ~ Var *)

let char_to_int c = (int_of_char c) - 48 
let char_to_str c = Char.escaped c

(* lex : char list -> t *)
let lex chars = 
  let rec lex_impl state chars int_acc var_acc = 
    match state with
    | S0 -> 
          begin
          match chars with
          | h :: t ->
                begin
                match h with 
                | '0' .. '9' -> lex_impl S2 t (int_acc * 10 + (char_to_int h)) var_acc

                | '-' -> let res = lex_impl S1 t int_acc var_acc in
                         ( match res with
                          | Int r -> Int (-1 * r) 
                          | _ -> failwith "Not a valid natural number or a valid variable" )

                | 'a'..'z' -> lex_impl S3 t int_acc (var_acc ^ (char_to_str h))

                | _ -> failwith "Not a valid natural number or a valid variable"
                end
          | [] -> failwith "Not a valid natural number or a valid variable"
          end

    | S1 -> 
          begin
          match chars with
          | h :: t ->
                begin
                match h with 
                | '0' .. '9' -> lex_impl S2 t (int_acc * 10 + (char_to_int h)) var_acc
                | _ -> failwith "Not a valid natural number or a valid variable"
                end
          | [] -> failwith "Not a valid natural number or a valid variable"
          end

    | S2 ->
          begin
          match chars with
          | h :: t ->
                begin 
                match h with 
                | '0' .. '9' -> lex_impl S2 t (int_acc * 10 + (char_to_int h)) var_acc
                | _ -> failwith "Not a valid natural number or a valid variable"
                end
          | [] -> Int int_acc   (* 종료 상태 *)
          end

    | S3 -> 
          begin 
          match chars with
          | h :: t ->
                begin
                match h with
                | 'a' .. 'z'
                | 'A' .. 'Z'
                | '0' .. '9'
                | '\''
                | '_' -> lex_impl S3 t int_acc (var_acc ^ (char_to_str h))
                | _ -> failwith "Not a valid natural number or a valid variable"
                end
          | [] -> Var var_acc
          end
    in
    lex_impl S0 chars 0 ""


let pp fmt v = 
  match v with
  | Int i -> F.fprintf fmt "Int %d" i
  | Var x -> F.fprintf fmt "Var %s" x 
