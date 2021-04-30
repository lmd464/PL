module F = Format

type expr = 
  | Num of int
  | Add of expr * expr  
  | Sub of expr * expr
  | Id of string
  | LetIn of string * expr * expr

  (* 호출 ~ 인자가 여러개 : expr list *)
  | FCall of string * expr list

(* 선언 ~ 인자가 여러개 : string list *)
type fundef = 
  | FDef of string * string list * expr

(* 함수 정의가 여러개 : fundef list *)
type f1vae =
  | Prog of fundef list * expr


let rec pp_e fmt e = 
  match e with
  | Num n -> F.fprintf fmt "(Num %d)" n 
  | Add (e1, e2) -> F.fprintf fmt "(Add %a %a)" pp_e e1 pp_e e2
  | Sub (e1, e2) -> F.fprintf fmt "(Sub %a %a)" pp_e e1 pp_e e2
  | Id x -> F.fprintf fmt "(Id %s)" x
  | LetIn (x, e1, e2) -> F.fprintf fmt "(LetIn %s %a %a)" x pp_e e1 pp_e e2
  | FCall (f, elist) -> F.fprintf fmt "(FCall %s%a)" f (fun fmt y -> List.iter (fun x -> F.fprintf fmt " %a" pp_e x) y) elist

let pp_fd fmt (FDef (f, plist, e)) = 
  F.fprintf fmt "(FDef %s%s %a)" f (List.fold_left (fun i x -> i ^ " " ^ x) "" plist) pp_e e 

let pp fmt (Prog (fdlist, e)) = 
  F.fprintf fmt "(Prog%a %a)" (fun fmt y -> List.iter (fun x -> F.fprintf fmt " %a" pp_fd x) y) fdlist pp_e e
  
