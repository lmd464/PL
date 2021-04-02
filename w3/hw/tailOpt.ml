(* 
split : [(a1, b1); .. (an, bn)]  ->  [a1; .. an]  [b1; .. bn] 

1. 리스트를 첫 튜플원소와 나머지 리스트로 분리
2. 첫 튜플원소에 대해 왼쪽과 오른쪽 각각 리스트에 담기
3. 나머지 리스트를 재귀호출 ~ 담은 리스트 포함하여 (Accumulate 원리)
*)
let split list = 
  let rec split_impl list (list_a, list_b) = 
    match list with
    | first_tuple :: others -> ( match first_tuple with
                                 | (left, right) -> 
                                  split_impl others (list_a @ left :: [], 
                                                      list_b @ right :: [])
                                )
    | [] -> (list_a, list_b)
    in
    split_impl list ([], [])



(*
combine : [a1; ... an]  [b1; ... bn]  ->  [(a1, an) ... (b1, bn)]
1. 각 리스트를 첫 원소와 나머지로 분리
2. 첫 원소들을 리스트에 튜플로 묶어서 추가
3. 나머지 리스트를 재귀호출 ~ 담은 리스트 포함하여 (Acc)
*)
let combine list_a list_b = 
  let rec combine_impl list_a list_b integ_list = 
    match (list_a, list_b) with
    | ([], []) -> integ_list
    | (h1::t1, h2::t2) -> combine_impl t1 t2 (integ_list @ ((h1, h2) :: []))
    | _ -> failwith "Unsupport operation"
    in
    combine_impl list_a list_b []

