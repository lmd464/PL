let _ = 
  let open TailOpt in
  let open Io in


  (* HW1 ~ split test *)
  let splitted_lists = split [('a', 'x'); ('b', 'y'); ('c', 'z')] in
  let _ = print_split_list splitted_lists in

  (* HW2 ~ combine test *)
  let integ_list = combine ['0'; '1'; '2'] ['a'; 'b'; 'c'] in
  print_integ_list integ_list

  