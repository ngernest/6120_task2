open Utils
  
(* let form_blocks body = 
  List.fold_left (fun acc) [] body *)

(** Creates labels for a list of blocks 
    (generating fresh labels when necessary) *)  
(* let mk_block_map blocks = 
  List.map (fun block -> 
    if contains_key block 
    
    
    ) blocks  *)

let () = 
  let json = load_json () in 
  let functions = json $? "functions" in 
  failwith "TODO"

