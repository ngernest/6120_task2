open Yojson.Basic.Util

(** Reads a JSON file from [stdin] *)
let load_json () : Yojson.Basic.t = 
  Yojson.Basic.from_channel stdin


(** Infix operator for looking up a key in a JSON object, 
    where [json $? key] means [json["key"]] *)
let ($?) (json : Yojson.Basic.t) (key : string) : Yojson.Basic.t = 
  member key json   

(** Determines if a JSON object contains a key *)  
let contains_key (json : Yojson.Basic.t) (key : string) : bool = 
  match json $? key with 
  | `Null -> false 
  | _ -> true   
  
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

