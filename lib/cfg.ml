open Yojson.Basic.Util

(** Reads a JSON file from [stdin] *)
let load_json () : Yojson.Basic.t = 
  Yojson.Basic.from_channel stdin


(** Infix operator for looking up a key in a JSON object, 
    where [json $? key] means [json["key"]] *)
let ($?) (json : Yojson.Basic.t) (key : string) : Yojson.Basic.t = 
  member key json   

let () = 
  let json = load_json () in 
  let functions = json $? "functions" in 
  failwith "TODO"

