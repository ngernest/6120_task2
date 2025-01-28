open Yojson.Basic.Util

(** Reads a JSON file from [stdin] *)
let load_json () : Yojson.Basic.t = Yojson.Basic.from_channel stdin

(** Infix operator for looking up a key in a JSON object, 
    where [json $? key] means [json["key"]] *)
let ( $? ) (json : Yojson.Basic.t) (key : string) : Yojson.Basic.t =
  member key json

(** Determines if a JSON object contains a key *)
let contains_key (json : Yojson.Basic.t) (key : string) : bool =
  match json $? key with
  | `Null -> false
  | _ -> true
