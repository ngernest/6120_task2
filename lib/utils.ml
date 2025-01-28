open Yojson.Basic.Util

(** Reads a JSON file from [stdin] *)
let load_json () : Yojson.Basic.t = Yojson.Basic.from_channel stdin

(** Infix operator for looking up a key in a JSON object, 
    where [json $! key] means [json["key"]] 
    - This returns [`Null] if [json] doesn't contain [key] *)
let ( $! ) (json : Yojson.Basic.t) (key : string) : Yojson.Basic.t =
  member key json

(** [json $? key] returns [true] if the [json] object contains the key,
    and false otherwise *)
let ( $? ) (json : Yojson.Basic.t) (key : string) : bool =
  match json $! key with
  | `Null -> false
  | _ -> true
