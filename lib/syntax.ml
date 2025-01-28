open Helpers
open StdLabels

(** All [label]s are just strings *)
type label = string

(** All arguments are strings *)
type arg = string

(** Primitive types (int or bools) *)
type ty =
  | TyInt
  | TyBool

(** Converts a string to a [ty] *)
let ty_of_string (str : string) : ty =
  match str with
  | "int" -> TyInt
  | "bool" -> TyBool
  | _ -> failwith (spf "invalid string: %s\n" str)

(** Literal values (int & bool values) *)
type literal =
  | LitInt of int
  | LitBool of bool

(** A {i destination variable} is a pair consisting of 
    the variable name & the variable's type *)
type dest = string * ty

(* -------------------------------------------------------------------------- *)
(*                              Binary Operators                              *)
(* -------------------------------------------------------------------------- *)

(** Binary operators *)
type binop =
  | Add
  | Mul
  | Sub
  | Div
  | Eq
  | Lt
  | Gt
  | Le
  | Ge
  | And
  | Or

let binop_opcode_map : (string * binop) list =
  [
    ("add", Add);
    ("mul", Mul);
    ("sub", Sub);
    ("div", Div);
    ("eq", Eq);
    ("lt", Lt);
    ("gt", Gt);
    ("le", Le);
    ("ge", Ge);
    ("and", And);
    ("or", Or);
  ]

(** Determines if an opcode represents a binary operator *)
let is_binop (opcode : string) : bool =
  let binop_opcodes = List.map ~f:fst binop_opcode_map in
  List.mem opcode ~set:binop_opcodes

(** Converts a string to a [binop] *)
let binop_of_string (opcode : string) : binop =
  List.assoc opcode binop_opcode_map

(* -------------------------------------------------------------------------- *)
(*                               Unary operators                              *)
(* -------------------------------------------------------------------------- *)

(** Unary operators *)
type unop =
  | Not
  | Id

(** Maps each unary operator's opcode to the corresponding [unop] *)
let unop_opcode_map : (string * unop) list = [ ("not", Not); ("id", Id) ]

(** Determines if an opcode represents a binary operator *)
let is_unop (opcode : string) : bool =
  let unop_opcodes = List.map ~f:fst unop_opcode_map in
  List.mem opcode ~set:unop_opcodes

(** Converts a string to an [unop] *)
let unop_of_string (opcode : string) : unop = List.assoc opcode unop_opcode_map

(* -------------------------------------------------------------------------- *)
(*                                Instructions                                *)
(* -------------------------------------------------------------------------- *)

(** The type of Bril instructions *)
type instr =
  | Label of label
  | Const of dest * literal
  | Binop of dest * binop * arg * arg
  | Unop of dest * unop * arg
  | Jmp of label
  | Br of arg * label * label
  | Ret of arg option
  | Print of arg list
  | Nop
  | Call of dest * string * arg list

(** The type of a Bril function: 
{[
  {
    "name": "<string>",
    "args": [{"name": "<string>", "type": <Type>}, ...]?,
    "type": <Type>?,
    "instrs": [<Instruction>, ...]
  } 
]} *)
type func = {
  name : string;
  args : (string * ty) list;
  ret_type : ty option;
  instrs : (string * instr list) list;
}

(******************************************************************************)
open Yojson.Basic.Util

(** Retrieves the contents of the ["args"] field in a JSON object 
    as a list of strings  *)
let get_args (json : Yojson.Basic.t) : arg list =
  let args_list = Helpers.list_of_json (json $! "args") in
  List.map ~f:to_string args_list

(** Retrieves the contents of the ["fields"] field in a JSON object 
    as a list of strings  *)
let get_labels (json : Yojson.Basic.t) : arg list =
  let labels_list = Helpers.list_of_json (json $! "labels") in
  List.map ~f:to_string labels_list

(** Retrieves the name and type of the destination variable
    from a JSON object *)
let get_dest (json : Yojson.Basic.t) : dest =
  let dest_string = to_string (json $! "dest") in
  let ty = ty_of_string @@ to_string (json $! "type") in
  (dest_string, ty)

(** Converts a JSON object to an [instr] *)
let instr_of_json (json : Yojson.Basic.t) : instr =
  match json $! "label" with
  | `String label -> Label label
  | `Null ->
    let opcode : string = to_string (json $! "op") in
    (* Binary operators *)
    if is_binop opcode then
      let binop = binop_of_string opcode in
      let dest = get_dest json in
      let args = get_args json in
      let arg1 = List.nth args 0 in
      let arg2 = List.nth args 1 in
      Binop (dest, binop, arg1, arg2)
    else if is_unop opcode then
      let unop = unop_of_string opcode in
      let dest = get_dest json in
      let arg = List.hd (get_args json) in
      Unop (dest, unop, arg)
    else failwith "TODO"
  | _ -> failwith (Printf.sprintf "Invalid JSON : %s" (to_string json))
