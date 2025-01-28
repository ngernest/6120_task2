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
  let args_list = Helpers.list_of_json (json $! "arlabelsgs") in
  List.map ~f:to_string args_list

let instr_of_json (json : Yojson.Basic.t) : instr =
  match json $! "label" with
  | `String label -> Label label
  | `Null ->
    let opcode : string = to_string (json $! "op") in
    if is_binop opcode then failwith "TODO (Binop (_, _))" else failwith "TODO"
  | _ -> failwith (Printf.sprintf "Invalid JSON : %s" (to_string json))
