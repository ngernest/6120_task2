open Helpers

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

(** Unary operators *)
type unop =
  | Not
  | And
  | Or

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

let get_args (json : Yojson.Basic.t) = Helpers.list_of_json (json $! "args")

let instr_of_json (json : Yojson.Basic.t) : instr =
  match json $! "label" with
  | `String label -> Label label
  | `Null -> failwith "TODO"
  | _ ->
    failwith
      (Printf.sprintf "Invalid JSON : %s" (Yojson.Basic.Util.to_string json))
