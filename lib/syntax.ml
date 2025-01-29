open Helpers
open Core

(* -------------------------------------------------------------------------- *)
(*                            Labels and arguments                            *)
(* -------------------------------------------------------------------------- *)

(** All [label]s are just strings *)
type label = string [@@deriving sexp]

(** All arguments are strings *)
type arg = string [@@deriving sexp]

(* -------------------------------------------------------------------------- *)
(*                                    Types                                   *)
(* -------------------------------------------------------------------------- *)

(** Primitive types (int or bools) *)
type ty =
  | TyInt
  | TyBool
[@@deriving sexp]

(** Converts a string to a [ty] *)
let ty_of_string (str : string) : ty =
  match str with
  | "int" -> TyInt
  | "bool" -> TyBool
  | _ -> failwith (spf "invalid string: %s\n" str)

(* -------------------------------------------------------------------------- *)
(*                                  Literals                                  *)
(* -------------------------------------------------------------------------- *)

(** Literal values (int & bool values) *)
type literal =
  | LitInt of int
  | LitBool of bool
[@@deriving sexp]

(* -------------------------------------------------------------------------- *)
(*                            Destination variables                           *)
(* -------------------------------------------------------------------------- *)

(** A {i destination variable} is a pair consisting of 
    the variable name & the variable's type *)
type dest = string * ty [@@deriving sexp]

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
[@@deriving sexp]

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
  let binop_opcodes : string list = List.map ~f:fst binop_opcode_map in
  List.mem binop_opcodes opcode ~equal:String.equal

(** Converts a string to a [binop] *)
let binop_of_string (opcode : string) : binop =
  List.Assoc.find_exn binop_opcode_map opcode ~equal:String.equal

(* -------------------------------------------------------------------------- *)
(*                               Unary operators                              *)
(* -------------------------------------------------------------------------- *)

(** Unary operators *)
type unop =
  | Not
  | Id
[@@deriving sexp]

(** Maps each unary operator's opcode to the corresponding [unop] *)
let unop_opcode_map : (string * unop) list = [ ("not", Not); ("id", Id) ]

(** Determines if an opcode represents a binary operator *)
let is_unop (opcode : string) : bool =
  let unop_opcodes = List.map ~f:fst unop_opcode_map in
  List.mem unop_opcodes opcode ~equal:String.equal

(** Converts a string to an [unop] *)
let unop_of_string (opcode : string) : unop =
  List.Assoc.find_exn unop_opcode_map opcode ~equal:String.equal

(* -------------------------------------------------------------------------- *)
(*                                Other opcodes                               *)
(* -------------------------------------------------------------------------- *)

(* -------------------- Functions for determining opcodes ------------------- *)

(** Determines if an opcode is ["const"] *)
let is_const (opcode : string) : bool = String.equal opcode "const"

(** Determines if an opcode is ["nop"] *)
let is_nop (opcode : string) : bool = String.equal opcode "nop"

(** Determines if an opcode is ["jmp"] *)
let is_jmp (opcode : string) : bool = String.equal opcode "jmp"

(** Determines if an opcode is ["br"] *)
let is_br (opcode : string) : bool = String.equal opcode "br"

(** Determines if an opcode is ["call"] *)
let is_call (opcode : string) : bool = String.equal opcode "call"

(** Determines if an opcode is ["print"] *)
let is_print (opcode : string) : bool = String.equal opcode "print"

(** Determines if an opcode is ["ret"] *)
let is_ret (opcode : string) : bool = String.equal opcode "ret"

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
  | Print of arg list [@sexp.list]
  | Call of dest option * string * arg list
  | Nop
[@@deriving sexp]

(* -------------------------------------------------------------------------- *)
(*                                  Functions                                 *)
(* -------------------------------------------------------------------------- *)

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
  args : (string * ty) list; [@sexp.list]
  ret_type : ty option; [@sexp.option]
  instrs : (string * instr list) list; [@sexp.list]
}
[@@deriving sexp]

(******************************************************************************)
(******************************************************************************)
(******************************************************************************)

(* -------------------------------------------------------------------------- *)
(*                           Extracting JSON fields                           *)
(* -------------------------------------------------------------------------- *)

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

(** Retrieves the contents of the ["value"] field in a JSON object
    as a Bril literal (either an int or a bool) *)
let get_value (json : Yojson.Basic.t) : literal =
  let value = json $! "value" in
  match value with
  | `Int i -> LitInt i
  | `Bool b -> LitBool b
  | _ -> failwith (spf "Invalid value %s" (to_string value))

(** Retrieves the name and type of the destination variable
    from a JSON object *)
let get_dest (json : Yojson.Basic.t) : dest =
  let dest_string = to_string (json $! "dest") in
  let ty = ty_of_string @@ to_string (json $! "type") in
  (dest_string, ty)

(** Retrieves the contents of the ["funcs"] field in a JSON object
    as a string list (list of function names) *)
let get_funcs (json : Yojson.Basic.t) : string list =
  List.map ~f:to_string (Helpers.list_of_json (json $! "funcs"))

(* -------------------------------------------------------------------------- *)
(*                  Converting from JSON to Bril instructions                 *)
(* -------------------------------------------------------------------------- *)

(** Converts a JSON object to an [instr] *)
let instr_of_json (json : Yojson.Basic.t) : instr =
  match json $! "label" with
  | `String label -> Label label
  | `Null ->
    let opcode : string = to_string (json $! "op") in
    if is_const opcode then
      (* Constants *)
      let dest = get_dest json in
      let literal = get_value json in
      Const (dest, literal)
    else if is_binop opcode then
      (* Binary operators *)
      let binop = binop_of_string opcode in
      let dest = get_dest json in
      let args = get_args json in
      let arg1 = List.nth_exn args 0 in
      let arg2 = List.nth_exn args 1 in
      Binop (dest, binop, arg1, arg2)
    else if is_unop opcode then
      (* Unary operators *)
      let unop = unop_of_string opcode in
      let dest = get_dest json in
      let arg = List.hd_exn (get_args json) in
      Unop (dest, unop, arg)
    else if is_jmp opcode then
      (* Jmp *)
      let label = List.hd_exn (get_labels json) in
      Jmp label
    else if is_br opcode then
      (* Br *)
      let arg = List.hd_exn (get_args json) in
      let labels = get_labels json in
      let true_lbl = List.nth_exn labels 0 in
      let false_lbl = List.nth_exn labels 1 in
      Br (arg, true_lbl, false_lbl)
    else if is_ret opcode then
      (* Ret *)
      let args = get_args json in
      match args with
      | [] -> Ret None
      | _ -> Ret (Some (List.hd_exn args))
    else if is_print opcode then
      (* Print *)
      let args = get_args json in
      Print args
    else if is_call opcode then
      (* Call *)
      let args = get_args json in
      let func_name = List.hd_exn (get_funcs json) in
      if contains_key json "dest" then
        let dest = get_dest json in
        Call (Some dest, func_name, args)
      else Call (None, func_name, args)
    else if is_nop opcode then Nop
    else failwith (spf "Invalid opcode : %s" opcode)
  | _ -> failwith (spf "Invalid JSON : %s" (to_string json))
