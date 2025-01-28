open Helpers
open Syntax
open StdLabels

(** Determines if an instruction is a terminator ([Jmp], [Br], [Ret]) *)
let is_terminator : instr -> bool = function
  | Jmp _ | Br _ | Ret _ -> true
  | _ -> false

(** Forms a basic block *)
let form_blocks body = failwith "TODO"
(* List.fold_left ~f:(fun blocks instr -> let curr_block = [] in if instr $?
   "op" then if is_terminator (instr $? "op") then List.append blocks
   (List.append curr_block instr) else failwith "TODO") ~init:[] body *)

(** Creates labels for a list of blocks 
    (generating fresh labels when necessary) *)
(* let mk_block_map blocks = List.map (fun block -> if block $? "key" )
   blocks *)

let () =
  let json = load_json () in

  let functions = Helpers.list_of_json (json $! "functions") in
  let blocks =
    List.map
      ~f:(fun func ->
        let blocks = form_blocks (func $! "instrs") in
        failwith "TODO")
      functions in
  failwith "TODO"
