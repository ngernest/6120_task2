open Helpers
open Syntax
open StdLabels

(** Determines if an instruction is a terminator ([Jmp], [Br], [Ret]) *)
let is_terminator : instr -> bool = function
  | Jmp _ | Br _ | Ret _ -> true
  | _ -> false

(** Determines whether an instruction is an operation 
    (all instructions are operations except labels) *)
let is_op : instr -> bool = function
  | Label _ -> false
  | _ -> true

(** A basic [block] contains an ordered list of instructions *)
type block = instr list

(** Forms basic blocks containing the instructions in [body] *)
let form_blocks (body : instr list) : block list =
  let curr_block = ref [] in
  List.fold_left
    ~f:(fun blocks instr ->
      if is_op instr then
        (* A terminator terminates [curr_block], so we need to add it to
           [blocks] *)
        if is_terminator instr then (
          let new_blocks = List.append blocks [ !curr_block ] in
          curr_block := [];
          new_blocks)
        else blocks
      else
        (* We have a label *)
        let new_blocks =
          if not (List.is_empty !curr_block) then
            List.append blocks [ !curr_block ]
          else blocks in
        curr_block := [ instr ];
        new_blocks)
    ~init:[] body

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
        let instrs =
          List.map ~f:instr_of_json (list_of_json (func $! "instrs")) in
        let blocks = form_blocks instrs in
        failwith "TODO")
      functions in
  failwith "TODO"
