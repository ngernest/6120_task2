open Helpers
open Syntax
open Core

(** Determines if an instruction is a terminator ([Jmp], [Br], [Ret]) *)
let is_terminator : instr -> bool = function
  | Jmp _ | Br _ | Ret _ -> true
  | _ -> false

(** Determines whether an instruction is an operation 
    (all instructions are operations except labels) *)
let is_op : instr -> bool = function
  | Label _ -> false
  | _ -> true

(** The type of {i basic blocks} (an ordered list of instructions) *)
type block = instr list [@@deriving sexp]

(** Forms basic blocks containing the instructions in [body] *)
let form_blocks (body : instr list) : block list =
  let curr_block : block ref = ref [] in
  (* NB: [blocks] is the list of basic blocks that this function outputs *)
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
let mk_block_map (blocks : block list) : (label * block) list =
  List.rev
  @@ List.fold
       ~f:(fun acc block ->
         match block with
         | [] -> failwith "Empty block with no instructions"
         | hd :: tl ->
           let name =
             match hd with
             | Label lbl -> lbl
             | _ -> spf "b%d" (List.length acc) in
           (name, tl) :: acc)
       ~init:[] blocks

(** Given a name-to-block map [name2block], [get_cfg] 
    produces a map from block names to a list of successor block names *)
let get_cfg (name2block : (string * block) list) : (label * label list) list =
  List.rev
  @@ List.foldi
       ~f:(fun i acc (name, block) ->
         let last = List.last_exn block in
         let succ =
           match last with
           | Jmp label -> [ label ]
           | Br (_, lbl1, lbl2) -> [ lbl1; lbl2 ]
           | Ret _ -> []
           | _ when Int.equal i (List.length name2block - 1) ->
             (* We've reached the last instruction in the block, so there is no
                successor *)
             []
           | _ ->
             (* Fall through to the next block in [name2block] *)
             let keys = List.map ~f:fst name2block in
             [ List.nth_exn keys (i + 1) ] in
         (name, succ) :: acc)
       ~init:[] name2block

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
