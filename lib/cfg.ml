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
  printf "Entered form_blocks\n";
  let curr_block : block ref = ref [] in
  (* NB: [blocks] is the list of basic blocks that this function outputs *)
  let blocks =
    List.fold
      ~f:(fun blocks instr ->
        printf "instr = %s\n" (Sexp.to_string_hum (sexp_of_instr instr));
        if is_op instr then (
          (* Add the current instruction to the block *)
          curr_block := List.append !curr_block [ instr ];
          (* A terminator terminates [curr_block], so we need to add it to
             [blocks] *)
          if is_terminator instr then (
            let new_blocks = List.append blocks [ !curr_block ] in
            curr_block := [];
            printf "  new_blocks = %s\n"
              (Sexp.to_string_hum ([%sexp_of: block list] new_blocks));
            new_blocks)
          else (
            printf "  blocks = %s\n"
              (Sexp.to_string_hum ([%sexp_of: block list] blocks));
            blocks))
        else
          (* We have a label *)
          let new_blocks =
            if not (List.is_empty !curr_block) then
              List.append blocks [ !curr_block ]
            else blocks in
          curr_block := [ instr ];
          printf "  new_blocks = %s\n"
            (Sexp.to_string_hum ([%sexp_of: block list] new_blocks));
          new_blocks)
      ~init:[] body in
  (* Need to add a final block to [blocks] if we have any instrs remaining *)
  if not (List.is_empty !curr_block) then List.append blocks [ !curr_block ]
  else blocks

(** Creates labels for a list of blocks 
    (generating fresh labels when necessary) *)
let mk_block_map (blocks : block list) : (label * block) list =
  List.rev
  @@ List.fold
       ~f:(fun acc block ->
         let fresh_name = spf "b%d" (List.length acc) in
         match block with
         | [] -> (fresh_name, block) :: acc
         | hd :: tl ->
           let name =
             match hd with
             | Label lbl -> lbl
             | _ -> fresh_name in
           (name, tl) :: acc)
       ~init:[] blocks

(** Given a name-to-block map [name2block], [get_cfg] 
    produces a map from block names to a list of successor block names *)
let get_cfg (name2block : (label * block) list) : (label * label list) list =
  List.rev
  @@ List.foldi
       ~f:(fun i acc (name, block) ->
         if List.is_empty block then (name, []) :: acc
         else
           let last = List.last_exn block in
           let succ =
             match last with
             | Jmp label -> [ label ]
             | Br (_, lbl1, lbl2) -> [ lbl1; lbl2 ]
             | Ret _ -> []
             | _ when Int.equal i (List.length name2block - 1) ->
               (* We've reached the last instruction in the block, so there is
                  no successor *)
               []
             | _ ->
               (* Fall through to the next block in [name2block] *)
               let keys = List.map ~f:fst name2block in
               [ List.nth_exn keys (i + 1) ] in
           (name, succ) :: acc)
       ~init:[] name2block

(** Constructs a CFG for a Bril program *)
let mycfg () : unit =
  let json = load_json () in
  let functions = list_of_json (json $! "functions") in
  List.iter functions ~f:(fun func ->
      (* Convert Bril programs from JSON to a typed representation *)
      let instrs = List.map ~f:instr_of_json (list_of_json (func $! "instrs")) in

      let blocks = form_blocks instrs in
      printf "\n\n";
      printf "No. of blocks = %d\n" (List.length blocks);
      printf "blocks:\n    %s\n"
        (Sexp.to_string_hum ~indent:8 ([%sexp_of: block list] blocks));

      (* Fetch labelled basic blocks *)
      let name2block = mk_block_map blocks in

      (* printf "name2block:\n %s\n" (Sexp.to_string_hum ([%sexp_of: (label *
         block) list] name2block)); *)

      (* Print the label & instructions in each block *)
      List.iter name2block ~f:(fun (name, block) ->
          printf "%s\n" name;
          printf "  %s\n" (Sexp.to_string_hum (sexp_of_block block)));

      (* Build the CFG *)
      let cfg = get_cfg name2block in

      (* Produce GraphViz visualization *)
      printf "diagraph %s {{\n" (Yojson.Basic.Util.to_string (func $! "name"));
      List.iter name2block ~f:(fun (name, _) -> printf "  %s;\n" name);
      List.iter cfg ~f:(fun (name, succs) ->
          List.iter succs ~f:(fun succ -> printf "  %s -> %s\n" name succ));
      printf "}\n")
