open Core

(* open Helpers *)
open Syntax

(** Adds a [Nop] after every instruction in a function *)
let add_nops (func : func) : func =
  let new_instrs = List.intersperse func.instrs ~sep:Nop in
  { func with instrs = new_instrs }

(* let add_nops_pipeline () = let json = load_json () in let functions =
   list_of_json (json $! "functions") in List.iter functions ~f:(fun func -> (*
   Convert Bril programs from JSON to a typed representation *) let instrs =
   List.map ~f:instr_of_json (list_of_json (func $! "instrs")) in

   (* Add a [Nop] after every instruction *) let new_instrs = List.intersperse
   instrs ~sep:Nop in

   ) *)
