open Core
open Syntax

(** Adds a [Nop] after every instruction in a function *)
let add_nops (func : func) : func =
  let new_instrs = List.intersperse func.instrs ~sep:Nop in
  { func with instrs = new_instrs }
