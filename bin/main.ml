open Lib
open Helpers
open Yojson.Basic.Util
open Core

let () =
  let json = Helpers.load_json () in
  let functions = Helpers.list_of_json (json $! "functions") in
  List.iter functions ~f:(fun func ->
      printf "function: %s\n" (to_string (func $! "name"));
      let instrs = Helpers.list_of_json (func $! "instrs") in
      List.iter instrs ~f:(fun instr_json ->
          let instr = Syntax.instr_of_json instr_json in
          let str = Sexp.to_string_hum (Syntax.sexp_of_instr instr) in
          printf "%s\n" str);
      printf "\n")
