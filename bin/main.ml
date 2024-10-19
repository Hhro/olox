module Lox = Olox_lib.Lox

let () =
  match Array.length Sys.argv with
  | 1 -> Lox.run_prompt ()
  | 2 -> Lox.run_file Sys.argv.(1)
  | _ ->
    Format.eprintf "Usage: olox [script]";
    exit 64
;;
