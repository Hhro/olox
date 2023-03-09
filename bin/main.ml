let () =
  match Array.length Sys.argv with
  | 1 -> Lox.run_prompt ()
  | 2 -> Lox.run_file Sys.argv.(1)
  | _ ->
      print_endline "Usage: olox [script]";
      exit 64
