let had_error = ref false
let interpret expr = print_endline (Value.show (Expr.evaluate expr))

let run src =
  try
    src |> Scanner.init |> Scanner.scan_tokens |> Parser.init |> Parser.parse |> interpret
  with
  | Scanner.ScanError (line, msg) ->
    Format.printf "[line %d] Error: %s\n" line msg;
    Format.print_flush ();
    had_error := true
  | Expr.TypeError (token, msg) ->
    Format.printf "[line %d] Error: %s\n" token.line msg;
    Format.print_flush ();
    had_error := true
;;

let run_prompt () =
  let rec exec_line () =
    print_string "> ";
    flush_all ();
    let line =
      try input_line stdin with
      | End_of_file -> exit 0
    in
    run line;
    exec_line ()
  in
  exec_line ()
;;

let run_file src_path =
  let ic = open_in src_path in
  let src =
    try really_input_string ic (in_channel_length ic) with
    | End_of_file ->
      Format.eprintf "read error: %s" src_path;
      exit 1
  in
  run src;
  if !had_error then exit 65
;;
