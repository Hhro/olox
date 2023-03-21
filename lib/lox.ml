let run source =
  source |> Scanner.create |> Scanner.scan_tokens |> Parser.create
  |> Parser.parse |> Interpreter.visit |> Expr.Value.to_string |> print_endline

let run_file path =
  let ic = open_in path in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;
  run source;
  if !Error.had_error then exit 70

let run_prompt () =
  let rec exec_line () =
    print_string "> ";
    let line = input_line stdin in
    if line = "" then exit 0 else run line;
    exec_line ()
  in
  exec_line ()
