let run source =
  let scanner = Scanner.create source in
  let tokens = scanner |> Scanner.scan_tokens in
  List.iter (fun tkn -> tkn |> Token.to_string |> print_endline) tokens

let run_file path =
  let ic = open_in path in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;
  run source

let run_prompt () =
  let rec exec_line () =
    print_string "> ";
    let line = input_line stdin in
    if line = "" then exit 0 else run line;
    exec_line ()
  in
  exec_line ()
