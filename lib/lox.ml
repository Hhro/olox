let had_error = ref false

let interpret stmts env =
  let rec aux stmts env =
    match stmts with
    | hd :: tl -> Stmt.accept hd env |> aux tl
    | [] -> env
  in
  aux stmts env
;;

let run src env =
  try
    let stmts =
      src |> Scanner.init |> Scanner.scan_tokens |> Parser.init |> Parser.parse
    in
    interpret stmts env
  with
  | Scanner.ScanError (line, msg) ->
    Format.printf "[line %d] Error: %s\n" line msg;
    Format.print_flush ();
    had_error := true;
    env
  | Expr.TypeError (token, msg) ->
    Format.printf "[line %d] Error: %s\n" token.line msg;
    Format.print_flush ();
    had_error := true;
    env
;;

let run_prompt () =
  let env = Env.empty in
  let rec exec_line env =
    print_string "> ";
    flush_all ();
    let line =
      try input_line stdin with
      | End_of_file -> exit 0
    in
    run line env |> exec_line
  in
  exec_line env
;;

let run_file src_path =
  let env = Env.empty in
  let ic = open_in src_path in
  let src =
    try really_input_string ic (in_channel_length ic) with
    | End_of_file ->
      Format.eprintf "read error: %s" src_path;
      exit 1
  in
  run src env |> ignore;
  if !had_error then exit 65
;;
