let run src =
  src
  |> Scanner.init
  |> Scanner.scan_tokens
  |> List.iter (fun t -> Format.asprintf "%a" Token.pp t |> print_endline);
  true
;;

let run_prompt () =
  let rec exec_line () =
    print_string "> ";
    flush_all ();
    let line =
      try input_line stdin with
      | End_of_file -> exit 0
    in
    if run line then exec_line () else exit 65
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
  if not (run src) then exit 65
;;
