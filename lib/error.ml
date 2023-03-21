type parse_error = { line : int; msg : string }

type type_error = { line : int; msg : string }

exception ParseError of parse_error

exception TypeError of type_error

let had_error = ref false

let report error =
  had_error := true;
  let fmt = format_of_string "[line %d] Error: %s" in
  match error with
  | TypeError te -> Printf.eprintf fmt te.line te.msg
  | ParseError pe -> Printf.eprintf fmt pe.line pe.msg
  | _ -> ()
