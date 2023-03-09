let had_error = ref false

let report line where msg =
  Printf.eprintf "[line %d] Error %s: %s" line where msg;
  had_error := true;
  exit 65

let error line msg = report line "" msg
