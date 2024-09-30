let report line where message =
  Format.eprintf "[line %d] Error %s: %s" line where message;
  exit 1
;;

let error line message = report line "" message
