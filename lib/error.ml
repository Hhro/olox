exception OLoxError of string

let report line where message =
  raise (OLoxError (Format.sprintf "[line %d] Error %s: %s" line where message))
;;

let error line message = report line "" message
