type t =
  | Number of float
  | String of string
  | Nil

let number_of_string s =
  try Number (float_of_string s) with
  | Failure _ -> Error.error 0 "invalid number"
;;

let to_string t =
  match t with
  | Number f -> string_of_float f
  | String s -> s
  | Nil -> ""
;;
