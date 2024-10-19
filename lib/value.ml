type t =
  | Number of float
  | String of string
  | Nil
[@@deriving show]

let to_string t =
  match t with
  | Number f -> string_of_float f
  | String s -> s
  | Nil -> ""
;;
