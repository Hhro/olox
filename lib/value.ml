type t =
  | Number of float [@printer fun fmt -> fprintf fmt "%g"]
  | String of string [@printer fun fmt -> fprintf fmt "\"%s\""]
  | Bool of bool
  | Nil
[@@deriving show { with_path = false }]
