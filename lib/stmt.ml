type t =
  | Expression of Expr.t
  | Print of Expr.t
[@@deriving show]

let accept t =
  match t with
  | Expression expr -> Expr.evaluate expr |> ignore
  | Print expr -> Value.show (Expr.evaluate expr) |> print_endline
;;
