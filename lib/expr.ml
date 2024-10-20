type t =
  | Binary of t * Token.t * t
  | Grouping of t
  | Literal of Value.t
  | Unary of Token.t * t
  | Nil
[@@deriving show]

let rec parenthesize t =
  match t with
  | Binary (lexpr, op, rexpr) ->
    Format.sprintf "(%s %s %s)" op.lexeme (parenthesize lexpr) (parenthesize rexpr)
  | Grouping expr -> Format.sprintf "(group %s)" (parenthesize expr)
  | Literal value -> Value.show value
  | Unary (op, expr) -> Format.sprintf "(%s %s)" op.lexeme (parenthesize expr)
  | Nil -> "nil"
;;
