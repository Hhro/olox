exception TypeError of Token.t * string

type t =
  | Binary of t * Token.t * t
  | Grouping of t
  | Literal of Value.t
  | Unary of Token.t * t
  | Nil
[@@deriving show]

let rec evaluate t =
  match t with
  | Literal v -> v
  | Grouping t -> evaluate t
  | Unary (token, t) -> evaluate_unary token t
  | Binary (lt, token, rt) -> evaluate_binary lt token rt
  | Nil -> Value.Nil

and evaluate_unary token t =
  let value = evaluate t in
  try
    match token.token_type with
    | MINUS -> Value.(~-value)
    | BANG -> Value.(!value)
    | _ -> failwith "unreachable"
  with
  | Value.TypeError msg -> raise (TypeError (token, msg))

and evaluate_binary lt token rt =
  let lvalue = evaluate lt in
  let rvalue = evaluate rt in
  try
    match token.token_type with
    | GREATER -> Value.(lvalue > rvalue)
    | GREATER_EQUAL -> Value.(lvalue >= rvalue)
    | LESS -> Value.(lvalue < rvalue)
    | LESS_EQUAL -> Value.(lvalue <= rvalue)
    | BANG_EQUAL -> Value.(lvalue != rvalue)
    | EQUAL_EQUAL -> Value.(lvalue == rvalue)
    | MINUS -> Value.(lvalue - rvalue)
    | PLUS -> Value.(lvalue + rvalue)
    | SLASH -> Value.(lvalue / rvalue)
    | STAR -> Value.(lvalue * rvalue)
    | _ -> failwith "unreachable"
  with
  | Value.TypeError msg -> raise (TypeError (token, msg))
;;

let rec parenthesize t =
  match t with
  | Binary (lexpr, op, rexpr) ->
    Format.sprintf "(%s %s %s)" op.lexeme (parenthesize lexpr) (parenthesize rexpr)
  | Grouping expr -> Format.sprintf "(group %s)" (parenthesize expr)
  | Literal value -> Value.show value
  | Unary (op, expr) -> Format.sprintf "(%s %s)" op.lexeme (parenthesize expr)
  | Nil -> "nil"
;;
