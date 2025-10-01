exception TypeError of Token.t * string

type t =
  | Binary of t * Token.t * t
  | Grouping of t
  | Literal of Value.t
  | Variable of Token.t
  | Unary of Token.t * t
  | Nil
[@@deriving show]

let rec evaluate t env =
  match t with
  | Literal v -> v
  | Variable name -> Env.get name env
  | Grouping t -> evaluate t env
  | Unary (token, t) -> evaluate_unary token t env
  | Binary (lt, token, rt) -> evaluate_binary lt token rt env
  | Nil -> Value.Nil

and evaluate_unary token t env =
  let value = evaluate t env in
  try
    match token.token_type with
    | MINUS -> Value.(~-value)
    | BANG -> Value.(!value)
    | _ -> failwith "unreachable"
  with
  | Value.TypeError msg -> raise (TypeError (token, msg))

and evaluate_binary lt token rt env =
  let lvalue = evaluate lt env in
  let rvalue = evaluate rt env in
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

let rec parenthesize t env =
  match t with
  | Binary (lexpr, op, rexpr) ->
    Format.sprintf
      "(%s %s %s)"
      op.lexeme
      (parenthesize lexpr env)
      (parenthesize rexpr env)
  | Grouping expr -> Format.sprintf "(group %s)" (parenthesize expr env)
  | Literal value -> Value.show value
  | Variable name -> name.lexeme
  | Unary (op, expr) -> Format.sprintf "(%s %s)" op.lexeme (parenthesize expr env)
  | Nil -> "nil"
;;
