type t =
  { tokens : Token.t list
  ; current : int
  }

let init tokens = { tokens; current = 0 }
let peek t = List.nth t.tokens t.current
let is_at_end t = (peek t).token_type == EOF
let advance t = { t with current = t.current + 1 }

let rec expression t = equality t

and equality t =
  let rec consume expr t =
    let op = peek t in
    let next = advance t in
    match op.token_type with
    | BANG_EQUAL | EQUAL_EQUAL ->
      let right, next = comparison next in
      consume (Expr.Binary (expr, op, right)) next
    | _ -> expr, t
  in
  let expr, next = comparison t in
  if is_at_end next then expr, next else consume expr next

and comparison t =
  let rec consume expr t =
    let op = peek t in
    let next = advance t in
    match op.token_type with
    | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL ->
      let right, next = term next in
      consume (Expr.Binary (expr, op, right)) next
    | _ -> expr, t
  in
  let expr, next = term t in
  if is_at_end next then expr, next else consume expr next

and term t =
  let rec consume expr t =
    let op = peek t in
    let next = advance t in
    match op.token_type with
    | MINUS | PLUS ->
      let right, next = factor next in
      consume (Expr.Binary (expr, op, right)) next
    | _ -> expr, t
  in
  let expr, next = factor t in
  if is_at_end next then expr, next else consume expr next

and factor t =
  let rec consume expr t =
    let op = peek t in
    let next = advance t in
    match op.token_type with
    | SLASH | STAR ->
      let right, next = unary next in
      consume (Expr.Binary (expr, op, right)) next
    | _ -> expr, t
  in
  let expr, next = unary t in
  if is_at_end next then expr, next else consume expr next

and unary t =
  let rec consume expr t =
    let op = peek t in
    let next = advance t in
    match op.token_type with
    | BANG | MINUS ->
      let right, next = unary next in
      consume (Expr.Unary (op, right)) next
    | _ -> expr, t
  in
  let expr, next = primary t in
  if is_at_end next then expr, next else consume expr next

and primary t =
  let token = peek t in
  let next = advance t in
  match token.token_type with
  | FALSE -> Expr.Literal (Value.Bool false), next
  | TRUE -> Expr.Literal (Value.Bool true), next
  | NIL -> Expr.Literal Value.Nil, next
  | NUMBER | STRING -> Expr.Literal token.literal, next
  | LEFT_PAREN ->
    let expr, next = expression next in
    if (peek next).token_type != RIGHT_PAREN
    then Error.error token.line "Expect ')' after expression."
    else Expr.Grouping expr, advance next
  | _ -> Error.error 0 "Invalid token"
;;

let parse t = expression t
