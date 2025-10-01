exception ParseError of string

type t =
  { tokens : Token.t list
  ; current : int
  }

let init tokens = { tokens; current = 0 }
let peek t = List.nth t.tokens t.current
let is_at_end t = (peek t).token_type == EOF
let advance t = { t with current = t.current + 1 }

let error (token : Token.t) message =
  let line = token.line in
  match token.token_type with
  | Token.EOF ->
    raise (ParseError (Format.sprintf "[line %d] Error at end: %s" line message))
  | _ ->
    raise
      (ParseError (Format.sprintf "[line %d] Error at '%s': %s" line token.lexeme message))
;;

let synchronize t =
  let rec discard_until_next_stmt t =
    let token = peek t in
    match token.token_type with
    | EOF | CLASS | FOR | FUN | IF | PRINT | RETURN | VAR | WHILE -> t
    | SEMICOLON -> advance t
    | _ -> discard_until_next_stmt (advance t)
  in
  discard_until_next_stmt t
;;

let rec expression t = equality t

and declaration t =
  try
    let op = peek t in
    match op.token_type with
    | VAR -> var_declaration (advance t)
    | _ -> statement t
  with
  | ParseError err ->
    print_endline err;
    Stmt.Expression Expr.Nil, synchronize t

and var_declaration t =
  let name, next =
    match (peek t).token_type with
    | IDENTIFIER -> peek t, advance t
    | _ -> error (peek t) "Expect variable name."
  in
  let init, next =
    match (peek next).token_type with
    | EQUAL -> expression (advance next)
    | _ -> Expr.Nil, next
  in
  if (peek next).token_type != SEMICOLON then error (peek next) "Expect ';' after value.";
  Stmt.Var (name, init), advance next

and statement t =
  let op = peek t in
  match op.token_type with
  | PRINT -> print_statement (advance t)
  | _ -> expression_statement t

and print_statement t =
  let expr, next = expression t in
  if (peek next).token_type != SEMICOLON then error (peek next) "Expect ';' after value.";
  Stmt.Print expr, advance next

and expression_statement t =
  let expr, next = expression t in
  if (peek next).token_type != SEMICOLON
  then error (peek next) "Expect ';' after expression.";
  Stmt.Expression expr, advance next

and equality t =
  let rec derive_right expr t =
    let op = peek t in
    let next = advance t in
    match op.token_type with
    | BANG_EQUAL | EQUAL_EQUAL ->
      let right, next = comparison next in
      derive_right (Expr.Binary (expr, op, right)) next
    | _ -> expr, t
  in
  let expr, next = comparison t in
  if is_at_end next then expr, next else derive_right expr next

and comparison t =
  let rec derive_right expr t =
    let op = peek t in
    let next = advance t in
    match op.token_type with
    | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL ->
      let right, next = term next in
      derive_right (Expr.Binary (expr, op, right)) next
    | _ -> expr, t
  in
  let expr, next = term t in
  if is_at_end next then expr, next else derive_right expr next

and term t =
  let rec derive_right expr t =
    let op = peek t in
    let next = advance t in
    match op.token_type with
    | MINUS | PLUS ->
      let right, next = factor next in
      derive_right (Expr.Binary (expr, op, right)) next
    | _ -> expr, t
  in
  let expr, next = factor t in
  if is_at_end next then expr, next else derive_right expr next

and factor t =
  let rec derive_right expr t =
    let op = peek t in
    let next = advance t in
    match op.token_type with
    | SLASH | STAR ->
      let right, next = unary next in
      derive_right (Expr.Binary (expr, op, right)) next
    | _ -> expr, t
  in
  let expr, next = unary t in
  if is_at_end next then expr, next else derive_right expr next

and unary t =
  let op = peek t in
  match op.token_type with
  | BANG | MINUS ->
    let next = advance t in
    let right, next = unary next in
    Expr.Unary (op, right), next
  | _ -> primary t

and primary t =
  let token = peek t in
  let next = advance t in
  match token.token_type with
  | FALSE -> Expr.Literal (Value.Bool false), next
  | TRUE -> Expr.Literal (Value.Bool true), next
  | NIL -> Expr.Literal Value.Nil, next
  | NUMBER | STRING -> Expr.Literal token.literal, next
  | IDENTIFIER -> Expr.Variable token, next
  | LEFT_PAREN ->
    let expr, next = expression next in
    if (peek next).token_type != RIGHT_PAREN
    then error (peek next) "Expect ')' after expression."
    else Expr.Grouping expr, advance next
  | _ -> error token "Expect expression."
;;

let parse t =
  let rec stmt_until_end statements cur =
    if is_at_end cur
    then List.rev statements
    else (
      let stmt, next = declaration t in
      stmt_until_end (stmt :: statements) next)
  in
  stmt_until_end [] t
;;
