type t = { expr : Expr.t; tokens : Token.t list; current : int }

let create tokens = { expr = Expr.Nil; tokens; current = 0 }

let advance t = { t with current = t.current + 1 }

let peek t = List.nth t.tokens t.current

let is_over t = (peek t).kind = Token.EOF

let token_is token kinds = List.mem token.Token.kind kinds

let rec expression t = equality t

and equality t =
  let rec aux left =
    let op = peek left in
    match op.kind with
    | Token.BANG_EQUAL | Token.EQUAL_EQUAL ->
        let right = comparison (left |> advance) in
        aux
          {
            right with
            expr =
              Expr.Binary { left = left.expr; binop = op; right = right.expr };
          }
    | _ -> left
  in
  aux (comparison t)

and comparison t =
  let rec aux left =
    let op = peek left in
    match op.kind with
    | Token.GREATER | Token.GREATER_EQUAL | Token.LESS | Token.LESS_EQUAL ->
        let right = term (left |> advance) in
        aux
          {
            right with
            expr =
              Expr.Binary { left = left.expr; binop = op; right = right.expr };
          }
    | _ -> left
  in
  aux (term t)

and term t =
  let rec aux left =
    let op = peek left in
    match op.kind with
    | Token.MINUS | Token.PLUS ->
        let right = factor (left |> advance) in
        aux
          {
            right with
            expr =
              Expr.Binary { left = left.expr; binop = op; right = right.expr };
          }
    | _ -> left
  in
  aux (factor t)

and factor t =
  let rec aux left =
    let op = peek left in
    match op.kind with
    | Token.SLASH | Token.STAR ->
        let right = unary (left |> advance) in
        aux
          {
            right with
            expr =
              Expr.Binary { left = left.expr; binop = op; right = right.expr };
          }
    | _ -> left
  in
  aux (unary t)

and unary t =
  let op = peek t in
  match op.kind with
  | Token.BANG | Token.MINUS ->
      let right = unary (t |> advance) in
      { right with expr = Expr.Unary { unop = op; operand = right.expr } }
  | _ -> primary t

and primary t =
  let l = peek t in
  match l.kind with
  | Token.NUMBER ->
      {
        t with
        expr = Expr.Literal { value = Number (l.lexeme |> float_of_string) };
      }
      |> advance
  | Token.STRING ->
      { t with expr = Expr.Literal { value = String l.lexeme } } |> advance
  | Token.FALSE ->
      { t with expr = Expr.Literal { value = Bool false } } |> advance
  | Token.TRUE ->
      { t with expr = Expr.Literal { value = Bool true } } |> advance
  | Token.NIL -> { t with expr = Expr.Literal { value = Null } } |> advance
  | Token.LEFT_PAREN ->
      let t = expression (t |> advance) in
      if (peek t).kind <> RIGHT_PAREN then
        failwith "Expect ')' after expression.";
      { t with expr = Expr.Grouping { expression = t.expr } } |> advance
  | _ ->
      raise
        (Error.ParseError { line = (peek t).line; msg = "Expect expression." })

let parse t = try (expression t).expr with Error.ParseError _ -> Expr.Nil
