let check_operation op operands =
  let open Token in
  let open Expr in
  match (op.kind, operands) with
  | BANG, [ Value.Bool _ ]
  | MINUS, [ Number _ ]
  | PLUS, [ Number _; Number _ ]
  | PLUS, [ String _; String _ ]
  | MINUS, [ Number _; Number _ ]
  | STAR, [ Number _; Number _ ]
  | SLASH, [ Number _; Number _ ]
  | GREATER, [ Number _; Number _ ]
  | GREATER_EQUAL, [ Number _; Number _ ]
  | LESS, [ Number _; Number _ ]
  | LESS_EQUAL, [ Number _; Number _ ]
  | EQUAL_EQUAL, [ _; _ ]
  | BANG_EQUAL, [ _; _ ] ->
      true
  | _ -> false

let rec visit (expr : Expr.t) =
  let open Expr in
  let error_fmt = format_of_string "Cannot evaluate '%s' in '%s'" in
  match expr with
  | Literal l -> l.value
  | Grouping g -> visit g.expression
  | Unary u ->
      let operand = visit u.operand in
      if check_operation u.unop [ operand ] then
        match u.unop.kind with
        | MINUS -> Value.unary_arithmetic ( ~-. ) operand
        | BANG -> Value.unary_logic not operand
        | _ -> failwith "unreachable"
      else
        let msg =
          Format.sprintf error_fmt u.unop.lexeme (expr |> Expr.to_string)
        in
        Error.report (Error.ParseError { line = u.unop.line; msg });
        Value.Null
  | Binary b ->
      let left = visit b.left in
      let right = visit b.right in
      if check_operation b.binop [ left; right ] then
        match b.binop.kind with
        | PLUS -> Value.binary_arithmetic ( +. ) left right
        | MINUS -> Value.binary_arithmetic ( -. ) left right
        | STAR -> Value.binary_arithmetic ( *. ) left right
        | SLASH -> Value.binary_arithmetic ( /. ) left right
        | LESS -> Value.binary_comparison ( < ) left right
        | LESS_EQUAL -> Value.binary_comparison ( <= ) left right
        | GREATER -> Value.binary_comparison ( > ) left right
        | GREATER_EQUAL -> Value.binary_comparison ( >= ) left right
        | EQUAL_EQUAL -> Value.equal left right
        | BANG_EQUAL -> Value.equal left right
        | _ -> failwith "unreachable"
      else
        let msg =
          Format.sprintf error_fmt b.binop.lexeme (expr |> Expr.to_string)
        in
        Error.report (Error.ParseError { line = b.binop.line; msg });
        Value.Null
  | Nil -> Expr.Value.Null
