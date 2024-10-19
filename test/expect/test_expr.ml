open! Olox_lib

let%expect_test "print" =
  let expr =
    Expr.Binary
      ( Expr.Unary
          ( { token_type = Token.MINUS; lexeme = "-"; literal = Value.Nil; line = 1 }
          , Expr.Literal (Value.Number 123.0) )
      , { token_type = Token.STAR; lexeme = "*"; literal = Value.Nil; line = 1 }
      , Expr.Grouping (Expr.Literal (Value.Number 45.67)) )
  in
  Format.printf "%s" (Expr.parenthesize expr);
  [%expect {| (* (- 123) (group 45.67)) |}]
;;
