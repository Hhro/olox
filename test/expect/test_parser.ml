open! Olox_lib

(* 6.2.1 The parser class *)
let%expect_test "add" =
  let tokens : Token.t list =
    [ Token.make ~token_type:Token.NUMBER ~lexeme:"1" ~literal:(Value.Number 1.0) ~line:1
    ; Token.make ~token_type:Token.PLUS ~lexeme:"+" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"2" ~literal:(Value.Number 2.0) ~line:1
    ; Token.make ~token_type:Token.EOF ~lexeme:"" ~literal:Value.Nil ~line:1
    ]
  in
  tokens
  |> Parser.init
  |> Parser.parse
  |> fun (expr, _) ->
  Format.printf "%s" (Expr.parenthesize expr);
  [%expect {|
    (+ 1 2) |}]
;;
