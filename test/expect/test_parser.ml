open! Olox_lib

(* 6.2.1 recursive descent parsing *)
let%expect_test "add" =
  let tokens : Token.t list =
    [ Token.make ~token_type:Token.NUMBER ~lexeme:"1" ~literal:(Value.Number 1.0) ~line:1
    ; Token.make ~token_type:Token.PLUS ~lexeme:"+" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"2" ~literal:(Value.Number 2.0) ~line:1
    ; Token.make ~token_type:Token.EOF ~lexeme:"" ~literal:Value.Nil ~line:1
    ]
  in
  tokens |> Parser.init |> Parser.parse |> Expr.parenthesize |> Format.printf "%s";
  [%expect {| (+ 1 2) |}]
;;

let%expect_test "add_left_assoc" =
  let tokens : Token.t list =
    [ Token.make ~token_type:Token.NUMBER ~lexeme:"1" ~literal:(Value.Number 1.0) ~line:1
    ; Token.make ~token_type:Token.PLUS ~lexeme:"+" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"2" ~literal:(Value.Number 2.0) ~line:1
    ; Token.make ~token_type:Token.PLUS ~lexeme:"+" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"3" ~literal:(Value.Number 3.0) ~line:1
    ; Token.make ~token_type:Token.EOF ~lexeme:"" ~literal:Value.Nil ~line:1
    ]
  in
  tokens |> Parser.init |> Parser.parse |> Expr.parenthesize |> Format.printf "%s";
  [%expect {| (+ (+ 1 2) 3) |}]
;;

let%expect_test "add_mul_precedence" =
  let tokens : Token.t list =
    [ Token.make ~token_type:Token.NUMBER ~lexeme:"1" ~literal:(Value.Number 1.0) ~line:1
    ; Token.make ~token_type:Token.PLUS ~lexeme:"+" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"2" ~literal:(Value.Number 2.0) ~line:1
    ; Token.make ~token_type:Token.STAR ~lexeme:"*" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"3" ~literal:(Value.Number 3.0) ~line:1
    ; Token.make ~token_type:Token.EOF ~lexeme:"" ~literal:Value.Nil ~line:1
    ]
  in
  tokens |> Parser.init |> Parser.parse |> Expr.parenthesize |> Format.printf "%s";
  [%expect {| (+ 1 (* 2 3)) |}]
;;

let%expect_test "group" =
  let tokens : Token.t list =
    [ Token.make ~token_type:Token.LEFT_PAREN ~lexeme:"(" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"1" ~literal:(Value.Number 1.0) ~line:1
    ; Token.make ~token_type:Token.PLUS ~lexeme:"+" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"2" ~literal:(Value.Number 2.0) ~line:1
    ; Token.make ~token_type:Token.RIGHT_PAREN ~lexeme:")" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.STAR ~lexeme:"*" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"3" ~literal:(Value.Number 3.0) ~line:1
    ; Token.make ~token_type:Token.EOF ~lexeme:"" ~literal:Value.Nil ~line:1
    ]
  in
  tokens |> Parser.init |> Parser.parse |> Expr.parenthesize |> Format.printf "%s";
  [%expect {| (* (group (+ 1 2)) 3) |}]
;;

let%expect_test "incomplete_group" =
  Printexc.record_backtrace false;
  let tokens : Token.t list =
    [ Token.make ~token_type:Token.LEFT_PAREN ~lexeme:"(" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"1" ~literal:(Value.Number 1.0) ~line:1
    ; Token.make ~token_type:Token.PLUS ~lexeme:"+" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"2" ~literal:(Value.Number 2.0) ~line:1
    ; Token.make ~token_type:Token.EOF ~lexeme:"" ~literal:Value.Nil ~line:1
    ]
  in
  tokens |> Parser.init |> Parser.parse |> Expr.parenthesize |> Format.printf "%s";
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| ("Olox_lib.Parser.ParseError(\"[line 1] Error at end: Expect ')' after expression.\")") |}]
;;

let%expect_test "invalid_token" =
  Printexc.record_backtrace false;
  let tokens : Token.t list =
    [ Token.make ~token_type:Token.MINUS ~lexeme:"-" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.EOF ~lexeme:"" ~literal:Value.Nil ~line:1
    ]
  in
  tokens |> Parser.init |> Parser.parse |> Expr.parenthesize |> Format.printf "%s";
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| ("Olox_lib.Parser.ParseError(\"[line 1] Error at end: Expect expression.\")") |}]
;;
