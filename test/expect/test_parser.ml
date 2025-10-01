open! Olox_lib

(* 6.2.1 recursive descent parsing *)
let%expect_test "add" =
  let env = Env.empty in
  let tokens : Token.t list =
    [ Token.make ~token_type:Token.NUMBER ~lexeme:"1" ~literal:(Value.Number 1.0) ~line:1
    ; Token.make ~token_type:Token.PLUS ~lexeme:"+" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"2" ~literal:(Value.Number 2.0) ~line:1
    ; Token.make ~token_type:Token.SEMICOLON ~lexeme:";" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.EOF ~lexeme:"" ~literal:Value.Nil ~line:1
    ]
  in
  match tokens |> Parser.init |> Parser.parse |> List.hd with
  | Stmt.Expression e ->
    Expr.parenthesize e env |> Format.printf "%s";
    [%expect {| (+ 1 2) |}]
  | _ -> failwith "unreachable"
;;

let%expect_test "add_left_assoc" =
  let env = Env.empty in
  let tokens : Token.t list =
    [ Token.make ~token_type:Token.NUMBER ~lexeme:"1" ~literal:(Value.Number 1.0) ~line:1
    ; Token.make ~token_type:Token.PLUS ~lexeme:"+" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"2" ~literal:(Value.Number 2.0) ~line:1
    ; Token.make ~token_type:Token.PLUS ~lexeme:"+" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"3" ~literal:(Value.Number 3.0) ~line:1
    ; Token.make ~token_type:Token.SEMICOLON ~lexeme:";" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.EOF ~lexeme:"" ~literal:Value.Nil ~line:1
    ]
  in
  match tokens |> Parser.init |> Parser.parse |> List.hd with
  | Stmt.Expression e ->
    Expr.parenthesize e env |> Format.printf "%s";
    [%expect {| (+ (+ 1 2) 3) |}]
  | _ -> failwith "unreachable"
;;

let%expect_test "add_mul_precedence" =
  let env = Env.empty in
  let tokens : Token.t list =
    [ Token.make ~token_type:Token.NUMBER ~lexeme:"1" ~literal:(Value.Number 1.0) ~line:1
    ; Token.make ~token_type:Token.PLUS ~lexeme:"+" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"2" ~literal:(Value.Number 2.0) ~line:1
    ; Token.make ~token_type:Token.STAR ~lexeme:"*" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"3" ~literal:(Value.Number 3.0) ~line:1
    ; Token.make ~token_type:Token.SEMICOLON ~lexeme:";" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.EOF ~lexeme:"" ~literal:Value.Nil ~line:1
    ]
  in
  match tokens |> Parser.init |> Parser.parse |> List.hd with
  | Stmt.Expression e ->
    Expr.parenthesize e env |> Format.printf "%s";
    [%expect {| (+ 1 (* 2 3)) |}]
  | _ -> failwith "unreachable"
;;

let%expect_test "group" =
  let env = Env.empty in
  let tokens : Token.t list =
    [ Token.make ~token_type:Token.LEFT_PAREN ~lexeme:"(" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"1" ~literal:(Value.Number 1.0) ~line:1
    ; Token.make ~token_type:Token.PLUS ~lexeme:"+" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"2" ~literal:(Value.Number 2.0) ~line:1
    ; Token.make ~token_type:Token.RIGHT_PAREN ~lexeme:")" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.STAR ~lexeme:"*" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"3" ~literal:(Value.Number 3.0) ~line:1
    ; Token.make ~token_type:Token.SEMICOLON ~lexeme:";" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.EOF ~lexeme:"" ~literal:Value.Nil ~line:1
    ]
  in
  match tokens |> Parser.init |> Parser.parse |> List.hd with
  | Stmt.Expression e ->
    Expr.parenthesize e env |> Format.printf "%s";
    [%expect {| (* (group (+ 1 2)) 3) |}]
  | _ -> failwith "unreachable"
;;

let%expect_test "incomplete_group" =
  let env = Env.empty in
  Printexc.record_backtrace false;
  let tokens : Token.t list =
    [ Token.make ~token_type:Token.LEFT_PAREN ~lexeme:"(" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"1" ~literal:(Value.Number 1.0) ~line:1
    ; Token.make ~token_type:Token.PLUS ~lexeme:"+" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.NUMBER ~lexeme:"2" ~literal:(Value.Number 2.0) ~line:1
    ; Token.make ~token_type:Token.SEMICOLON ~lexeme:";" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.EOF ~lexeme:"" ~literal:Value.Nil ~line:1
    ]
  in
  match tokens |> Parser.init |> Parser.parse |> List.hd with
  | Stmt.Expression e ->
    Expr.parenthesize e env |> Format.printf "%s";
    [%expect {|
      [line 1] Error at ';': Expect ')' after expression.
      nil |}]
  | _ -> failwith "unreachable"
;;

let%expect_test "invalid_token" =
  let env = Env.empty in
  Printexc.record_backtrace false;
  let tokens : Token.t list =
    [ Token.make ~token_type:Token.MINUS ~lexeme:"-" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.SEMICOLON ~lexeme:";" ~literal:Value.Nil ~line:1
    ; Token.make ~token_type:Token.EOF ~lexeme:"" ~literal:Value.Nil ~line:1
    ]
  in
  match tokens |> Parser.init |> Parser.parse |> List.hd with
  | Stmt.Expression e ->
    Expr.parenthesize e env |> Format.printf "%s";
    [%expect {|
      [line 1] Error at ';': Expect expression.
      nil |}]
  | _ -> failwith "unreachable"
;;
