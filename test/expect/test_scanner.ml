open! Olox_lib

(* 4.6 Longer Lexemes *)
let%expect_test "longer_lexemes" =
  let src =
    {|  // this is a comment
        (( )){} // grouping stuff
        !*+-/=<> <= == // operators |}
  in
  src
  |> Scanner.init
  |> Scanner.scan_tokens
  |> List.iter (fun t -> Format.asprintf "%a" Token.pp t |> print_endline);
  [%expect
    {|
    LEFT_PAREN (
    LEFT_PAREN (
    RIGHT_PAREN )
    RIGHT_PAREN )
    LEFT_BRACE {
    RIGHT_BRACE }
    BANG !
    STAR *
    PLUS +
    MINUS -
    SLASH /
    EQUAL =
    LESS <
    GREATER >
    LESS_EQUAL <=
    EQUAL_EQUAL ==
    EOF |}]
;;

(* 4.6.1 String literals *)
let%expect_test "empty_string" =
  {| "" |}
  |> Scanner.init
  |> Scanner.scan_tokens
  |> List.iter (fun t -> Format.asprintf "%a" Token.pp t |> print_endline);
  [%expect {|
    STRING ""
    EOF |}]
;;

let%expect_test "simple" =
  {| "olox" |}
  |> Scanner.init
  |> Scanner.scan_tokens
  |> List.iter (fun t -> Format.asprintf "%a" Token.pp t |> print_endline);
  [%expect {|
    STRING "olox" olox
    EOF |}]
;;

let%expect_test "multi_line" =
  {| "multi
  line" |}
  |> Scanner.init
  |> Scanner.scan_tokens
  |> List.iter (fun t -> Format.asprintf "%a" Token.pp t |> print_endline);
  [%expect {|
    STRING "multi
      line" multi
      line
    EOF |}]
;;

let%expect_test "unterm" =
  Printexc.record_backtrace false;
  let unterminated = {| "H |} in
  unterminated
  |> Scanner.init
  |> Scanner.scan_tokens
  |> List.iter (fun t -> Format.asprintf "%a" Token.pp t |> print_endline)
[@@expect.uncaught_exn
  {| ("Olox_lib.Error.OLoxError(\"[line 1] Error : Unterminated String.\")") |}]
;;

(* 4.6.2 Number literals *)
let%expect_test "integer" =
  {| 12345678 |}
  |> Scanner.init
  |> Scanner.scan_tokens
  |> List.iter (fun t -> Format.asprintf "%a" Token.pp t |> print_endline);
  [%expect {|
    NUMBER 12345678 12345678.
    EOF |}]
;;

let%expect_test "decimal" =
  {| 12345678.1234 |}
  |> Scanner.init
  |> Scanner.scan_tokens
  |> List.iter (fun t -> Format.asprintf "%a" Token.pp t |> print_endline);
  [%expect {|
    NUMBER 12345678.1234 12345678.1234
    EOF |}]
;;
