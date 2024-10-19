open! Olox_lib

let print_tokens tokens =
  tokens |> List.iter (fun t -> Format.asprintf "%a" Token.pp t |> print_endline)
;;

(* 4.6 Longer Lexemes *)
let%expect_test "longer_lexemes" =
  let src =
    {|  // this is a comment
        (( )){} // grouping stuff
        !*+-/=<> <= == // operators |}
  in
  src |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
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
  {| "" |} |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
  [%expect {|
    STRING ""
    EOF |}]
;;

let%expect_test "simple" =
  {| "olox" |} |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
  [%expect {|
    STRING "olox" olox
    EOF |}]
;;

let%expect_test "multi_line" =
  {| "multi
  line" |} |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
  [%expect {|
    STRING "multi
      line" multi
      line
    EOF |}]
;;

let%expect_test "unterm" =
  Printexc.record_backtrace false;
  let unterminated = {| "H |} in
  unterminated |> Scanner.init |> Scanner.scan_tokens |> print_tokens
[@@expect.uncaught_exn
  {| ("Olox_lib.Error.OLoxError(\"[line 1] Error : Unterminated String.\")") |}]
;;

(* 4.6.2 Number literals *)
let%expect_test "integer" =
  {| 12345678 |} |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
  [%expect {|
    NUMBER 12345678 12345678.
    EOF |}]
;;

let%expect_test "decimal" =
  {| 12345678.1234 |} |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
  [%expect {|
    NUMBER 12345678.1234 12345678.1234
    EOF |}]
;;

(* 4.7 Reserved Words and Identifiers *)
let%expect_test "keyword" =
  {| or |} |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
  [%expect {|
    OR or
    EOF |}]
;;

let%expect_test "identifier" =
  {| var x |} |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
  [%expect {|
    VAR var
    IDENTIFIER x
    EOF |}]
;;

let%expect_test "member_of_number" =
  {| 3.sqrt() |} |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
  [%expect {|
    NUMBER 3 3.
    DOT .
    IDENTIFIER sqrt
    LEFT_PAREN (
    RIGHT_PAREN )
    EOF |}]
;;
