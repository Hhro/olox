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
    { token_type = LEFT_PAREN; lexeme = "("; literal = Nil; line = 2 }
    { token_type = LEFT_PAREN; lexeme = "("; literal = Nil; line = 2 }
    { token_type = RIGHT_PAREN; lexeme = ")"; literal = Nil; line = 2 }
    { token_type = RIGHT_PAREN; lexeme = ")"; literal = Nil; line = 2 }
    { token_type = LEFT_BRACE; lexeme = "{"; literal = Nil; line = 2 }
    { token_type = RIGHT_BRACE; lexeme = "}"; literal = Nil; line = 2 }
    { token_type = BANG; lexeme = "!"; literal = Nil; line = 3 }
    { token_type = STAR; lexeme = "*"; literal = Nil; line = 3 }
    { token_type = PLUS; lexeme = "+"; literal = Nil; line = 3 }
    { token_type = MINUS; lexeme = "-"; literal = Nil; line = 3 }
    { token_type = SLASH; lexeme = "/"; literal = Nil; line = 3 }
    { token_type = EQUAL; lexeme = "="; literal = Nil; line = 3 }
    { token_type = LESS; lexeme = "<"; literal = Nil; line = 3 }
    { token_type = GREATER; lexeme = ">"; literal = Nil; line = 3 }
    { token_type = LESS_EQUAL; lexeme = "<="; literal = Nil; line = 3 }
    { token_type = EQUAL_EQUAL; lexeme = "=="; literal = Nil; line = 3 }
    { token_type = EOF; lexeme = ""; literal = Nil; line = 3 } |}]
;;

(* 4.6.1 String literals *)
let%expect_test "empty_string" =
  {| "" |} |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
  [%expect
    {|
    { token_type = STRING; lexeme = "\"\""; literal = ""; line = 1 }
    { token_type = EOF; lexeme = ""; literal = Nil; line = 1 } |}]
;;

let%expect_test "simple" =
  {| "olox" |} |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
  [%expect
    {|
    { token_type = STRING; lexeme = "\"olox\""; literal = "olox"; line = 1 }
    { token_type = EOF; lexeme = ""; literal = Nil; line = 1 } |}]
;;

let%expect_test "multi_line" =
  {| "multi
  line" |} |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
  [%expect
    {|
    { token_type = STRING; lexeme = "\"multi\n  line\"";
      literal = "multi
      line"; line = 2 }
    { token_type = EOF; lexeme = ""; literal = Nil; line = 2 } |}]
;;

let%expect_test "unterm" =
  Printexc.record_backtrace false;
  let unterminated = {| "H |} in
  unterminated |> Scanner.init |> Scanner.scan_tokens |> print_tokens
[@@expect.uncaught_exn
  {| ("Olox_lib.Scanner.ScanError(1, \"Unterminated String.\")") |}]
;;

(* 4.6.2 Number literals *)
let%expect_test "integer" =
  {| 12345678 |} |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
  [%expect
    {|
    { token_type = NUMBER; lexeme = "12345678"; literal = 1.23457e+07; line = 1 }
    { token_type = EOF; lexeme = ""; literal = Nil; line = 1 } |}]
;;

let%expect_test "decimal" =
  {| 12345678.1234 |} |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
  [%expect
    {|
    { token_type = NUMBER; lexeme = "12345678.1234"; literal = 1.23457e+07;
      line = 1 }
    { token_type = EOF; lexeme = ""; literal = Nil; line = 1 } |}]
;;

(* 4.7 Reserved Words and Identifiers *)
let%expect_test "keyword" =
  {| or |} |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
  [%expect
    {|
    { token_type = OR; lexeme = "or"; literal = Nil; line = 1 }
    { token_type = EOF; lexeme = ""; literal = Nil; line = 1 } |}]
;;

let%expect_test "identifier" =
  {| var x |} |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
  [%expect
    {|
    { token_type = VAR; lexeme = "var"; literal = Nil; line = 1 }
    { token_type = IDENTIFIER; lexeme = "x"; literal = Nil; line = 1 }
    { token_type = EOF; lexeme = ""; literal = Nil; line = 1 } |}]
;;

let%expect_test "member_of_number" =
  {| 3.sqrt() |} |> Scanner.init |> Scanner.scan_tokens |> print_tokens;
  [%expect
    {|
    { token_type = NUMBER; lexeme = "3"; literal = 3; line = 1 }
    { token_type = DOT; lexeme = "."; literal = Nil; line = 1 }
    { token_type = IDENTIFIER; lexeme = "sqrt"; literal = Nil; line = 1 }
    { token_type = LEFT_PAREN; lexeme = "("; literal = Nil; line = 1 }
    { token_type = RIGHT_PAREN; lexeme = ")"; literal = Nil; line = 1 }
    { token_type = EOF; lexeme = ""; literal = Nil; line = 1 } |}]
;;
