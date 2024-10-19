open! Olox_lib

let%expect_test "simple" =
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
