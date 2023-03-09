type kind =
  (* single-character tokens *)
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  (* one or two character tokens *)
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  (* literals *)
  | IDENTIFIER
  | STRING
  | NUMBER
  (* reserved *)
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF

type t = { kind : kind; lexeme : string; line : int }

let create kind lexeme line = { kind; lexeme; line }

let string_of_kind kind =
  match kind with
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | COMMA -> "COMMA"
  | DOT -> "DOT"
  | MINUS -> "MINUS"
  | PLUS -> "PLUS"
  | SEMICOLON -> "SEMICOLON"
  | SLASH -> "SLASH"
  | STAR -> "STAR"
  | BANG -> "BANG"
  | BANG_EQUAL -> "BANG_EQUAL"
  | EQUAL -> "EQUAL"
  | EQUAL_EQUAL -> "EQUAL_EQUAL"
  | GREATER -> "GREATER"
  | GREATER_EQUAL -> "GREATER_EQUAL"
  | LESS -> "LESS"
  | LESS_EQUAL -> "LESS_EQUAL"
  | IDENTIFIER -> "IDENTIFIER"
  | STRING -> "STRING"
  | NUMBER -> "NUMBER"
  | AND -> "AND"
  | CLASS -> "CLASS"
  | ELSE -> "ELSE"
  | FALSE -> "FALSE"
  | FUN -> "FUN"
  | FOR -> "FOR"
  | IF -> "IF"
  | NIL -> "NIL"
  | OR -> "OR"
  | PRINT -> "PRINT"
  | RETURN -> "RETURN"
  | SUPER -> "SUPER"
  | THIS -> "THIS"
  | TRUE -> "TRUE"
  | VAR -> "VAR"
  | WHILE -> "WHILE"
  | EOF -> "EOF"

let rsvd_kind_of_string s =
  match s with
  | "and" -> AND
  | "class" -> CLASS
  | "else" -> ELSE
  | "false" -> FALSE
  | "fun" -> FUN
  | "for" -> FOR
  | "if" -> IF
  | "nil" -> NIL
  | "or" -> OR
  | "print" -> PRINT
  | "return" -> RETURN
  | "super" -> SUPER
  | "this" -> THIS
  | "true" -> TRUE
  | "var" -> VAR
  | "while" -> WHILE
  | "eof" -> EOF
  | _ -> failwith "unreachable"

let to_string t = Format.sprintf "%s %s" (string_of_kind t.kind) t.lexeme
