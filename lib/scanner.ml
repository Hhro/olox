exception ScanError of int * string

type t =
  { source : string
  ; tokens : Token.t list
  ; start : int
  ; current : int
  ; line : int
  }

module StringMap = Map.Make (String)

let keyword_map =
  StringMap.of_seq
  @@ List.to_seq
       [ "and", Token.AND
       ; "class", Token.CLASS
       ; "else", Token.ELSE
       ; "false", Token.FALSE
       ; "for", Token.FOR
       ; "fun", Token.FUN
       ; "if", Token.IF
       ; "nil", Token.NIL
       ; "or", Token.OR
       ; "print", Token.PRINT
       ; "return", Token.RETURN
       ; "super", Token.SUPER
       ; "this", Token.THIS
       ; "true", Token.TRUE
       ; "var", Token.VAR
       ; "while", Token.WHILE
       ]
;;

let init source = { source; tokens = []; start = 0; current = 0; line = 1 }
let is_at_end t = t.current >= String.length t.source

let get_char t =
  if t.current > String.length t.source
  then None
  else Some (String.get t.source (t.current - 1))
;;

let get_lexeme t = String.sub t.source t.start (t.current - t.start)
let advance t = { t with current = t.current + 1 }
let newline t = { t with line = t.line + 1 }
let peek t = if is_at_end t then None else Some (String.get t.source t.current)

let peek_next t =
  let next = advance t in
  if is_at_end next then None else Some (String.get next.source next.current)
;;

let is_digit c =
  match c with
  | '0' .. '9' -> true
  | _ -> false
;;

let is_alpha c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let is_alphanumeric c = is_alpha c || is_digit c

let is_some_digit optc =
  match optc with
  | Some c -> is_digit c
  | None -> false
;;

let add_token ?(literal = Value.Nil) token_type t =
  let token : Token.t = { token_type; lexeme = get_lexeme t; literal; line = t.line } in
  { t with tokens = token :: t.tokens }
;;

let add_token_conditionally true_type false_type t =
  let next = advance t in
  match get_char next with
  | Some '=' -> add_token true_type next
  | _ -> add_token false_type t
;;

let add_slash_or_skip_comment t =
  let rec skip_comment t =
    let next = advance t in
    match get_char next with
    | Some '\n' -> t
    | None -> t
    | _ -> skip_comment next
  in
  let next = advance t in
  match get_char next with
  | Some '/' -> skip_comment next
  | _ -> add_token SLASH t
;;

let add_string_token t =
  let rec consume_string t =
    match peek t with
    | Some '"' ->
      let next = advance t in
      let literal =
        Value.String
          (String.sub next.source (next.start + 1) (next.current - next.start - 2))
      in
      add_token ~literal STRING next
    | Some '\n' -> advance t |> newline |> consume_string
    | Some _ -> consume_string (advance t)
    | None -> raise (ScanError (t.line, "Unterminated String."))
  in
  consume_string t
;;

let add_number_token t =
  let rec consume_digit t =
    match peek t with
    | Some c when is_digit c -> consume_digit (advance t)
    | Some '.' when peek_next t |> is_some_digit -> consume_digit (advance t)
    | _ ->
      let text = String.sub t.source t.start (t.current - t.start) in
      let literal =
        try Value.Number (float_of_string text) with
        | _ -> raise (ScanError (t.line, "Invalid Number."))
      in
      add_token ~literal NUMBER t
  in
  consume_digit t
;;

let add_identifier_token t =
  let rec consume_alnum t =
    match peek t with
    | Some c when is_alphanumeric c -> consume_alnum (advance t)
    | _ ->
      let text = String.sub t.source t.start (t.current - t.start) in
      if StringMap.mem text keyword_map
      then add_token (StringMap.find text keyword_map) t
      else add_token IDENTIFIER t
  in
  consume_alnum t
;;

let scan_token t =
  let next = advance t in
  match get_char next with
  | Some c ->
    (match c with
     | '(' -> add_token LEFT_PAREN next
     | ')' -> add_token RIGHT_PAREN next
     | '{' -> add_token LEFT_BRACE next
     | '}' -> add_token RIGHT_BRACE next
     | ',' -> add_token COMMA next
     | '.' -> add_token DOT next
     | '-' -> add_token MINUS next
     | '+' -> add_token PLUS next
     | ';' -> add_token SEMICOLON next
     | '*' -> add_token STAR next
     | '!' -> add_token_conditionally BANG_EQUAL BANG next
     | '=' -> add_token_conditionally EQUAL_EQUAL EQUAL next
     | '<' -> add_token_conditionally LESS_EQUAL LESS next
     | '>' -> add_token_conditionally GREATER_EQUAL GREATER next
     | '/' -> add_slash_or_skip_comment next
     | '"' -> add_string_token next
     | c when is_digit c -> add_number_token next
     | c when is_alpha c -> add_identifier_token next
     | ' ' | '\r' | '\t' -> next
     | '\n' -> newline next
     | _ -> raise (ScanError (next.line, "Unexpected character.")))
  | None -> next
;;

let rec scan_tokens t =
  if is_at_end t
  then (
    let eof_token : Token.t =
      { token_type = Token.EOF; lexeme = ""; literal = Value.Nil; line = t.line }
    in
    List.rev (eof_token :: t.tokens))
  else (
    let scanner = { t with start = t.current } in
    scan_tokens (scan_token scanner))
;;
