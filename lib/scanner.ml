type t =
  { source : string
  ; tokens : Token.t list
  ; start : int
  ; current : int
  ; line : int
  }

let init source = { source; tokens = []; start = 0; current = 0; line = 1 }
let is_at_end t = t.current >= String.length t.source

let get_char t =
  if t.current > String.length t.source
  then None
  else Some (String.get t.source (t.current - 1))
;;

let peek t = if is_at_end t then None else Some (String.get t.source t.current)
let get_lexeme t = String.sub t.source t.start (t.current - t.start)
let advance t = { t with current = t.current + 1 }
let newline t = { t with line = t.line + 1 }

let add_token token_type t =
  let token : Token.t =
    { token_type; lexeme = get_lexeme t; literal = ""; line = t.line }
  in
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
     | ' ' | '\r' | '\t' -> next
     | '\n' -> newline next
     | _ -> Error.error next.line "Unexpected character.")
  | None -> next
;;

let rec scan_tokens t =
  if is_at_end t
  then (
    let eof_token : Token.t =
      { token_type = Token.EOF; lexeme = ""; literal = ""; line = t.line }
    in
    List.rev (eof_token :: t.tokens))
  else (
    let scanner = { t with start = t.current } in
    scan_tokens (scan_token scanner))
;;
