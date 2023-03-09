type t = { source : string; current : int; line : int; tokens : Token.t list }

let create source = { source; current = 0; line = 1; tokens = [] }

let next t = { t with current = t.current + 1 }

let advance n t = { t with current = t.current + n }

let peek t = String.get t.source t.current

let read t = (t |> peek, t |> next)

let skip t = t

let is_over t = t.current >= String.length t.source

let get_lexeme length t = String.sub t.source (t.current - length) length

let next_is c t = (not (t |> is_over)) && t |> peek = c

let is_digit c = '0' <= c && c <= '9'

let is_alphanumeric c =
  let is_alpha c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') in
  c |> is_alpha || c |> is_digit || c == '_'

let read_until cond t =
  let rec aux buf t =
    (* assume string always have an end quote *)
    if t |> is_over || t |> peek |> cond |> not then (buf |> List.rev, t)
    else
      let c, t = read t in
      aux (c :: buf) t
  in
  let s, t = aux [] t in
  (s |> List.to_seq |> String.of_seq, t)

let add_single_char_token kind t =
  let token = Token.create kind (t |> get_lexeme 1) t.line in
  { t with tokens = token :: t.tokens }

let add_token_if exp fk tk t =
  if t |> next_is exp |> not then t |> add_single_char_token fk
  else
    let t = t |> next in
    let token = Token.create tk (t |> get_lexeme 2) t.line in
    { t with tokens = token :: t.tokens }

let add_slash_or_skip_comment t =
  if t |> next_is '/' |> not then t |> add_single_char_token SLASH
  else
    let _, t = read_until (fun c -> c <> '\n') t in
    t

let add_string_token t =
  let lexeme, t = read_until (fun c -> c <> '"') t in
  let token = Token.create STRING lexeme t.line in
  { t with tokens = token :: t.tokens } |> next

let add_number_token c t =
  let lexeme, t = read_until (fun c -> is_digit c || c = '.') t in
  let token = Token.create NUMBER (String.make 1 c ^ lexeme) t.line in
  { t with tokens = token :: t.tokens }

let add_identfier_or_keyword c t =
  let lexeme, t = read_until is_alphanumeric t in
  let lexeme = String.make 1 c ^ lexeme in
  let token =
    match lexeme with
    | "and" | "class" | "else" | "false" | "fun" | "for" | "if" | "nil" | "or"
    | "print" | "return" | "super" | "this" | "true" | "var" | "while" | "eof"
      ->
        Token.create (Token.rsvd_kind_of_string lexeme) lexeme t.line
    | _ -> Token.create IDENTIFIER lexeme t.line
  in
  { t with tokens = token :: t.tokens }

let scan_token t =
  let c, t = read t in
  t
  |>
  match c with
  | '(' -> add_single_char_token LEFT_PAREN
  | ')' -> add_single_char_token RIGHT_PAREN
  | '{' -> add_single_char_token LEFT_BRACE
  | '}' -> add_single_char_token RIGHT_BRACE
  | ',' -> add_single_char_token COMMA
  | '.' -> add_single_char_token DOT
  | '-' -> add_single_char_token MINUS
  | '+' -> add_single_char_token PLUS
  | ';' -> add_single_char_token SEMICOLON
  | '*' -> add_single_char_token STAR
  | '!' -> add_token_if '=' BANG BANG_EQUAL
  | '=' -> add_token_if '=' EQUAL EQUAL_EQUAL
  | '>' -> add_token_if '=' GREATER GREATER_EQUAL
  | '<' -> add_token_if '=' LESS LESS_EQUAL
  | '/' -> add_slash_or_skip_comment
  | '\n' -> fun t -> { t with line = t.line + 1 }
  | '"' -> add_string_token
  | c when c |> is_digit -> add_number_token c
  | c when c |> is_alphanumeric -> add_identfier_or_keyword c
  | ' ' | '\r' | '\t' -> skip
  | _ ->
      Error.error t.line
        (Format.sprintf "Unexpected character: %c at %d:%d" c t.line t.current)

let scan_tokens =
  let rec loop t =
    if t |> is_over then t.tokens |> List.rev else t |> scan_token |> loop
  in
  loop
