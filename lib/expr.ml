module Value = struct
  type t = Bool of bool | Number of float | String of string | Null

  let to_string t =
    match t with
    | Bool b -> string_of_bool b
    | Number f ->
        if Float.is_integer f then f |> int_of_float |> string_of_int
        else f |> string_of_float
    | String s -> s
    | Null -> "null"

  let unary_arithmetic operator t =
    match t with Number n -> Number (operator n) | _ -> failwith "unreachable"

  let unary_logic operator t =
    match t with Bool b -> Bool (operator b) | _ -> failwith "unreachable"

  let binary_arithmetic operator lt rt =
    match (lt, rt) with
    | Number l, Number r -> Number (operator l r)
    | _ -> failwith "unreachable"

  let binary_comparison operator lt rt =
    match (lt, rt) with
    | Number l, Number r -> Bool (operator l r)
    | _ -> failwith "unreachable"

  let binary_string operator lt rt =
    match (lt, rt) with
    | String l, String r -> String (operator l r)
    | _ -> failwith "unreachable"

  let equal lt rt =
    match (lt, rt) with
    | Bool l, Bool r -> Bool (l = r)
    | Number l, Number r -> Bool (l = r)
    | String l, String r -> Bool (l = r)
    | Null, Null -> Bool true
    | _ -> Bool false
end

type t =
  | Literal of literal
  | Unary of unary
  | Binary of binary
  | Grouping of grouping
  | Nil
[@@deriving show { with_path = false }]

and literal = { value : Value.t }

and unary = { unop : Token.t; operand : t }

and binary = { left : t; binop : Token.t; right : t }

and grouping = { expression : t }

let rec to_string t =
  match t with
  | Literal { value } -> value |> Value.to_string
  | Unary { unop; operand } ->
      Printf.sprintf "%s%s" unop.lexeme (to_string operand)
  | Binary { left; binop; right } ->
      Printf.sprintf "%s%s%s" (to_string left) binop.lexeme (to_string right)
  | Grouping { expression } ->
      Printf.sprintf "(group %s)" (to_string expression)
  | Nil -> ""
