type t =
  | Expression of Expr.t
  | Var of Token.t * Expr.t
  | Print of Expr.t
[@@deriving show]

let accept t env =
  match t with
  | Expression expr ->
    Expr.evaluate expr env |> ignore;
    env
  | Var (name, init) ->
    let value =
      match init with
      | Expr.Nil -> Value.Nil
      | _ -> Expr.evaluate init env
    in
    Env.define name.lexeme value env
  | Print expr ->
    Value.show (Expr.evaluate expr env) |> print_endline;
    env
;;
