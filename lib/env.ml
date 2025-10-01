exception EnvError of string

module Values = Map.Make (String)

type t = { values : Value.t Values.t }

let empty = { values = Values.empty }

let get (name : Token.t) t =
  match Values.find_opt name.lexeme t.values with
  | Some v -> v
  | None -> raise (EnvError (Format.sprintf "Undefined variable '%s'." name.lexeme))
;;

let define name value t = { values = Values.add name value t.values }
