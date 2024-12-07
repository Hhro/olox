exception TypeError of string

type t =
  | Number of float [@printer fun fmt -> fprintf fmt "%g"]
  | String of string [@printer fun fmt -> fprintf fmt "\"%s\""]
  | Bool of bool [@printer fun fmt -> fprintf fmt "%b"]
  | Nil
[@@deriving show { with_path = false }]

let isTruthy t =
  match t with
  | Nil -> false
  | Bool b -> b
  | _ -> true
;;

let isEqual lt rt =
  match lt, rt with
  | Number l, Number r -> l == r
  | String l, String r -> l == r
  | Bool l, Bool r -> l == r
  | Nil, Nil -> true
  | _, _ -> raise (TypeError "Operands must have same type.")
;;

let ( ~- ) t =
  match t with
  | Number f -> Number (-.f)
  | _ -> raise (TypeError "Operand must be a number.")
;;

let ( ! ) t = Bool (not (isTruthy t))

let ( > ) lt rt =
  match lt, rt with
  | Number l, Number r -> Bool (l > r)
  | _, _ -> raise (TypeError "Operands must be numbers.")
;;

let ( >= ) lt rt =
  match lt, rt with
  | Number l, Number r -> Bool (l >= r)
  | _, _ -> raise (TypeError "Operands must be numbers.")
;;

let ( < ) lt rt =
  match lt, rt with
  | Number l, Number r -> Bool (l < r)
  | _, _ -> raise (TypeError "Operands must be numbers.")
;;

let ( <= ) lt rt =
  match lt, rt with
  | Number l, Number r -> Bool (l <= r)
  | _, _ -> raise (TypeError "Operands must be numbers.")
;;

let ( != ) lt rt = Bool (not (isEqual lt rt))
let ( == ) lt rt = Bool (isEqual lt rt)

let ( + ) lt rt =
  match lt, rt with
  | Number l, Number r -> Number (l +. r)
  | String l, String r -> String (l ^ r)
  | _, _ -> raise (TypeError "Operands must be numbers or strings.")
;;

let ( - ) lt rt =
  match lt, rt with
  | Number l, Number r -> Number (l -. r)
  | _, _ -> raise (TypeError "Operands must be numbers.")
;;

let ( / ) lt rt =
  match lt, rt with
  | Number l, Number r -> Number (l /. r)
  | _, _ -> raise (TypeError "Operands must be numbers.")
;;

let ( * ) lt rt =
  match lt, rt with
  | Number l, Number r -> Number (l *. r)
  | _, _ -> raise (TypeError "Operands must be numbers.")
;;
