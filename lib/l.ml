type prog = version * samples * expr
and version = int

(* Value types for input & output samples *)
and plain_value =
  [ `Num of number
  | `Pair of plain_value * plain_value
  ]

and input = plain_value
and output = plain_value
and samples = (input * output) list

and hole_value =
  [ `Hole
  | `Num of number
  | `Pair of hole_value * hole_value
  ]

and number = int
and id = string

and expr =
  | Hole
  | Num of number
  | Var of id
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Add of expr * expr
  | Neg of expr
  | Case of expr * id * id * expr * expr
  | If of expr * expr * expr
  | Let of id * expr * expr

(** Convert input-output value types to the `value` type *)
let hole_value_of_plain_value (v : plain_value) : hole_value =
  (v : plain_value :> hole_value)

let rec expr_of_hole_value : hole_value -> expr = function
  | `Hole -> Hole
  | `Num n -> Num n
  | `Pair (a, b) -> Pair (expr_of_hole_value a, expr_of_hole_value b)

let rec string_of_exp : expr -> string = function
  | Hole -> "[]"
  | Num n -> string_of_int n
  | Pair (e1, e2) -> "(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Fst e -> string_of_exp e ^ ".1"
  | Snd e -> string_of_exp e ^ ".2"
  | Add (e1, e2) -> string_of_exp e1 ^ " + " ^ string_of_exp e2
  | Neg e -> "-" ^ string_of_exp e
  | Case (x, y, z, e1, e2) ->
      "case " ^ string_of_exp x ^ " (" ^ y ^ ", " ^ z ^ ") " ^ string_of_exp e1
      ^ " " ^ string_of_exp e2
  | If (e_p, e_t, e_f) ->
      "if " ^ string_of_exp e_p ^ " " ^ string_of_exp e_t ^ " "
      ^ string_of_exp e_f
  | Let (x, exp, body) ->
      "let " ^ x ^ " " ^ string_of_exp exp ^ " " ^ string_of_exp body
  | Var x -> x
