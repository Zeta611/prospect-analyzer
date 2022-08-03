type prog = version * samples * expr
and version = int

(* Value types for input & output samples *)
and vvalue =
  | VNum of number
  | VPair of vvalue * vvalue

and input = vvalue
and output = vvalue
and samples = (input * output) list

and hvalue =
  | HHole
  | HNum of number
  | HPair of hvalue * hvalue

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
let rec hvalue_of_vvalue = function
  | VNum n -> HNum n
  | VPair (a, b) -> HPair (hvalue_of_vvalue a, hvalue_of_vvalue b)

let rec expr_of_hvalue = function
  | HHole -> Hole
  | HNum n -> Num n
  | HPair (a, b) -> Pair (expr_of_hvalue a, expr_of_hvalue b)

let rec string_of_exp = function
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
