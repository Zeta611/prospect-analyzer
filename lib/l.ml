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

(* Convert input-output value types to the `value` type *)
let rec hvalue_of_vvalue = function
  | VNum n -> HNum n
  | VPair (a, b) -> HPair (hvalue_of_vvalue a, hvalue_of_vvalue b)

let rec expr_of_hvalue = function
  | HHole -> Hole
  | HNum n -> Num n
  | HPair (a, b) -> Pair (expr_of_hvalue a, expr_of_hvalue b)
