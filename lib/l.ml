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
let rec vvalue_to_hvalue = function
  | VNum n -> HNum n
  | VPair (a, b) -> HPair (vvalue_to_hvalue a, vvalue_to_hvalue b)
