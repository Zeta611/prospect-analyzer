type prog = version * samples * expr
and version = int

and value =
  | VNum of number
  | VPair of value * value

and input = value
and output = value
and samples = (input * output) list
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
