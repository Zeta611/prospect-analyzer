type prog = version * samples * expr
and version = int

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

val hole_value_of_plain_value : plain_value -> hole_value

val expr_of_value :
  ([< `Hole | `Num of number | `Pair of 'a * 'a ] as 'a) -> expr

val string_of_exp : expr -> string
