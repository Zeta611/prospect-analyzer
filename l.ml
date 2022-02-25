type expr = Num of number
         | Var of id
         | Pair of expr * expr
         | Fst of expr
         | Snd of expr
         | Add of expr * expr
         | Neg of expr
         | Case of expr * expr * expr * expr
         | If of expr * expr * expr
         | Let of id * expr * expr
and number = int
and id = string
