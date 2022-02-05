type exp = Num of number
         | Var of id
         | Pair of exp * exp
         | Ind1 of exp
         | Ind2 of exp
         | Sum of exp * exp
         | Neg of exp
         | Case of exp * exp * exp * exp
         | If of exp * exp * exp
         | Let of id * exp * exp
and number = int
and id = string
