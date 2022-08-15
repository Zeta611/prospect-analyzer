type number = int
type id = string
type tag = int

type path =
  | PtNil
  | PtPair of path * path
  | PtAdd of path * path
  | PtCaseP of path * path
  | PtCaseN of path * path
  | PtIfTru of path * path
  | PtIfFls of path * path
  | PtLet of path * path

val string_of_path : path -> string

type tagged_exp =
  | TgHole of tag
  | TgNum of tag * number
  | TgVar of tag * id
  | TgPair of tag * tagged_exp * tagged_exp
  | TgFst of tag * tagged_exp
  | TgSnd of tag * tagged_exp
  | TgAdd of tag * tagged_exp * tagged_exp
  | TgNeg of tag * tagged_exp
  | TgCase of tag * tagged_exp * id * id * tagged_exp * tagged_exp
  | TgIf of tag * tagged_exp * tagged_exp * tagged_exp
  | TgLet of tag * id * tagged_exp * tagged_exp

val tag_exp : L.expr -> tagged_exp
