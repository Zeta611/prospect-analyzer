exception TypeError of string

type id = string

type ty =
  | TyInt
  | TyPair of ty * ty
  | TyVar of tyvar

and tyvar = string

type type_check_info = {
  hole_type : ty;
  exp_type : ty;
  taken_path : Path.path;
  tagged_exp : Path.tagged_exp;
  tag_list : Path.tag list;
}

val type_of_plain_value : ([< `Num of 'b | `Pair of 'a * 'a ] as 'a) -> ty

exception UnificationError

val print_type_check_info : type_check_info -> unit
val print_type_list : type_check_info list -> unit
val type_check : L.expr -> ty -> type_check_info list
