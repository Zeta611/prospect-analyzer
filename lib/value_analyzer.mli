module HoleCoeffs : sig
  type t = int list

  type cond_checker = {
    context : Z3.context;
    solver : Z3.Solver.solver;
  }

  val zero : int -> int list
  val make : index:int -> k:int -> hole_cnt:int -> int list
  val hole_count : 'a list -> int

  val ( +! ) :
    int Monads.ListBaseApplicative.t ->
    int Monads.ListBaseApplicative.t ->
    int Monads.ListBaseApplicative.t

  val ( ~-! ) :
    int Monads.ListBaseApplicative.t -> int Monads.ListBaseApplicative.t

  val can_be_zero : cond_checker -> t -> bool
  val can_be_nonzero : cond_checker -> t -> bool
end

type value =
  | VNum of HoleCoeffs.t
  | VPair of value * value

val value_of_plain_value :
  ([< `Num of int | `Pair of 'a * 'a ] as 'a) -> hole_cnt:int -> value

val count_holes : Shape_analyzer.ty -> L.number
val value_of_hole_type : Shape_analyzer.ty -> value
val string_of_value : value -> string

type id = string
type env = id -> value

exception TypeError of string
exception RunError of string
exception PathError of string

val ( @: ) : 'a * 'b -> ('a -> 'b) -> 'a -> 'b

val eval :
  (L.id -> value) ->
  L.expr ->
  Path.path ->
  Shape_analyzer.ty ->
  L.plain_value ->
  bool * value
