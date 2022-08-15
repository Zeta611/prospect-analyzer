module HoleCoeffs : sig
  type t = int list

  val hole_count : 'a list -> int
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
