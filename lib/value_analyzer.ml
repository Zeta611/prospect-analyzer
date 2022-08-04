open L

(* Types and exceptions *)

module HoleCoeffs = struct
  open Monads.List

  type t = int list

  let is_zero hole_coeffs =
    List.find_opt (( <> ) 0) hole_coeffs |> Option.is_none

  let length = List.length
  let make_num = return
  let add_hole ?coeff:(k = 1) hole_coeffs = hole_coeffs @ make_num k

  let rec ( +! ) h1 h2 =
    match (h1, h2) with
    | h, [] | [], h -> h
    | x :: xs, y :: ys -> (x + y) :: (xs +! ys)

  let ( ~-! ) h =
    let+ k = h in
    -k
end

type value =
  | VNum of HoleCoeffs.t
  | VPair of value * value

let rec value_of_hole_value : L.hole_value -> value =
  let open HoleCoeffs in
  function
  | `Hole -> VNum (add_hole (make_num 0))
  | `Num n -> VNum (make_num n)
  | `Pair (h1, h2) -> VPair (value_of_hole_value h1, value_of_hole_value h2)

let rec string_of_value = function
  | VNum hole_coeffs -> (
      match hole_coeffs with
      | [] -> failwith "Empty HoleCoeffs: This is a programming error!"
      | [ n ] -> string_of_int n
      | n :: ks ->
          string_of_int n ^ " + "
          ^
          let rec string_of index = function
            | [] -> ""
            | k :: ks when k <> 0 ->
                (if k <> 1 then string_of_int k else "")
                ^ "[" ^ string_of_int index ^ "]"
                ^ if ks <> [] then " + " ^ string_of (index + 1) ks else ""
            | _ :: ks -> string_of (index + 1) ks
          in
          string_of 1 ks)
  | VPair (h1, h2) -> "(" ^ string_of_value h1 ^ ", " ^ string_of_value h2 ^ ")"

type id = string
type env = id -> value

type comp_op =
  | Eq (* =0 *)
  | Ne (* ≠0 *)

(* | Lt (* <0 *) *)
(* | Gt (* >0 *) *)
(* | Le (* ≤0 *) *)
(* | Ge (* ≥0 *) *)

type cond_eqn = HoleCoeffs.t * comp_op

exception TypeError of string
exception RunError of string

let raiseTypeError expected op =
  let expected, current =
    let n, p = ("number", "pair") in
    match expected with `Num -> (n, p) | `Pair -> (p, n)
  in
  let msg = Printf.sprintf "%s: %s expected, not a %s" op expected current in
  raise (TypeError msg)

let empty_env (_ : id) : value = raise (RunError "undefined variable")

(** Environment augmentation. Use @: to bind (x, v) to f *)
let ( @: ) (x, v) e y = if y = x then v else e y

let rec eval env expr =
  let open HoleCoeffs in
  match expr with
  | Hole -> VNum (add_hole (make_num 0))
  | Num n -> VNum (make_num n)
  | Pair (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      VPair (v1, v2)
  | Fst e -> (
      match eval env e with
      | VPair (fst, _) -> fst
      | VNum _ -> raiseTypeError `Pair "FIRST")
  | Snd e -> (
      match eval env e with
      | VPair (_, snd) -> snd
      | VNum _ -> raiseTypeError `Pair "SECOND")
  | Add (e1, e2) -> (
      match eval env e1 with
      | VNum lhs_n -> (
          match eval env e2 with
          | VNum rhs_n -> VNum (lhs_n +! rhs_n)
          | VPair _ -> raiseTypeError `Num "ADD")
      | VPair _ -> raiseTypeError `Num "ADD")
  | Neg e -> (
      match eval env e with
      | VNum n -> VNum ~-!n
      | VPair _ -> raiseTypeError `Num "NEGATE")
  | Case (x, y, z, e1, e2) -> (
      let v = eval env x in
      match v with
      | VPair (v1, v2) ->
          let env' = (y, v1) @: (z, v2) @: env in
          eval env' e1
      | _ -> eval env e2)
  | If (pred, true_e, false_e) -> (
      let v = eval env pred in
      match v with
      | VNum n -> eval env (if is_zero n then false_e else true_e)
      | VPair _ -> raiseTypeError `Num "IF")
  | Let (x, exp, body) ->
      let v = eval env exp in
      eval ((x, v) @: env) body
  | Var x -> env x
