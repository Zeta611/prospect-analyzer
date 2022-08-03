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

(** Eventually, hvalue type must be generalized and use the following as well *)
type hvalue' =
  | HNum' of HoleCoeffs.t
  | HPair' of hvalue' * hvalue'

let rec hvalue'_of_hvalue =
  let open HoleCoeffs in
  function
  | HHole -> HNum' (add_hole (make_num 0))
  | HNum n -> HNum' (make_num n)
  | HPair (h1, h2) -> HPair' (hvalue'_of_hvalue h1, hvalue'_of_hvalue h2)

let rec hvalue_of_hvalue' = function
  | HNum' [ n ] -> HNum n
  | HNum' [ 0; 1 ] -> HHole
  | HNum' _ -> failwith "[WIP] Arbitrary HNum' cannot be converted."
  | HPair' (h1, h2) -> HPair (hvalue_of_hvalue' h1, hvalue_of_hvalue' h2)

let rec string_of_hvalue' = function
  | HNum' hole_coeffs -> (
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
  | HPair' (h1, h2) ->
      "(" ^ string_of_hvalue' h1 ^ ", " ^ string_of_hvalue' h2 ^ ")"

type id = string
type env = id -> hvalue'

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

let empty_env (_ : id) : hvalue' = raise (RunError "undefined variable")

(** Environment augmentation. Use @: to bind (x, v) to f *)
let ( @: ) (x, v) e y = if y = x then v else e y

let rec eval env expr =
  let open HoleCoeffs in
  match expr with
  | Hole -> HNum' (add_hole (make_num 0))
  | Num n -> HNum' (make_num n)
  | Pair (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      HPair' (v1, v2)
  | Fst e -> (
      match eval env e with
      | HPair' (fst, _) -> fst
      | HNum' _ -> raiseTypeError `Pair "FIRST")
  | Snd e -> (
      match eval env e with
      | HPair' (_, snd) -> snd
      | HNum' _ -> raiseTypeError `Pair "SECOND")
  | Add (e1, e2) -> (
      match eval env e1 with
      | HNum' lhs_n -> (
          match eval env e2 with
          | HNum' rhs_n -> HNum' (lhs_n +! rhs_n)
          | HPair' _ -> raiseTypeError `Num "ADD")
      | HPair' _ -> raiseTypeError `Num "ADD")
  | Neg e -> (
      match eval env e with
      | HNum' n -> HNum' ~-!n
      | HPair' _ -> raiseTypeError `Num "NEGATE")
  | Case (x, y, z, e1, e2) -> (
      let v = eval env x in
      match v with
      | HPair' (v1, v2) ->
          let env' = (y, v1) @: (z, v2) @: env in
          eval env' e1
      | _ -> eval env e2)
  | If (pred, true_e, false_e) -> (
      let v = eval env pred in
      match v with
      | HNum' n -> eval env (if is_zero n then false_e else true_e)
      | HPair' _ -> raiseTypeError `Num "IF")
  | Let (x, exp, body) ->
      let v = eval env exp in
      eval ((x, v) @: env) body
  | Var x -> env x
