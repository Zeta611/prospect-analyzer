open L

(* Types and exceptions *)
module HoleCoeffs = struct
  open Monads.List

  type t = int list

  let is_zero hole_coeffs =
    List.find_opt (( <> ) 0) hole_coeffs |> Option.is_none

  let length = List.length
  let make = return

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
  | HHole' of int
  | HNum' of HoleCoeffs.t
  | HPair' of hvalue' * hvalue'

let rec hvalue'_of_hvalue = function
  | HHole -> HHole' 1
  | HNum n -> HNum' (HoleCoeffs.make n)
  | HPair (h1, h2) -> HPair' (hvalue'_of_hvalue h1, hvalue'_of_hvalue h2)

let rec hvalue_of_hvalue' = function
  | HHole' 1 -> HHole
  | HNum' [ n ] -> HNum n
  | HPair' (h1, h2) -> HPair (hvalue_of_hvalue' h1, hvalue_of_hvalue' h2)
  | _ -> failwith "[WIP] Conversion failure!"

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

let empty_env (_ : id) : hvalue' = raise (RunError "undefined variable")

(** Environment augmentation. Use @: to bind (x, v) to f *)
let ( @: ) (x, v) e y = if y = x then v else e y

let rec eval env expr =
  let open HoleCoeffs in
  match expr with
  | Hole -> HHole' 1
  | Num n -> HNum' (make n)
  | Pair (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      HPair' (v1, v2)
  | Fst e -> (
      let pair = eval env e in
      match pair with
      | HPair' (fst, _) -> fst
      | _ -> raise (TypeError "FIRST: not a pair"))
  | Snd e -> (
      let pair = eval env e in
      match pair with
      | HPair' (_, snd) -> snd
      | _ -> raise (TypeError "SECOND: not a pair"))
  | Add (e1, e2) -> (
      let lhs = eval env e1 in
      match lhs with
      | HNum' lhs_n -> (
          let rhs = eval env e2 in
          match rhs with
          | HNum' rhs_n -> HNum' (lhs_n +! rhs_n)
          | _ -> raise (TypeError "ADD: lhs not a number"))
      | _ -> raise (TypeError "ADD: rhs not a number"))
  | Neg e -> (
      let num = eval env e in
      match num with
      | HNum' n -> HNum' ~-!n
      | _ -> raise (TypeError "NEGATE: not a number"))
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
      | _ -> raise (TypeError "IF: pred not a number"))
  | Let (x, exp, body) ->
      let v = eval env exp in
      eval ((x, v) @: env) body
  | Var x -> env x
