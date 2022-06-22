(* Types and exceptions *)
type value = L.hvalue
and number = int
and env = id -> value
and id = string

type comp_op =
  | Eq (* =0 *)
  | Ne (* ≠0 *)

(* | Lt (* <0 *) *)
(* | Gt (* >0 *) *)
(* | Le (* ≤0 *) *)
(* | Ge (* ≥0 *) *)
and ih_coeffs = int list
and cond_eqn = ih_coeffs * comp_op

exception TypeError of string
exception RunError of string

let rec expr_to_string = function
  | L.Hole -> "[]"
  | L.Num n -> string_of_int n
  | L.Pair (e1, e2) -> "(" ^ expr_to_string e1 ^ "," ^ expr_to_string e2 ^ ")"
  | L.Fst e -> expr_to_string e ^ ".1"
  | L.Snd e -> expr_to_string e ^ ".2"
  | L.Add (e1, e2) -> expr_to_string e1 ^ "+" ^ expr_to_string e2
  | L.Neg e -> "-" ^ expr_to_string e
  | L.Case (x, y, z, e1, e2) ->
      "case " ^ expr_to_string x ^ " (" ^ y ^ "," ^ z ^ ") " ^ expr_to_string e1
      ^ " " ^ expr_to_string e2
  | L.If (e_p, e_t, e_f) ->
      "if " ^ expr_to_string e_p ^ " " ^ expr_to_string e_t ^ " "
      ^ expr_to_string e_f
  | L.Let (x, exp, body) ->
      "let " ^ x ^ " " ^ expr_to_string exp ^ " " ^ expr_to_string body
  | L.Var x -> x

(* Environment augmentation *)
(* Use ++ to bind (x, v) to f *)
let ( ++ ) (e : env) ((x, v) : id * value) : env =
 fun y -> if y = x then v else e y

let rec eval env expr =
  match expr with
  | L.Hole -> L.HHole
  | L.Num n -> L.HNum n
  | L.Pair (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      L.HPair (v1, v2)
  | L.Fst e -> (
      let pair = eval env e in
      match pair with
      | L.HPair (fst, _) -> fst
      | _ -> raise (TypeError "FIRST: not a pair"))
  | L.Snd e -> (
      let pair = eval env e in
      match pair with
      | L.HPair (_, snd) -> snd
      | _ -> raise (TypeError "SECOND: not a pair"))
  | L.Add (e1, e2) -> (
      let lhs = eval env e1 in
      match lhs with
      | L.HNum lhs_n -> (
          let rhs = eval env e2 in
          match rhs with
          | L.HNum rhs_n -> L.HNum (lhs_n + rhs_n)
          | _ -> raise (TypeError "ADD: lhs not a number"))
      | _ -> raise (TypeError "ADD: rhs not a number"))
  | L.Neg e -> (
      let num = eval env e in
      match num with
      | L.HNum n -> L.HNum (-n)
      | _ -> raise (TypeError "NEGATE: not a number"))
  | L.Case (x, y, z, e1, e2) -> (
      let v = eval env x in
      match v with
      | L.HPair (v1, v2) ->
          let env' = env ++ (y, v1) ++ (z, v2) in
          eval env' e1
      | _ -> eval env e2)
  | L.If (pred, true_e, false_e) -> (
      let v = eval env pred in
      match v with
      | L.HNum n -> eval env (if n <> 0 then true_e else false_e)
      | _ -> raise (TypeError "IF: pred not a number"))
  | L.Let (x, exp, body) ->
      let v = eval env exp in
      eval (env ++ (x, v)) body
  | L.Var x -> env x
