(* Types and exceptions *)
type value = Hole
           | Num of number
           | Pair of value * value
and number = int
and env = id -> value
and id = string

type comp_op = Eq (* =0 *)
             | Ne (* ≠0 *)
             | Lt (* <0 *)
             (* | Gt (* >0 *) *)
             (* | Le (* ≤0 *) *)
             (* | Ge (* ≥0 *) *)
and ih_coeffs = int list
and cond_eqn = ih_coeffs * comp_op

exception TypeError of string
exception RunError of string
exception VersionError of string

(* Convert input-output value types to the `value` type *)
let rec vvalue_to_value = function
  | L.VNum n -> Num n
  | L.VPair (a, b) -> Pair (vvalue_to_value a, vvalue_to_value b)

(* Environment augmentation *)
let (@+) f (x, v) = (fun y -> if y = x then v else f y)
let bind env (x, v) = env @+ (x, v)

let rec eval env expr =
  match expr with
  | L.Hole -> Hole
  | L.Num n -> Num n
  | L.Pair (e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    Pair (v1, v2)
  | L.Fst e ->
    let pair = eval env e in
    (match pair with
    | Pair (fst, _) -> fst
    | _ -> raise (TypeError "FIRST: not a pair"))
  | L.Snd e ->
    let pair = eval env e in
    (match pair with
    | Pair (_, snd) -> snd
    | _ -> raise (TypeError "SECOND: not a pair"))
  | L.Add (e1, e2) ->
      let lhs = eval env e1 in
      (match lhs with
      | Num lhs_n ->
        let rhs = eval env e2 in
        (match rhs with
        | Num rhs_n -> Num (lhs_n + rhs_n)
        | _ -> raise (TypeError "ADD: lhs not a number"))
      | _ -> raise (TypeError "ADD: rhs not a number"))
  | L.Neg e ->
    let num = eval env e in
    (match num with
    | Num n -> Num (-n)
    | _ -> raise (TypeError "NEGATE: not a number"))
  | L.Case (x, y, z, e1, e2) ->
    let v = eval env x in
    (match v with
    | Pair (v1, v2) ->
      let env' = bind env (y, v1) in
      let env'' = bind env' (z, v2) in
      eval env'' e1
    | _ -> eval env e2)
  | L.If (pred, true_e, false_e) ->
    let v = eval env pred in
    (match v with
    | Num n -> if n = 0 then eval env false_e else eval env true_e
    | _ -> raise (TypeError "IF: pred not a number"))
  | L.Let (x, exp, body) ->
    let v = eval env exp in
    eval (bind env (x, v)) body
  | L.Var x -> env x

let main () =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Lexer.token lexbuf in
  program

(* samples is a list of input-output pairs *)
let (version, samples, root_expr) = main ()
let converted_samples = List.map
  (fun (i, o) -> (vvalue_to_value i, vvalue_to_value o))
  samples

(* Process version *)
let _ = print_string "Interpreter version: L"
let _ = print_int version
let _ = print_newline ()

let rec check_version version expr =
  match version with
  | 0 ->
    (match expr with
    | L.Pair _ ->
      raise (VersionError "PAIR: not supported")
    | L.Fst _ ->
      raise (VersionError "FIRST: not supported")
    | L.Snd _ ->
      raise (VersionError "SECOND: not supported")
    | L.Case _ ->
      raise (VersionError "CASE: not supported")
    | L.Add (e1, e2) ->
      check_version version e1;
      check_version version e2
    | L.Neg e ->
      check_version version e
    | L.If (e_p, e_t, e_f) ->
      check_version version e_p;
      check_version version e_t;
      check_version version e_f
    | L.Let (x, v, e) ->
      check_version version v;
      check_version version e
    | L.Hole -> ()
    | L.Num _ -> ()
    | L.Var _ -> ())
  | 1 -> ()
  | _ ->
    raise (VersionError "Version not supported")

let _ = check_version version root_expr

(* Evaluate expression for each input *)
let empty_env = fun x -> raise (RunError "undefined variable")
let outputs = List.map
  (fun i -> eval (bind empty_env ("x", i)) root_expr)
  (List.map fst converted_samples)

(* Filter the outputs that do not match the user-provided outputs *)
let diffs = List.filter
  (fun p -> fst p <> snd (snd p))
  (List.combine outputs converted_samples)

(* Print results *)
let _ = match diffs with
  | [] -> print_string "All samples passed!\n"
  | _ ->
    let rec print value =
      match value with
      | Hole -> print_string "[]"
      | Num n -> print_int n
      | Pair (fst, snd) ->
        print_char '('; print fst; print_char ','; print snd; print_char ')'
    in
    List.iter
      (fun (o', (i, o)) ->
        print_string "Input "; print i;
        print_string " should output "; print o;
        print_string ", but got "; print o';
        print_newline ())
      diffs
