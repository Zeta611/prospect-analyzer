let main () =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Lexer.token lexbuf in
  program

type value = Num of number
           | Pair of value * value
and number = int
and env = id -> value
and id = string

exception TypeError of string
exception RunError of string

let (@+) f (x, v) = (fun y -> if y = x then v else f y)
let bind env (x, v) = env @+ (x, v)

let rec print value =
  match value with
  | Num n -> print_int n
  | Pair (fst, snd) ->
    print_char '('; print fst; print_char ','; print snd; print_char ')'

let rec eval env expr =
  match expr with
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

let (version, root_expr) = main ()
let _ = print_string "Interpreter version: L"
let _ = print_int version
let _ = print_newline ()

let empty_env = fun x -> raise (RunError "undefined variable")
let result = eval empty_env root_expr
let _ = print result
let _ = print_newline ()
