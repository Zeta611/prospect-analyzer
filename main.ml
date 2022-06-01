(* Types and exceptions *)
type value = Hole | Num of number | Pair of value * value

and number = int

and env = id -> value

and id = string

type comp_op =
  | Eq
  (* =0 *)
  | Ne (* ≠0 *)

(* | Lt (* <0 *) *)
(* | Gt (* >0 *) *)
(* | Le (* ≤0 *) *)
(* | Ge (* ≥0 *) *)
and ih_coeffs = int list

and cond_eqn = ih_coeffs * comp_op

exception TypeError of string

exception RunError of string

exception VersionError of string

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

(* Convert input-output value types to the `value` type *)
let rec vvalue_to_value = function
  | L.VNum n -> Num n
  | L.VPair (a, b) -> Pair (vvalue_to_value a, vvalue_to_value b)

(* Environment augmentation *)
(* Use ++ to bind (x, v) to f *)
let ( ++ ) (e : env) ((x, v) : id * value) : env =
 fun y -> if y = x then v else e y

let rec eval env expr =
  match expr with
  | L.Hole -> Hole
  | L.Num n -> Num n
  | L.Pair (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      Pair (v1, v2)
  | L.Fst e -> (
      let pair = eval env e in
      match pair with
      | Pair (fst, _) -> fst
      | _ -> raise (TypeError "FIRST: not a pair") )
  | L.Snd e -> (
      let pair = eval env e in
      match pair with
      | Pair (_, snd) -> snd
      | _ -> raise (TypeError "SECOND: not a pair") )
  | L.Add (e1, e2) -> (
      let lhs = eval env e1 in
      match lhs with
      | Num lhs_n -> (
          let rhs = eval env e2 in
          match rhs with
          | Num rhs_n -> Num (lhs_n + rhs_n)
          | _ -> raise (TypeError "ADD: lhs not a number") )
      | _ -> raise (TypeError "ADD: rhs not a number") )
  | L.Neg e -> (
      let num = eval env e in
      match num with
      | Num n -> Num (-n)
      | _ -> raise (TypeError "NEGATE: not a number") )
  | L.Case (x, y, z, e1, e2) -> (
      let v = eval env x in
      match v with
      | Pair (v1, v2) ->
          let env' = env ++ (y, v1) ++ (z, v2) in
          eval env' e1
      | _ -> eval env e2 )
  | L.If (pred, true_e, false_e) -> (
      let v = eval env pred in
      match v with
      | Num n -> eval env (if n <> 0 then true_e else false_e)
      | _ -> raise (TypeError "IF: pred not a number") )
  | L.Let (x, exp, body) ->
      let v = eval env exp in
      eval (env ++ (x, v)) body
  | L.Var x -> env x

let main () =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Lexer.token lexbuf in
  program

(* Type check *)
type ty =
  | TyInt
  | TyPair of ty * ty
  | TyHole
  (* of hvtype? *)
  | TyVar of tyvar

and tyvar = string

and hvty = HVRoot | HVLeft of hvty | HVRight of hvty

and ty_env = (id * ty) list

exception UnificationError

let rec type_to_string = function
  | TyInt -> "ι"
  | TyPair (e1, e2) -> "(" ^ type_to_string e1 ^ ", " ^ type_to_string e2 ^ ")"
  | TyHole -> "[]"
  | TyVar tv -> tv

let var_count = ref 0

let new_var () =
  let _ = var_count := !var_count + 1 in
  "τ" ^ string_of_int !var_count

(* type env *)
let lookup (x : id) (env : ty_env) : ty =
  try List.assoc x env
  with Not_found -> raise (TypeError "Unbound type variable")

let bind_type (x : id) (t : ty) (env : ty_env) : ty_env = (x, t) :: env

(* substitution *)
type substitution = ty -> ty

let empty_subst : substitution = fun t -> t

let apply_subst (old_type : ty) (new_type : ty) : substitution =
  let _ = match old_type with TyVar _ -> () | _ -> failwith "ty not TyVar" in
  let rec subs t =
    match t with
    | TyPair (l, r) -> TyPair (subs l, subs r)
    | _ -> if t = old_type then new_type else t
  in
  subs

let subst_env (subs : substitution) (env : ty_env) : ty_env =
  List.map (fun (x, t) -> (x, subs t)) env

let rec tyvars_in_type (t : ty) : tyvar list =
  let rec union (tyvars1 : tyvar list) (tyvars2 : tyvar list) : tyvar list =
    let tyvars1_sub_tyvars2 =
      List.filter (fun tv -> not (List.mem tv tyvars2)) tyvars1
    in
    tyvars1_sub_tyvars2 @ tyvars2
  in
  match t with
  | TyInt -> []
  | TyPair (t1, t2) -> union (tyvars_in_type t1) (tyvars_in_type t2)
  | TyVar tyvar -> [tyvar]
  | TyHole -> [] (* TODO *)

let ( @* ) (subs' : substitution) (subs : substitution) : substitution =
 fun t -> subs' (subs t)

let rec unify (t1 : ty) (t2 : ty) : substitution =
  (* let _ = print_endline ("Unify " ^ type_to_string t1 ^ " & " ^
     type_to_string t2) in *)
  if t1 = t2 then empty_subst
  else
    match (t1, t2) with
    | TyPair (t1, t2), TyPair (t1', t2') ->
        let s = unify t1 t1' in
        let s' = unify t2 t2' in
        s' @* s
    | TyVar tv, t | t, TyVar tv ->
        if List.mem tv (tyvars_in_type t) then raise UnificationError
        else apply_subst (TyVar tv) t
    | _ -> raise UnificationError

let map3 (f : 'a -> 'b -> 'c -> 'd) (la : 'a list) (lb : 'b list) (lc : 'c list)
    : 'd list =
  List.map2 (fun f c -> f c) (List.map2 f la lb) lc

let map4 (f : 'a -> 'b -> 'c -> 'd -> 'e) (la : 'a list) (lb : 'b list)
    (lc : 'c list) (ld : 'd list) : 'e list =
  List.map2 (fun f d -> f d) (map3 f la lb lc) ld

(* Modified M algorithm *)
let rec infer (env : ty_env) (e : L.expr) (t : ty) : substitution list =
  (* let _ = print_endline ("M (Gamma, " ^ expr_to_string e ^ ", " ^
     type_to_string t ^ ")") in *)
  (* Generate a list of s''s's from a non-branching expression with two
     subexpressions *)
  let gen_s''s's (t' : ty) (e1 : L.expr) (e2 : L.expr) (t1 : ty) (t2 : ty) :
      substitution list =
    let s = unify t t' in
    let ls' = infer (subst_env s env) e1 (s t1) in
    (* ls' = [s1'; s2'; ...; sn'] *)
    let gen_s'' s' =
      (* use each s' and combine with s to generate a new list of s'' *)
      infer (subst_env (s' @* s) env) e2 ((s' @* s) t2)
    in
    let lls'' = List.map gen_s'' ls' in
    (* lls'' = [[s11''; ...]; ...; [s1n''; ...]] *)
    let lls''s' =
      List.map2 (* possibly better to use tail-recursive rev_map2 *)
        (fun s' ls'' -> List.map (fun s'' -> s'' @* s') ls'')
        ls' lls''
    in
    (* lls''s' = [[s11'' s1'; ...]; ...; [s1n'' sn'; ...]] *)
    let ls''s' = List.flatten lls''s' in
    let ls''s's = List.map (fun s''s' -> s''s' @* s) ls''s' in
    ls''s's
  in
  match e with
  | L.Hole ->
      let h_t = TyVar "τ" in
      [unify t h_t]
  | L.Num n -> [unify t TyInt]
  | L.Var x ->
      let x_t = lookup x env in
      [unify t x_t]
  | L.Pair (e1, e2) ->
      let t1 = TyVar (new_var ()) in
      let t2 = TyVar (new_var ()) in
      gen_s''s's (TyPair (t1, t2)) e1 e2 t1 t2
  | L.Fst e -> infer env e (TyPair (t, TyVar (new_var ())))
  | L.Snd e -> infer env e (TyPair (TyVar (new_var ()), t))
  | L.Add (e1, e2) -> gen_s''s's TyInt e1 e2 TyInt TyInt
  | L.Neg e ->
      let s = unify t TyInt in
      let ls' = infer (subst_env s env) e (s TyInt) in
      let ls's = List.map (fun s' -> s' @* s) ls' in
      ls's
  | L.Case (x, y, z, e1, e2) ->
      (* x binds to (y, z) *)
      let ls's_bind =
        try
          let y_t = TyVar (new_var ()) in
          let z_t = TyVar (new_var ()) in
          let ls = infer env x (TyPair (y_t, z_t)) in
          let lenv' = List.map (fun s -> subst_env s env) ls in
          let ly_t' = List.map (fun s -> s y_t) ls in
          let lz_t' = List.map (fun s -> s z_t) ls in
          let gen_s' env' y_t' z_t' s =
            infer (bind_type y y_t' (bind_type z z_t' env')) e1 (s t)
          in
          let lls' = map4 gen_s' lenv' ly_t' lz_t' ls in
          let lls's =
            List.map2 (fun s ls' -> List.map (fun s' -> s' @* s) ls') ls lls'
          in
          let ls's = List.flatten lls's in
          ls's
        with UnificationError -> []
      in
      (* x is TyInt *)
      let ls's_nbind =
        try
          let ls = infer env x TyInt in
          let lenv' = List.map (fun s -> subst_env s env) ls in
          let gen_s' env' s = infer env' e2 (s t) in
          let lls' = List.map2 gen_s' lenv' ls in
          let lls's =
            List.map2 (fun s ls' -> List.map (fun s' -> s' @* s) ls') ls lls'
          in
          let ls's = List.flatten lls's in
          ls's
        with UnificationError -> []
      in
      ls's_bind @ ls's_nbind
  | L.If (e_p, e_t, e_f) ->
      let ls = infer env e_p TyInt in
      let gen_ls's e =
        let lls' = List.map (fun s -> infer (subst_env s env) e (s t)) ls in
        let lls's =
          List.map2 (fun s ls' -> List.map (fun s' -> s' @* s) ls') ls lls'
        in
        let ls's = List.flatten lls's in
        ls's
      in
      let ls's_t = gen_ls's e_t in
      let ls's_f = gen_ls's e_f in
      ls's_t @ ls's_f
  | L.Let (x, v, e) ->
      let x_t = TyVar (new_var ()) in
      let ls = infer env v x_t in
      let lenv' = List.map (fun s -> subst_env s env) ls in
      let lx_t' = List.map (fun s -> s x_t) ls in
      let gen_s' env' x_t' s = infer (bind_type x x_t' env') e (s t) in
      let lls' = map3 gen_s' lenv' lx_t' ls in
      let lls's =
        List.map2 (fun s ls' -> List.map (fun s' -> s' @* s) ls') ls lls'
      in
      let ls's = List.flatten lls's in
      ls's

(* Returns the possible combinations of the [] and the output *)
let type_check (e : L.expr) (t : ty) : (ty * ty) list =
  let hole_type = TyVar "τ" in
  let ls = infer [] e t in
  List.map (fun subst -> (subst hole_type, subst t)) ls

(* samples is a list of input-output pairs *)
let version, samples, root_expr = main ()

let converted_samples =
  List.map (fun (i, o) -> (vvalue_to_value i, vvalue_to_value o)) samples

(* Process version *)
(* let _ = print_string "Interpreter version: L" *)
let _ = print_string "Type checker version: L"

let _ = print_int version

let _ = print_newline ()

let rec check_version version expr =
  match version with
  | 0 -> (
    match expr with
    | L.Pair _ -> raise (VersionError "PAIR: not supported")
    | L.Fst _ -> raise (VersionError "FIRST: not supported")
    | L.Snd _ -> raise (VersionError "SECOND: not supported")
    | L.Case _ -> raise (VersionError "CASE: not supported")
    | L.Add (e1, e2) -> check_version version e1 ; check_version version e2
    | L.Neg e -> check_version version e
    | L.If (e_p, e_t, e_f) ->
        check_version version e_p ;
        check_version version e_t ;
        check_version version e_f
    | L.Let (x, v, e) -> check_version version v ; check_version version e
    | L.Hole -> ()
    | L.Num _ -> ()
    | L.Var _ -> () )
  | 1 -> ()
  | _ -> raise (VersionError "Version not supported")

let _ = check_version version root_expr

let out_types =
  let rec val_to_expr = function
    | Hole -> L.Hole
    | Num n -> L.Num n
    | Pair (v1, v2) -> L.Pair (val_to_expr v1, val_to_expr v2)
  in
  let rec val_to_type = function
    | Hole -> failwith "Hole should not exist in output"
    | Num _ -> TyInt
    | Pair (v1, v2) -> TyPair (val_to_type v1, val_to_type v2)
  in
  List.map
    (fun (i, t) -> type_check (L.Let ("x", i, root_expr)) t)
    (List.map
       (fun p -> (val_to_expr @@ fst @@ p, val_to_type @@ snd @@ p))
       converted_samples )

let rec print_type_list (types : (ty * ty) list) : unit =
  match types with
  | (ht, ot) :: ps ->
      print_endline ("| []: " ^ type_to_string ht ^ ", O: " ^ type_to_string ot) ;
      print_type_list ps
  | [] -> print_newline ()

let _ =
  if List.flatten out_types = [] then print_endline "Unsatisfiable!"
  else List.iter print_type_list out_types

(* TODO: Update evaluation with holes *)
(* (* Evaluate expression for each input *) *)
(* let empty_env = fun x -> raise (RunError "undefined variable") *)
(* let outputs = List.map *)
(*   (fun i -> eval (empty_env ++ ("x", i)) root_expr) *)
(*   (List.map fst converted_samples) *)
(*  *)
(* (* Filter the outputs that do not match the user-provided outputs *) *)
(* let diffs = List.filter *)
(*   (fun p -> fst p <> snd (snd p)) *)
(*   (List.combine outputs converted_samples) *)
(*  *)
(* (* Print results *) *)
(* let _ = match diffs with *)
(*   | [] -> print_string "All samples passed!\n" *)
(*   | _ -> *)
(*     let rec print value = *)
(*       match value with *)
(*       | Hole -> print_string "[]" *)
(*       | Num n -> print_int n *)
(*       | Pair (fst, snd) -> *)
(* print_char '('; print fst; print_char ','; print snd; print_char ')' *)
(*     in *)
(*     List.iter *)
(*       (fun (o', (i, o)) -> *)
(*         print_string "Input "; print i; *)
(*         print_string " should output "; print o; *)
(*         print_string ", but got "; print o'; *)
(*         print_newline ()) *)
(*       diffs *)
