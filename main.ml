(* Types and exceptions *)
type value =
  | Hole
  | Num of number
  | Pair of value * value

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
      | _ -> raise (TypeError "FIRST: not a pair"))
  | L.Snd e -> (
      let pair = eval env e in
      match pair with
      | Pair (_, snd) -> snd
      | _ -> raise (TypeError "SECOND: not a pair"))
  | L.Add (e1, e2) -> (
      let lhs = eval env e1 in
      match lhs with
      | Num lhs_n -> (
          let rhs = eval env e2 in
          match rhs with
          | Num rhs_n -> Num (lhs_n + rhs_n)
          | _ -> raise (TypeError "ADD: lhs not a number"))
      | _ -> raise (TypeError "ADD: rhs not a number"))
  | L.Neg e -> (
      let num = eval env e in
      match num with
      | Num n -> Num (-n)
      | _ -> raise (TypeError "NEGATE: not a number"))
  | L.Case (x, y, z, e1, e2) -> (
      let v = eval env x in
      match v with
      | Pair (v1, v2) ->
          let env' = env ++ (y, v1) ++ (z, v2) in
          eval env' e1
      | _ -> eval env e2)
  | L.If (pred, true_e, false_e) -> (
      let v = eval env pred in
      match v with
      | Num n -> eval env (if n <> 0 then true_e else false_e)
      | _ -> raise (TypeError "IF: pred not a number"))
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
  | TyHole (* of hvtype? *)
  | TyVar of tyvar

and tyvar = string

and hvty =
  | HVRoot
  | HVLeft of hvty
  | HVRight of hvty

(* Taken path that should accompany a ty *)
and path =
  | PtNil
  | PtPair of path * path
  | PtAdd of path * path
  | PtCaseP of path * path
  | PtCaseN of path * path
  | PtIfTru of path * path
  | PtIfFls of path * path
  | PtLet of path * path

and tp_env = (id * (ty * path)) list
and tag = int

and tagged_exp =
  | TgHole of tag
  | TgNum of tag * number
  | TgVar of tag * id
  | TgPair of tag * tagged_exp * tagged_exp
  | TgFst of tag * tagged_exp
  | TgSnd of tag * tagged_exp
  | TgAdd of tag * tagged_exp * tagged_exp
  | TgNeg of tag * tagged_exp
  | TgCase of tag * tagged_exp * id * id * tagged_exp * tagged_exp
  | TgIf of tag * tagged_exp * tagged_exp * tagged_exp
  | TgLet of tag * id * tagged_exp * tagged_exp

exception UnificationError

let rec type_to_string = function
  | TyInt -> "ι"
  | TyPair (e1, e2) -> "(" ^ type_to_string e1 ^ ", " ^ type_to_string e2 ^ ")"
  | TyHole -> "[]"
  | TyVar tv -> tv

let rec path_to_string = function
  | PtNil -> "."
  | PtPair (p1, p2) -> "(" ^ path_to_string p1 ^ ", " ^ path_to_string p2 ^ ")"
  | PtAdd (p1, p2) -> path_to_string p1 ^ " + " ^ path_to_string p2
  | PtCaseP (p1, p2) ->
      "case (" ^ path_to_string p1 ^ ") : * " ^ path_to_string p2
  | PtCaseN (p1, p2) ->
      "case (" ^ path_to_string p1 ^ ") : ι " ^ path_to_string p2
  | PtIfTru (p1, p2) ->
      "if (" ^ path_to_string p1 ^ ") ≠ 0 " ^ path_to_string p2
  | PtIfFls (p1, p2) ->
      "if (" ^ path_to_string p1 ^ ") = 0 " ^ path_to_string p2
  | PtLet (p1, p2) ->
      "let . = (" ^ path_to_string p1 ^ ") in (" ^ path_to_string p2 ^ ")"

let colorize (col : int) (text : string) : string =
  let xterm_256_color (color : int) : string =
    let escape_prefix = "\027" (* = \e = \0x1B = (\033 in oct) *) in
    let finish = "m" in
    escape_prefix ^ "[38;5;" ^ string_of_int color ^ finish
  in
  let reset = "\027[0m" in
  xterm_256_color col ^ text ^ reset

let palette =
  [ 046; 045; 166; 105; 226; 207; 063; 027; 165; 166; 219; 069; 202 ]

let colorize_palatte t =
  colorize (List.nth palette ((t - 1) mod List.length palette))

let rec tags_to_string = function
  | [] -> ""
  | [ hd ] -> colorize_palatte hd ("ℓ" ^ string_of_int hd)
  | hd :: tl ->
      colorize_palatte hd ("ℓ" ^ string_of_int hd) ^ "-" ^ tags_to_string tl

let var_count = ref 0

let new_var () =
  let _ = var_count := !var_count + 1 in
  "τ" ^ string_of_int !var_count

let tag_count = ref 0

let new_tag () =
  incr tag_count;
  !tag_count
(* let _ = tag_count := !tag_count + 1 in *)
(* "ℓ" ^ string_of_int !tag_count *)

let rec tag_exp (e : L.expr) : tagged_exp =
  let tg = new_tag () in
  match e with
  | Hole -> TgHole tg
  | Num n -> TgNum (tg, n)
  | Var x -> TgVar (tg, x)
  | Pair (e1, e2) ->
      let te1 = tag_exp e1 in
      let te2 = tag_exp e2 in
      TgPair (tg, te1, te2)
  | Fst e -> TgFst (tg, tag_exp e)
  | Snd e -> TgSnd (tg, tag_exp e)
  | Add (e1, e2) ->
      let te1 = tag_exp e1 in
      let te2 = tag_exp e2 in
      TgAdd (tg, te1, te2)
  | Neg e -> TgNeg (tg, tag_exp e)
  | Case (x, y, z, e1, e2) ->
      let tx = tag_exp x in
      let te1 = tag_exp e1 in
      let te2 = tag_exp e2 in
      TgCase (tg, tx, y, z, te1, te2)
  | If (e_p, e_t, e_f) ->
      let te_p = tag_exp e_p in
      let te_t = tag_exp e_t in
      let te_f = tag_exp e_f in
      TgIf (tg, te_p, te_t, te_f)
  | Let (x, exp, body) ->
      let te = tag_exp exp in
      let tb = tag_exp body in
      TgLet (tg, x, te, tb)

(* type env *)
let lookup (x : id) (env : tp_env) : ty * path =
  try List.assoc x env
  with Not_found -> raise (TypeError "Unbound type variable")

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

let subst_env (subs : substitution) (env : tp_env) : tp_env =
  List.map (fun (x, (t, p)) -> (x, (subs t, p))) env

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
  | TyVar tyvar -> [ tyvar ]
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

(** Modified M algorithm *)
let rec infer (env : tp_env) (e : tagged_exp) (t : ty) :
    (substitution * path * tag list) list =
  (* let _ = print_endline ("M (Gamma, " ^ expr_to_string e ^ ", " ^
     type_to_string t ^ ")") in *)

  (* Generate a list of s''s's from a non-branching expression with two
     subexpressions *)
  let gen_s''s's (t' : ty) (e1 : tagged_exp) (e2 : tagged_exp) (t1 : ty)
      (t2 : ty) : (substitution * (path * path) * tag list) list =
    let s = unify t t' in

    (* ls' = [s1', p1, tg1; s2', p1, tg1; ...; sn', p1, tg1] *)
    let ls' = infer (subst_env s env) e1 (s t1) in

    let gen_s'' (s', p1, tg1) =
      (* use each s' and combine with s to generate a new list of s'' *)
      infer (subst_env (s' @* s) env) e2 ((s' @* s) t2)
      |> List.map (fun (s, p2, tg2) -> (s, (p1, p2), tg1 @ tg2))
    in

    (* lls'' = [[s11'', (p1, p2), tg1 @ tg2; ...]; ...; [s1n'', (p1, p2), tg1 @
       tg2; ...]] *)
    let lls'' = List.map gen_s'' ls' in

    let lls''s' =
      List.map2 (* possibly better to use tail-recursive rev_map2 *)
        (fun (s', _, _) ls'' ->
          List.map (fun (s'', pp, tgl) -> (s'' @* s', pp, tgl)) ls'')
        ls' lls''
    in
    (* lls''s' = [[s11'' s1', (p1, p2), tg1 @ tg2; ...]; ...; [s1n'' sn', (p1,
       p2), tg1 @ tg2; ...]] *)
    let ls''s' = List.flatten lls''s' in
    let ls''s's =
      List.map (fun (s''s', pp, tgl) -> (s''s' @* s, pp, tgl)) ls''s'
    in
    ls''s's
  in
  try
    match e with
    | TgHole tg ->
        let h_t = TyVar "τ" in
        [ (unify t h_t, PtNil, [ tg ]) ]
    | TgNum (tg, n) -> [ (unify t TyInt, PtNil, [ tg ]) ]
    | TgVar (tg, x) ->
        let x_t, x_p = lookup x env in
        (* [ (unify t x_t, x_p) ] *)
        [ (unify t x_t, PtNil, [ tg ]) ]
    | TgPair (tg, e1, e2) ->
        let t1 = TyVar (new_var ()) in
        let t2 = TyVar (new_var ()) in
        List.map
          (fun (s, (p1, p2), tgl) -> (s, PtPair (p1, p2), tg :: tgl))
          (gen_s''s's (TyPair (t1, t2)) e1 e2 t1 t2)
    | TgFst (tg, e) ->
        List.map
          (fun (s, p, tgl) -> (s, p, tg :: tgl))
          (infer env e (TyPair (t, TyVar (new_var ()))))
    | TgSnd (tg, e) ->
        List.map
          (fun (s, p, tgl) -> (s, p, tg :: tgl))
          (infer env e (TyPair (TyVar (new_var ()), t)))
    | TgAdd (tg, e1, e2) ->
        List.map
          (fun (s, (p1, p2), tgl) -> (s, PtAdd (p1, p2), tg :: tgl))
          (gen_s''s's TyInt e1 e2 TyInt TyInt)
    | TgNeg (tg, e) ->
        let s = unify t TyInt in
        let ls' = infer (subst_env s env) e (s TyInt) in
        (* TODO: (s TyInt) -> TyInt *)
        let ls's = List.map (fun (s', p, tgl) -> (s' @* s, p, tg :: tgl)) ls' in
        ls's
    | TgCase (tg, x, y, z, e1, e2) ->
        (* x binds to (y, z) *)
        let ls's_bind =
          let y_t = TyVar (new_var ()) in
          let z_t = TyVar (new_var ()) in

          let ls = infer env x (TyPair (y_t, z_t)) in
          let lenv' = List.map (fun (s, _, _) -> subst_env s env) ls in
          let ly_t' = List.map (fun (s, _, _) -> (s y_t, PtNil)) ls in
          let lz_t' = List.map (fun (s, _, _) -> (s z_t, PtNil)) ls in

          let gen_s' env' y_t' z_t' (s, _, _) =
            infer ((y, y_t') :: (z, z_t') :: env') e1 (s t)
          in
          let lls' = map4 gen_s' lenv' ly_t' lz_t' ls in
          let lls's =
            List.map2
              (fun (s, x_p, x_tgl) ls' ->
                List.map
                  (fun (s', e1_p, e1_tgl) ->
                    (s' @* s, PtCaseP (x_p, e1_p), x_tgl @ e1_tgl))
                  ls')
              ls lls'
          in
          let ls's = List.flatten lls's in
          ls's
        in

        (* x is TyInt *)
        let ls's_nbind =
          let ls = infer env x TyInt in
          let lenv' = List.map (fun (s, _, _) -> subst_env s env) ls in

          let gen_s' env' (s, _, _) = infer env' e2 (s t) in
          let lls' = List.map2 gen_s' lenv' ls in
          let lls's =
            List.map2
              (fun (s, x_p, x_tgl) ls' ->
                List.map
                  (fun (s', e2_p, e2_tgl) ->
                    (s' @* s, PtCaseN (x_p, e2_p), x_tgl @ e2_tgl))
                  ls')
              ls lls'
          in
          let ls's = List.flatten lls's in
          ls's
        in

        ls's_bind @ ls's_nbind
    | TgIf (tg, e_p, e_t, e_f) ->
        let ls = infer env e_p TyInt in

        let gen_ls's e_tf choice =
          let lls' =
            List.map (fun (s, _, _) -> infer (subst_env s env) e_tf (s t)) ls
          in
          let lls's =
            List.map2
              (fun (s, e_p_p, e_p_tgl) ls' ->
                List.map
                  (fun (s', e_tf_p, e_tf_tgl) ->
                    ( s' @* s,
                      (if choice then PtIfTru (e_p_p, e_tf_p)
                      else PtIfFls (e_p_p, e_tf_p)),
                      e_p_tgl @ e_tf_tgl ))
                  ls')
              ls lls'
          in
          let ls's = List.flatten lls's in
          ls's
        in
        let ls's_t = gen_ls's e_t true in
        let ls's_f = gen_ls's e_f false in
        ls's_t @ ls's_f
    | TgLet (tg, x, v, e) ->
        let x_t = TyVar (new_var ()) in

        let ls = infer env v x_t in
        let lenv' = List.map (fun (s, _, _) -> subst_env s env) ls in
        let lx_tp' = List.map (fun (s, x_p, _) -> (s x_t, x_p)) ls in

        let gen_s' env' x_tp' (s, _, _) = infer ((x, x_tp') :: env') e (s t) in
        let lls' = map3 gen_s' lenv' lx_tp' ls in
        let lls's =
          List.map2
            (fun (s, v_p, v_tgl) ls' ->
              List.map
                (fun (s', e_p, e_tgl) ->
                  (s' @* s, PtLet (v_p, e_p), tg :: (v_tgl @ e_tgl)))
                ls')
            ls lls'
        in
        let ls's = List.flatten lls's in
        ls's
  with UnificationError -> []

let rec tagged_exp_to_string e =
  let parwrap t s = colorize_palatte t "[" ^ s ^ colorize_palatte t "]" in
  let annot t s = s ^ colorize_palatte t (" : " ^ "ℓ" ^ string_of_int t) in
  match e with
  | TgHole t -> annot t "[]" |> parwrap t
  | TgNum (t, n) -> string_of_int n |> annot t |> parwrap t
  | TgVar (t, x) -> annot t x |> parwrap t
  | TgPair (t, e1, e2) ->
      "(" ^ tagged_exp_to_string e1 ^ ", " ^ tagged_exp_to_string e2 ^ ")"
      |> annot t |> parwrap t
  | TgFst (t, e) -> tagged_exp_to_string e ^ ".1" |> annot t |> parwrap t
  | TgSnd (t, e) -> tagged_exp_to_string e ^ ".2" |> annot t |> parwrap t
  | TgAdd (t, e1, e2) ->
      tagged_exp_to_string e1 ^ " + " ^ tagged_exp_to_string e2
      |> annot t |> parwrap t
  | TgNeg (t, e) -> "-" ^ tagged_exp_to_string e |> annot t |> parwrap t
  | TgCase (t, x, y, z, e1, e2) ->
      "case " ^ tagged_exp_to_string x ^ " (" ^ y ^ "," ^ z ^ ") "
      ^ tagged_exp_to_string e1 ^ " " ^ tagged_exp_to_string e2
      |> annot t |> parwrap t
  | TgIf (t, e_p, e_t, e_f) ->
      "if " ^ tagged_exp_to_string e_p ^ " " ^ tagged_exp_to_string e_t ^ " "
      ^ tagged_exp_to_string e_f
      |> annot t |> parwrap t
  | TgLet (t, x, exp, body) ->
      "let " ^ x ^ " = " ^ tagged_exp_to_string exp ^ " in "
      ^ tagged_exp_to_string body
      |> annot t |> parwrap t

(* Returns the possible combinations of the [] and the output *)
let type_check (e : L.expr) (t : ty) : (ty * ty * path * tag list) list =
  let hole_type = TyVar "τ" in
  let tagged_e = tag_exp e in
  (* let tagged_e' = *)
  (*   match tagged_e with *)
  (*   | TgLet (_, _, _, e) -> e *)
  (*   | _ -> failwith "No top-level binding for x; programming error" *)
  (* in *)
  let _ = print_endline (tagged_exp_to_string tagged_e) in
  let ls = infer [] tagged_e t in
  List.map (fun (subst, pt, tgl) -> (subst hole_type, subst t, pt, tgl)) ls

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
      | L.Add (e1, e2) ->
          check_version version e1;
          check_version version e2
      | L.Neg e -> check_version version e
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
       converted_samples)

let rec print_type_list (typts : (ty * ty * path * tag list) list) : unit =
  match typts with
  | (ht, ot, pt, tgl) :: ps ->
      let pt' =
        match pt with
        | PtLet (_, pt') -> pt'
        | _ -> failwith "No top-level binding for x; programming error"
      in
      print_endline
        ("| []: "
        ^ colorize 009 (type_to_string ht)
        ^ ", O: "
        ^ colorize 009 (type_to_string ot)
        ^ ", Trace: "
        ^ colorize 011 (path_to_string pt')
        ^ "; " ^ tags_to_string tgl);
      print_type_list ps
  | [] -> ()

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
