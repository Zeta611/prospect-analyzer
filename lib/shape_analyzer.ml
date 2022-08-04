open Path

exception TypeError of string

type id = string

(* Type check *)
type ty =
  | TyInt
  | TyPair of ty * ty
  | TyHole
  | TyVar of tyvar

and tyvar = string

type tp_env = (id * (ty * path)) list

type type_check_info = {
  hole_type : ty;
  exp_type : ty;
  taken_path : path;
  tagged_exp : tagged_exp;
  tag_list : tag list;
}

(** Type of an hole_value without a hole *)
let rec type_of_plain_value = function
  | `Num _ -> TyInt
  | `Pair (a, b) -> TyPair (type_of_plain_value a, type_of_plain_value b)

exception UnificationError

let rec string_of_type = function
  | TyInt -> "ι"
  | TyPair (e1, e2) -> "(" ^ string_of_type e1 ^ ", " ^ string_of_type e2 ^ ")"
  | TyHole -> "[]"
  | TyVar tv -> tv

let var_count = ref 0

let new_var () =
  incr var_count;
  "τ" ^ string_of_int !var_count

(* type env *)
let lookup (x : id) (env : tp_env) : ty * path =
  try List.assoc x env
  with Not_found -> raise (TypeError "Unbound type variable")

(* substitution *)
type substitution = ty -> ty

let empty_subst : substitution = fun t -> t

let apply_subst (old_type : ty) (new_type : ty) : substitution =
  (match old_type with TyVar _ -> () | _ -> failwith "ty not TyVar");
  let rec subs = function
    | TyPair (l, r) -> TyPair (subs l, subs r)
    | t when t = old_type -> new_type
    | t -> t
  in
  subs

let subst_env (subs : substitution) (env : tp_env) : tp_env =
  List.map (fun (x, (t, p)) -> (x, (subs t, p))) env

let rec tyvars_in_type (t : ty) : tyvar list =
  let union (tyvars1 : tyvar list) (tyvars2 : tyvar list) : tyvar list =
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

let ( << ) (subs' : substitution) (subs : substitution) : substitution =
 fun t -> subs' (subs t)

let rec unify (t1 : ty) (t2 : ty) : substitution =
  if t1 = t2 then empty_subst
  else
    match (t1, t2) with
    | TyPair (t1, t2), TyPair (t1', t2') -> unify t2 t2' << unify t1 t1'
    | TyVar tv, t | t, TyVar tv ->
        if List.mem tv (tyvars_in_type t) then raise UnificationError
        else apply_subst (TyVar tv) t
    | _ -> raise UnificationError

(** Modified M algorithm *)
let rec infer (env : tp_env) (e : tagged_exp) (t : ty) :
    (substitution * path * tag list) list =
  let open Monads.List in
  (* Generate a list of s''s's from a non-branching expression with two
     subexpressions *)
  let gen_s''s's (t' : ty) (e1 : tagged_exp) (e2 : tagged_exp) (t1 : ty)
      (t2 : ty) : (substitution * (path * path) * tag list) list =
    let s = unify t t' in
    let* s', p1, tg1 = infer (subst_env s env) e1 (s t1) in
    let* s'', p2, tg2 = infer (subst_env (s' << s) env) e2 ((s' << s) t2) in
    return (s'' << s' << s, (p1, p2), tg1 @ tg2)
  in
  try
    match e with
    | TgHole tg ->
        let h_t = TyVar "τ" in
        return (unify t h_t, PtNil, [ tg ])
    | TgNum (tg, _) -> return (unify t TyInt, PtNil, [ tg ])
    | TgVar (tg, x) ->
        let x_t, _ = lookup x env in
        return (unify t x_t, PtNil, [ tg ])
    | TgPair (tg, e1, e2) ->
        let t1 = TyVar (new_var ()) in
        let t2 = TyVar (new_var ()) in
        let+ s, (p1, p2), tgl = gen_s''s's (TyPair (t1, t2)) e1 e2 t1 t2 in
        (s, PtPair (p1, p2), tg :: tgl)
    | TgFst (tg, e) ->
        let+ s, p, tgl = infer env e (TyPair (t, TyVar (new_var ()))) in
        (s, p, tg :: tgl)
    | TgSnd (tg, e) ->
        let+ s, p, tgl = infer env e (TyPair (TyVar (new_var ()), t)) in
        (s, p, tg :: tgl)
    | TgAdd (tg, e1, e2) ->
        let+ s, (p1, p2), tgl = gen_s''s's TyInt e1 e2 TyInt TyInt in
        (s, PtAdd (p1, p2), tg :: tgl)
    | TgNeg (tg, e) ->
        let s = unify t TyInt in
        let+ s', p, tgl = infer (subst_env s env) e (s TyInt) in
        (* (s TyInt) -> TyInt ?*)
        (s' << s, p, tg :: tgl)
    | TgCase (tg, x, y, z, e1, e2) ->
        let ls's_p =
          (* x binds to (y, z) *)
          let y_t = TyVar (new_var ()) in
          let z_t = TyVar (new_var ()) in
          let* s_p, x_p_p, x_p_tgl = infer env x (TyPair (y_t, z_t)) in
          let* s'_p, e1_p, e1_tgl =
            let env' = subst_env s_p env in
            let y_t' = (s_p y_t, PtNil) in
            let z_t' = (s_p z_t, PtNil) in
            infer ((y, y_t') :: (z, z_t') :: env') e1 (s_p t)
          in
          return (s'_p << s_p, PtCaseP (x_p_p, e1_p), tg :: (x_p_tgl @ e1_tgl))
        in
        let ls's_i =
          (* x is TyInt *)
          let* s_i, x_i_p, x_i_tgl = infer env x TyInt in
          let* s'_i, e2_p, e2_tgl =
            let env' = subst_env s_i env in
            infer env' e2 (s_i t)
          in
          return (s'_i << s_i, PtCaseN (x_i_p, e2_p), tg :: (x_i_tgl @ e2_tgl))
        in
        ls's_p @ ls's_i
    | TgIf (tg, e_p, e_t, e_f) ->
        let* s, e_p_p, e_p_tgl = infer env e_p TyInt in
        let* s'_t, e_t_p, e_t_tgl = infer (subst_env s env) e_t (s t)
        and+ s'_f, e_f_p, e_f_tgl = infer (subst_env s env) e_f (s t) in
        [
          (s'_t << s, PtIfTru (e_p_p, e_t_p), tg :: (e_p_tgl @ e_t_tgl));
          (s'_f << s, PtIfFls (e_p_p, e_f_p), tg :: (e_p_tgl @ e_f_tgl));
        ]
    | TgLet (tg, x, v, e) ->
        let x_t = TyVar (new_var ()) in
        let* s, v_p, v_tgl = infer env v x_t in
        let* s', e_p, e_tgl =
          let env' = subst_env s env in
          let x_tp' = (s x_t, v_p) in
          infer ((x, x_tp') :: env') e (s t)
        in
        return (s' << s, PtLet (v_p, e_p), tg :: (v_tgl @ e_tgl))
  with UnificationError -> []

let rec string_of_tagged_exp e =
  let open Colorizer in
  let parwrap t s = colorize_palette t "[" ^ s ^ colorize_palette t "]" in
  let annot t s = s ^ colorize_palette t (" : " ^ "ℓ" ^ string_of_int t) in
  match e with
  | TgHole t -> annot t "[]" |> parwrap t
  | TgNum (t, n) -> string_of_int n |> annot t |> parwrap t
  | TgVar (t, x) -> annot t x |> parwrap t
  | TgPair (t, e1, e2) ->
      "(" ^ string_of_tagged_exp e1 ^ ", " ^ string_of_tagged_exp e2 ^ ")"
      |> annot t |> parwrap t
  | TgFst (t, e) -> string_of_tagged_exp e ^ ".1" |> annot t |> parwrap t
  | TgSnd (t, e) -> string_of_tagged_exp e ^ ".2" |> annot t |> parwrap t
  | TgAdd (t, e1, e2) ->
      string_of_tagged_exp e1 ^ " + " ^ string_of_tagged_exp e2
      |> annot t |> parwrap t
  | TgNeg (t, e) -> "-" ^ string_of_tagged_exp e |> annot t |> parwrap t
  | TgCase (t, x, y, z, e1, e2) ->
      "case " ^ string_of_tagged_exp x ^ " (" ^ y ^ "," ^ z ^ ") "
      ^ string_of_tagged_exp e1 ^ " " ^ string_of_tagged_exp e2
      |> annot t |> parwrap t
  | TgIf (t, e_p, e_t, e_f) ->
      "if " ^ string_of_tagged_exp e_p ^ " " ^ string_of_tagged_exp e_t ^ " "
      ^ string_of_tagged_exp e_f
      |> annot t |> parwrap t
  | TgLet (t, x, exp, body) ->
      "let " ^ x ^ " = " ^ string_of_tagged_exp exp ^ " in "
      ^ string_of_tagged_exp body
      |> annot t |> parwrap t

let rec print_type_list (typts : type_check_info list) : unit =
  let open Colorizer in
  let rec tags_to_string = function
    | [] -> ""
    | [ hd ] -> colorize_palette hd ("ℓ" ^ string_of_int hd)
    | hd :: tl ->
        colorize_palette hd ("ℓ" ^ string_of_int hd) ^ "-" ^ tags_to_string tl
  in
  match typts with
  | { hole_type; exp_type; taken_path; tag_list; tagged_exp } :: ps ->
      let pt' =
        match taken_path with
        | PtLet (_, pt') -> pt'
        | _ -> failwith "No top-level binding for x; programming error"
      in
      print_endline (string_of_tagged_exp tagged_exp);
      print_endline
        ("| []: "
        ^ colorize 009 (string_of_type hole_type)
        ^ ", O: "
        ^ colorize 009 (string_of_type exp_type)
        ^ ", Trace: "
        ^ colorize 011 (string_of_path pt')
        ^ "; " ^ tags_to_string tag_list);
      print_type_list ps
  | [] -> ()

(** Returns the possible combinations of the [] and the output *)
let type_check (e : L.expr) (t : ty) : type_check_info list =
  let hole_type = TyVar "τ" in
  let tagged_exp = tag_exp e in

  let open Monads.List in
  let+ subst, taken_path, tag_list = infer [] tagged_exp t in
  {
    hole_type = subst hole_type;
    exp_type = subst t;
    taken_path;
    tagged_exp;
    tag_list;
  }
