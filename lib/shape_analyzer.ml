open Path

exception TypeError of string

type id = string

(* Type check *)
type ty =
  | TyInt
  | TyPair of ty * ty
  | TyHole (* of hvtype? *)
  | TyVar of tyvar

and tyvar = string

type hvty =
  | HVRoot
  | HVLeft of hvty
  | HVRight of hvty

type tp_env = (id * (ty * path)) list

let rec type_of_hvalue = function
  | L.HHole -> None
  | L.HNum _ -> Some TyInt
  | L.HPair (a, b) ->
      let open Monads.Option in
      let* ta = type_of_hvalue a in
      let* tb = type_of_hvalue b in
      return (TyPair (ta, tb))

exception UnificationError

let rec string_of_type = function
  | TyInt -> "ι"
  | TyPair (e1, e2) -> "(" ^ string_of_type e1 ^ ", " ^ string_of_type e2 ^ ")"
  | TyHole -> "[]"
  | TyVar tv -> tv

let var_count = ref 0

let new_var () =
  let _ = var_count := !var_count + 1 in
  "τ" ^ string_of_int !var_count

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
    | TgNum (tg, _) -> [ (unify t TyInt, PtNil, [ tg ]) ]
    | TgVar (tg, x) ->
        let x_t, _ = lookup x env in
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
                    (s' @* s, PtCaseP (x_p, e1_p), tg :: (x_tgl @ e1_tgl)))
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
                    (s' @* s, PtCaseN (x_p, e2_p), tg :: (x_tgl @ e2_tgl)))
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
                      tg :: (e_p_tgl @ e_tf_tgl) ))
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

let rec print_type_list (typts : (ty * ty * path * tag list) list) : unit =
  let open Colorizer in
  let rec tags_to_string = function
    | [] -> ""
    | [ hd ] -> colorize_palette hd ("ℓ" ^ string_of_int hd)
    | hd :: tl ->
        colorize_palette hd ("ℓ" ^ string_of_int hd) ^ "-" ^ tags_to_string tl
  in
  match typts with
  | (ht, ot, pt, tgl) :: ps ->
      let pt' =
        match pt with
        | PtLet (_, pt') -> pt'
        | _ -> failwith "No top-level binding for x; programming error"
      in
      print_endline
        ("| []: "
        ^ colorize 009 (string_of_type ht)
        ^ ", O: "
        ^ colorize 009 (string_of_type ot)
        ^ ", Trace: "
        ^ colorize 011 (string_of_path pt')
        ^ "; " ^ tags_to_string tgl);
      print_type_list ps
  | [] -> ()

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

(* Returns the possible combinations of the [] and the output *)
let type_check (e : L.expr) (t : ty) : (ty * ty * path * tag list) list =
  let hole_type = TyVar "τ" in
  let tagged_e = tag_exp e in
  (* let tagged_e' = *)
  (*   match tagged_e with *)
  (*   | TgLet (_, _, _, e) -> e *)
  (*   | _ -> failwith "No top-level binding for x; programming error" *)
  (* in *)
  let _ = print_endline (string_of_tagged_exp tagged_e) in
  let ls = infer [] tagged_e t in
  List.map (fun (subst, pt, tgl) -> (subst hole_type, subst t, pt, tgl)) ls
