type number = int
type id = string
type tag = int

(** Taken code path *)
type path =
  | PtNil
  | PtPair of path * path
  | PtAdd of path * path
  | PtCaseP of path * path
  | PtCaseN of path * path
  | PtIfTru of path * path
  | PtIfFls of path * path
  | PtLet of path * path

let rec string_of_path = function
  | PtNil -> "."
  | PtPair (p1, p2) -> "(" ^ string_of_path p1 ^ ", " ^ string_of_path p2 ^ ")"
  | PtAdd (p1, p2) -> string_of_path p1 ^ " + " ^ string_of_path p2
  | PtCaseP (p1, p2) ->
      "case (" ^ string_of_path p1 ^ ") : * " ^ string_of_path p2
  | PtCaseN (p1, p2) ->
      "case (" ^ string_of_path p1 ^ ") : ι " ^ string_of_path p2
  | PtIfTru (p1, p2) ->
      "if (" ^ string_of_path p1 ^ ") ≠ 0 " ^ string_of_path p2
  | PtIfFls (p1, p2) ->
      "if (" ^ string_of_path p1 ^ ") = 0 " ^ string_of_path p2
  | PtLet (p1, p2) ->
      "let . = (" ^ string_of_path p1 ^ ") in (" ^ string_of_path p2 ^ ")"

type tagged_exp =
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

(* let _ = tag_count := !tag_count + 1 in *)
(* "ℓ" ^ string_of_int !tag_count *)
let tag_exp (e : L.expr) : tagged_exp =
  let tag_count = ref 0 in
  let new_tag () =
    incr tag_count;
    !tag_count
  in
  let rec inner (e : L.expr) =
    let tg = new_tag () in
    match e with
    | Hole -> TgHole tg
    | Num n -> TgNum (tg, n)
    | Var x -> TgVar (tg, x)
    | Pair (e1, e2) ->
        let te1 = inner e1 in
        let te2 = inner e2 in
        TgPair (tg, te1, te2)
    | Fst e -> TgFst (tg, inner e)
    | Snd e -> TgSnd (tg, inner e)
    | Add (e1, e2) ->
        let te1 = inner e1 in
        let te2 = inner e2 in
        TgAdd (tg, te1, te2)
    | Neg e -> TgNeg (tg, inner e)
    | Case (x, y, z, e1, e2) ->
        let tx = inner x in
        let te1 = inner e1 in
        let te2 = inner e2 in
        TgCase (tg, tx, y, z, te1, te2)
    | If (e_p, e_t, e_f) ->
        let te_p = inner e_p in
        let te_t = inner e_t in
        let te_f = inner e_f in
        TgIf (tg, te_p, te_t, te_f)
    | Let (x, exp, body) ->
        let te = inner exp in
        let tb = inner body in
        TgLet (tg, x, te, tb)
  in
  inner e
