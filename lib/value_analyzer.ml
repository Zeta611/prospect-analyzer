open L

(* Types and exceptions *)
module HoleCoeffs = struct
  type t = int list [@@deriving show]

  type cond_checker = {
    context : Z3.context;
    solver : Z3.Solver.solver;
  }

  let zero len = List.init len (fun _ -> 0)

  let rec make ~index ~k ~hole_cnt =
    assert (index >= 0 && hole_cnt >= 0 && index <= hole_cnt);
    if index = 0 then k :: zero hole_cnt
    else 0 :: make ~index:(index - 1) ~k ~hole_cnt:(hole_cnt - 1)

  let hole_count n = List.length n - 1

  let ( +! ) h1 h2 =
    let open Monads.List in
    let+ k1 = h1 and+ k2 = h2 in
    k1 + k2

  let ( ~-! ) h =
    let open Monads.List in
    let+ k = h in
    -k

  let to_z3_expr context n =
    let open Z3.Arithmetic in
    let xs = ref [] in
    for k = hole_count n - 1 downto 0 do
      let x = Integer.mk_const_s context ("x_" ^ string_of_int k) in
      let k = Integer.mk_numeral_i context (List.nth n (k + 1)) in
      let kx = mk_mul context [ k; x ] in
      xs := kx :: !xs
    done;
    let a = Integer.mk_numeral_i context (List.hd n) in
    mk_add context (a :: !xs)

  let can_be_zero { context; solver } n =
    let open Z3 in
    let n_expr = to_z3_expr context n in
    let z_expr = Arithmetic.Integer.mk_numeral_i context 0 in
    let phi = Boolean.mk_eq context n_expr z_expr in
    Solver.add solver [ phi ];
    match Solver.check solver [] with
    | SATISFIABLE -> true
    | UNSATISFIABLE -> false
    | UNKNOWN ->
        Printf.eprintf "CAN_BE_ZERO: %s with %s" "Unknown Z3 result" (show n);
        true

  let can_be_nonzero { context; solver } n =
    let open Z3 in
    let n_expr = to_z3_expr context n in
    let z_expr = Arithmetic.Integer.mk_numeral_i context 0 in
    let phi = Boolean.mk_not context @@ Boolean.mk_eq context n_expr z_expr in
    Solver.add solver [ phi ];
    match Solver.check solver [] with
    | SATISFIABLE -> true
    | UNSATISFIABLE -> false
    | UNKNOWN ->
        Printf.eprintf "CAN_BE_NONZERO: %s with %s" "Unknown Z3 result" (show n);
        true
end

type value =
  | VNum of HoleCoeffs.t
  | VPair of value * value

let rec value_of_plain_value v ~hole_cnt =
  match v with
  | `Num n -> VNum HoleCoeffs.(make ~index:0 ~k:n ~hole_cnt)
  | `Pair (h1, h2) ->
      VPair
        (value_of_plain_value h1 ~hole_cnt, value_of_plain_value h2 ~hole_cnt)

(** Count the number of the leaves in the hole type inferred by Shape_analyzer *)
let rec count_holes : Shape_analyzer.ty -> number = function
  | TyInt | TyVar _ -> 1
  | TyPair (t1, t2) -> count_holes t1 + count_holes t2

(** Convert the type of the hole inferred by Shape_analyzer into value. *)
let value_of_hole_type (t : Shape_analyzer.ty) : value =
  let hole_cnt = count_holes t in
  let hole_id = ref 0 in

  let open Shape_analyzer in
  let rec inner = function
    | TyInt ->
        incr hole_id;
        VNum (HoleCoeffs.make ~index:!hole_id ~k:1 ~hole_cnt)
    | TyVar _ ->
        incr hole_id;
        VNum (HoleCoeffs.make ~index:!hole_id ~k:987654321 ~hole_cnt)
    | TyPair (t1, t2) -> VPair (inner t1, inner t2)
  in
  inner t

let rec string_of_value = function
  | VNum hole_coeffs -> (
      match hole_coeffs with
      | [] -> failwith "Empty HoleCoeffs: This is a programming error!"
      | [ n ] -> string_of_int n
      | n :: ks ->
          string_of_int n
          ^
          let rec string_of index = function
            | [] -> ""
            | k :: ks when k <> 0 ->
                " + "
                ^ (if k <> 1 then string_of_int k else "")
                ^ "[" ^ string_of_int index ^ "]"
                ^ if ks <> [] then " + " ^ string_of (index + 1) ks else ""
            | _ :: ks -> string_of (index + 1) ks
          in
          string_of 1 ks)
  | VPair (h1, h2) -> "(" ^ string_of_value h1 ^ ", " ^ string_of_value h2 ^ ")"

type id = string
type env = id -> value

exception TypeError of string
exception RunError of string

exception PathError of string
(** Use PathError when the path guided by Shape_analyzer is impossible *)

let raiseTypeError expected op =
  let expected, current =
    let n, p = ("number", "pair") in
    match expected with `Num -> (n, p) | `Pair -> (p, n)
  in
  let msg = Printf.sprintf "%s: %s expected, not a %s" op expected current in
  raise (TypeError msg)

(** Environment augmentation. Use @: to bind (x, v) to f *)
let ( @: ) (x, v) e y = if y = x then v else e y

let eval env expr guide_path hole_type (output : L.plain_value) =
  let hole = value_of_hole_type hole_type in
  let hole_cnt = count_holes hole_type in

  let open HoleCoeffs in
  let checker =
    let context = Z3.mk_context [ ("model", "false"); ("proof", "false") ] in
    { context; solver = Z3.Solver.mk_solver context None }
  in
  let rec inner env expr (guide_path : Path.path) (checker : cond_checker) =
    match (expr, guide_path) with
    | Hole, PtNil -> hole
    | Num n, PtNil -> VNum HoleCoeffs.(make ~index:0 ~k:n ~hole_cnt)
    | Pair (e1, e2), PtPair (p1, p2) ->
        let v1 = inner env e1 p1 checker in
        let v2 = inner env e2 p2 checker in
        VPair (v1, v2)
    | Fst e, p -> (
        match inner env e p checker with
        | VPair (fst, _) -> fst
        | VNum _ -> raiseTypeError `Pair "FIRST")
    | Snd e, p -> (
        match inner env e p checker with
        | VPair (_, snd) -> snd
        | VNum _ -> raiseTypeError `Pair "SECOND")
    | Add (e1, e2), PtAdd (p1, p2) -> (
        match inner env e1 p1 checker with
        | VNum lhs_n -> (
            match inner env e2 p2 checker with
            | VNum rhs_n -> VNum HoleCoeffs.(lhs_n +! rhs_n)
            | VPair _ -> raiseTypeError `Num "ADD")
        | VPair _ -> raiseTypeError `Num "ADD")
    | Neg e, p -> (
        match inner env e p checker with
        | VNum n -> VNum HoleCoeffs.(~-!n)
        | VPair _ -> raiseTypeError `Num "NEGATE")
    | Case (x, y, z, e1, _), PtCaseP (x_p_p, e1_p) -> (
        match inner env x x_p_p checker with
        | VPair (v1, v2) ->
            let env' = (y, v1) @: (z, v2) @: env in
            inner env' e1 e1_p checker
        | VNum _ ->
            failwith
              "VNum found when guide_path expected VPair: This is a \
               programming error. 'Well typed' program cannot go wrong!")
    | Case (x, _, _, _, e2), PtCaseN (x_n_p, e2_p) -> (
        match inner env x x_n_p checker with
        | VPair _ ->
            failwith
              "VPair found when guide_path expected VNum: This is a \
               programming error. 'Well typed' program cannot go wrong!"
        | VNum _ -> inner env e2 e2_p checker)
    | If (pred, true_e, _), PtIfTru (e_p_p, e_t_p) -> (
        match inner env pred e_p_p checker with
        | VNum n ->
            if HoleCoeffs.can_be_nonzero checker n then
              inner env true_e e_t_p checker
            else
              raise
                (PathError
                   "Falsy value found when guide_path expected a truthy value")
        | VPair _ -> raiseTypeError `Num "IF")
    | If (pred, _, false_e), PtIfFls (e_p_p, e_f_p) -> (
        match inner env pred e_p_p checker with
        | VNum n ->
            if HoleCoeffs.can_be_zero checker n then
              inner env false_e e_f_p checker
            else
              raise
                (PathError
                   "Falsy value found when guide_path expected a truthy value")
        | VPair _ -> raiseTypeError `Num "IF")
    | Let (x, exp, body), PtLet (v_p, e_p) ->
        let v = inner env exp v_p checker in
        inner ((x, v) @: env) body e_p checker
    | Var x, PtNil -> env x
    | e, p ->
        failwith
          (Printf.sprintf
             "Path mismatch: This is a programming error\nexpr: %s\npath: %s\n"
             (string_of_exp e) (Path.string_of_path p))
  in
  let rec unify result output =
    match (result, output) with
    | VNum n_r, `Num n_o ->
        let open HoleCoeffs in
        let n = n_r +! make ~index:0 ~k:(-n_o) ~hole_cnt:(hole_count n_r) in
        (* TODO: Make use of cond_eqns *)
        let checker =
          let context =
            Z3.mk_context [ ("model", "false"); ("proof", "false") ]
          in
          { context; solver = Z3.Solver.mk_solver context None }
        in
        can_be_zero checker n
    | VPair (p1_r, p2_r), `Pair (p1_o, p2_o) ->
        unify p1_r p1_o && unify p2_r p2_o
    | VNum _, `Pair _ -> raiseTypeError `Pair "UNIFY"
    | VPair _, `Num _ -> raiseTypeError `Num "UNIFY"
  in
  let result = inner env expr guide_path checker in
  (unify result output, result)
