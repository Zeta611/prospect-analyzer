open Analyzer
open Shape_analyzer

let l_program = Util.get_program stdin

(* samples is a list of input-output pairs *)
let version, samples, root_expr = l_program
let () = Util.check_version version root_expr

let converted_samples =
  List.map (fun (i, o) -> (L.vvalue_to_hvalue i, L.vvalue_to_hvalue o)) samples

(* Process version *)
(* let _ = print_string "Interpreter version: L" *)
let _ = print_string "Type checker version: L"
let _ = print_int version
let _ = print_newline ()

let out_types =
  let rec val_to_expr = function
    | L.HHole -> L.Hole
    | L.HNum n -> L.Num n
    | L.HPair (v1, v2) -> L.Pair (val_to_expr v1, val_to_expr v2)
  in
  let rec val_to_type = function
    | L.HHole -> failwith "Hole should not exist in output"
    | L.HNum _ -> TyInt
    | L.HPair (v1, v2) -> TyPair (val_to_type v1, val_to_type v2)
  in
  List.map
    (fun (i, t) -> type_check (L.Let ("x", i, root_expr)) t)
    (List.map
       (fun p -> (val_to_expr @@ fst @@ p, val_to_type @@ snd @@ p))
       converted_samples)

let () =
  if List.flatten out_types = [] then print_endline "Unsatisfiable!"
  else List.iter print_type_list out_types
