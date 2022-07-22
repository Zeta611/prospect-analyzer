open Analyzer
open Shape_analyzer

let l_program = Util.get_program stdin

(* samples is a list of input-output pairs *)
let version, samples, root_expr = l_program
let () = Util.check_version version root_expr

let converted_samples =
  List.map (fun (i, o) -> (L.hvalue_of_vvalue i, L.hvalue_of_vvalue o)) samples

(* Process version *)
(* let _ = print_string "Interpreter version: L" *)
let _ = print_string "Type checker version: L"
let _ = print_int version
let _ = print_newline ()

let out_types =
  let type_of_hvalue hvalue =
    match type_of_hvalue hvalue with
    | Some t -> t
    | None -> failwith "Hole should not exist in output"
  in
  List.map
    (fun (i, t) -> type_check (L.Let ("x", i, root_expr)) t)
    (List.map
       (fun p -> (L.expr_of_hvalue @@ fst @@ p, type_of_hvalue @@ snd @@ p))
       converted_samples)

let () =
  if List.flatten out_types = [] then print_endline "Unsatisfiable!"
  else List.iter print_type_list out_types
