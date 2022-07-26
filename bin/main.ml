open Analyzer
open Shape_analyzer

let usage_msg = "[-shape]"
let shape_analysis = ref false
let input_files = ref []
let anon_fun filename = input_files := filename :: !input_files
let speclist = [ ("-shape", Arg.Set shape_analysis, "Shape analysis") ]

let _ =
  Arg.parse speclist anon_fun usage_msg;

  let open Monads.List in
  let+ filename = !input_files in

  let l_program = filename |> In_channel.open_text |> Util.get_program in
  (* samples is a list of input-output pairs *)
  let version, samples, root_expr = l_program in
  let () = Util.check_version version root_expr in

  let converted_samples =
    let+ i, o = samples in
    L.(hvalue_of_vvalue i, hvalue_of_vvalue o)
  in

  if !shape_analysis then (
    (* Process version *)
    (* let _ = print_string "Interpreter version: L" *)
    Printf.printf "Shape analyzer version: L%d\n" version;
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
    in

    let () =
      if List.flatten out_types = [] then print_endline "Unsatisfiable!"
      else List.iter print_type_list out_types
    in
    ())
  else ()
