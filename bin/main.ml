open Analyzer

let usage_msg = "[-shape]"
let shape_analysis = ref false
let input_files = ref []
let anon_fun filename = input_files := filename :: !input_files
let speclist = [ ("-shape", Arg.Set shape_analysis, "Shape analysis") ]
let first_flag = ref true

let _ =
  Arg.parse speclist anon_fun usage_msg;
  input_files := List.rev !input_files;

  let open Monads.List in
  let+ filename = !input_files in

  let is_first =
    if !first_flag then (
      first_flag := false;
      true)
    else false
  in

  let l_program = filename |> In_channel.open_text |> Util.get_program in
  (* samples is a list of input-output pairs *)
  let version, samples, root_expr = l_program in
  let () = Util.check_version version root_expr in

  let converted_samples =
    let+ i, o = samples in
    L.(hvalue_of_vvalue i, hvalue_of_vvalue o)
  in

  let log analyzer_kind =
    if not is_first then print_newline () else ();
    Printf.printf "%s analyzer. %s (L%d)\n" analyzer_kind filename version
  in

  if !shape_analysis then (
    log "Shape";

    let open Shape_analyzer in
    let out_types =
      let type_of_hvalue hvalue =
        match type_of_hvalue hvalue with
        | Some t -> t
        | None -> failwith "Hole should not exist in output"
      in
      let+ i, o = converted_samples in
      let iv, ot = (L.expr_of_hvalue i, type_of_hvalue o) in
      type_check (L.Let ("x", iv, root_expr)) ot
    in

    if List.flatten out_types = [] then print_endline "Unsatisfiable!"
    else List.iter print_type_list out_types)
  else (
    log "Value";

    let open Value_analyzer in
    let empty_env _ = raise (RunError "undefined variable") in
    let _ =
      let+ i, _ = converted_samples in

      (* TODO: merge hvalue' and hvalue *)
      let input_bound_env = ("x", hvalue'_of_hvalue i) @: empty_env in
      let result = eval input_bound_env root_expr in
      result |> string_of_hvalue' |> print_endline
    in
    ())
