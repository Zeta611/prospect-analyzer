open Analyzer

let usage_msg = "[-all]"
let all_analyses = ref false
let input_files = ref []
let anon_fun filename = input_files := filename :: !input_files
let speclist = [ ("-all", Arg.Set all_analyses, "Shape and value analyses") ]
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

  let log analyzer_kind =
    if not is_first then print_newline () else ();
    Printf.printf "%s analyzer. %s (L%d)\n" analyzer_kind filename version
  in

  if !all_analyses then (
    log "Shape & value";

    let open Shape_analyzer in
    let all_samples_out_types =
      let+ i, o = samples in
      let iv, ot = (L.expr_of_value i, type_of_plain_value o) in
      let root_expr = L.Let ("x", iv, root_expr) in
      let out_types = type_check root_expr ot in
      Printf.printf "Sample: (%s, %s)\n" (L.string_of_exp iv)
        (L.string_of_exp (L.expr_of_value o));
      if out_types = [] then print_endline "Shape_analyzer: Unsatisfiable";

      let* ({ hole_type; taken_path; _ } as out_type) = out_types in
      let open Value_analyzer in
      let empty_env _ = raise (RunError "undefined variable") in
      let input_bound_env =
        ("x", value_of_plain_value i ~hole_cnt:(count_holes hole_type))
        @: empty_env
      in
      print_type_check_info out_type;
      match eval input_bound_env root_expr taken_path hole_type with
      | result ->
          Printf.printf "| Eval: %s\n" (string_of_value result);
          return (out_type, result)
      | exception PathError msg ->
          Printf.printf "| %s\n" msg;
          []
    in
    if List.flatten all_samples_out_types = [] then
      print_endline "Unsatisfiable for all samples!")
  else (
    log "Shape";

    let open Shape_analyzer in
    let all_samples_out_types =
      let+ i, o = samples in
      let iv, ot = (L.expr_of_value i, type_of_plain_value o) in
      let out_types = type_check (L.Let ("x", iv, root_expr)) ot in
      Printf.printf "Sample: (%s, %s)\n" (L.string_of_exp iv)
        (L.string_of_exp (L.expr_of_value o));
      if out_types = [] then print_endline "Unsatisfiable"
      else print_type_list out_types;
      print_newline ();
      out_types
    in
    if List.flatten all_samples_out_types = [] then
      print_endline "Unsatisfiable for all samples!")
