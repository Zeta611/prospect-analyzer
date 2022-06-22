exception VersionError of string

let get_program stdin = Parser.program Lexer.token (Lexing.from_channel stdin)

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
      | L.Let (_, v, e) ->
          check_version version v;
          check_version version e
      | L.Hole -> ()
      | L.Num _ -> ()
      | L.Var _ -> ())
  | 1 -> ()
  | _ -> raise (VersionError "Version not supported")

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
