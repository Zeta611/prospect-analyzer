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
