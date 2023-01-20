open Asllib

(* Taken from herdtools7/lib/Pos.ml *)
let pp_pos chan pos =
  let open Lexing in
  Printf.fprintf chan "File \"%s\", line %i, character %i" pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)

let exec_tests =
  let exec_one_test any_failed (name, f) =
    let on_fail e =
      Printf.printf "failed 𐄂\n";
      any_failed := Some e
    in
    Printf.printf "[asl-test] Running %s ... %!" name;
    try
      f ();
      Printf.printf "ok ✔︎\n"
    with
    | Error.ASLException err as e ->
        Format.eprintf "%a@." Error.pp_error err;
        on_fail e
    | e -> on_fail e
  in
  fun li ->
    let any_failed = ref None in
    List.iter (exec_one_test any_failed) li;
    match !any_failed with Some e -> raise e | None -> ()

let build_ast_from_file f =
  let lexbuf = Lexing.from_channel (open_in f) in
  let () =
    lexbuf.Lexing.lex_curr_p <-
      { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = f }
  in
  try Parser.ast Lexer.token lexbuf with
  | Parser.Error as e ->
      Printf.eprintf "%a: Cannot parse.\n%!" pp_pos lexbuf.Lexing.lex_curr_p;
      raise e
  | Lexer.LexerError as e ->
      Printf.eprintf "%a: unknown token.\n%!" pp_pos lexbuf.Lexing.lex_curr_p;
      raise e
