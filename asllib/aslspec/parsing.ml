exception ParseError of { loc : AST.source_location; msg : string }

(** [pp_position] pretty-prints the position of the lexeme in the source file.
*)
let pp_position out lexbuf =
  let open Lexing in
  let p = Lexing.lexeme_start_p lexbuf in
  Format.fprintf out "%s line %d column %d" p.pos_fname p.pos_lnum
    (p.pos_cnum - p.pos_bol + 1)

(** [parse_spec_from_lexbuf lexbuf filename] parses a specification from the
    given lexing buffer [lexbuf] and uses [filename] as the source file name for
    error reporting. *)
let parse_spec_from_lexbuf lexbuf filename =
  let () =
    lexbuf.Lexing.lex_curr_p <-
      { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
  in
  try SpecParser.spec SpecLexer.token lexbuf with
  | AST.SpecError { loc; msg } -> raise (ParseError { loc; msg })
  | SpecParser.Error ->
      let msg = Format.asprintf "Syntax error at %a" pp_position lexbuf in
      let loc =
        {
          AST.start_pos = lexbuf.Lexing.lex_start_p;
          end_pos = lexbuf.Lexing.lex_curr_p;
        }
      in
      raise (ParseError { loc; msg })
  | SpecLexer.Error msg ->
      let msg =
        Format.asprintf "Lexical error at %a: %s" pp_position lexbuf msg
      in
      let loc =
        {
          AST.start_pos = lexbuf.Lexing.lex_start_p;
          end_pos = lexbuf.Lexing.lex_curr_p;
        }
      in
      raise (ParseError { loc; msg })

let parse_spec_from_file filename =
  let file_channel = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr file_channel)
    (fun () ->
      let lexbuf = Lexing.from_channel file_channel in
      parse_spec_from_lexbuf lexbuf filename)

let parse_spec_from_string ~spec ~filename =
  let lexbuf = Lexing.from_string spec in
  parse_spec_from_lexbuf lexbuf filename
