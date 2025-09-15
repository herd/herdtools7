(** Command-line arguments *)

module CLI = struct
  exception CLIError of string

  let usage_msg = "aslspec [options] <spec-file1> [<spec-file2>]..."
  let arg_spec_filenames = ref []
  let arg_pp = ref false
  let arg_render = ref false
  let set_pp () = arg_pp := true
  let set_render () = arg_render := true

  let speclist =
    [
      ( "--pp",
        Arg.Unit set_pp,
        "Print a pretty-printed version of the specification." );
      ( "--render",
        Arg.Unit set_render,
        "Generate files containing LaTeX blocks for all elements of the \
         specification." );
    ]

  type configuration = { spec_files : string list; pp : bool; render : bool }

  let anon_fun filename =
    arg_spec_filenames := !arg_spec_filenames @ [ filename ]

  let parse_args () =
    Arg.parse speclist anon_fun usage_msg;
    let () =
      if Utils.list_is_empty !arg_spec_filenames then
        raise (CLIError "No specification files given!")
    in
    { spec_files = !arg_spec_filenames; pp = !arg_pp; render = !arg_render }
end

let pp_position out lexbuf =
  let open Lexing in
  let p = Lexing.lexeme_start_p lexbuf in
  Format.fprintf out "%s line %d column %d" p.pos_fname p.pos_lnum
    (p.pos_cnum - p.pos_bol + 1)

exception ParseError of string

let parse_spec_from_file filename =
  let file_channel = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr file_channel)
    (fun () ->
      let lexbuf = Lexing.from_channel file_channel in
      let () =
        lexbuf.Lexing.lex_curr_p <-
          { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
      in
      try SpecParser.spec SpecLexer.token lexbuf with
      | AST.SpecError msg ->
          let msg =
            Format.asprintf "Spec error: %s around %a" msg pp_position lexbuf
          in
          raise (ParseError msg)
      | SpecParser.Error ->
          let msg = Format.asprintf "Syntax error at %a" pp_position lexbuf in
          raise (ParseError msg)
      | SpecLexer.Error msg ->
          let msg =
            Format.asprintf "Lexical error at %a: %s" pp_position lexbuf msg
          in
          raise (ParseError msg))

let pp_std spec =
  PP.pp_spec Format.std_formatter spec;
  Format.print_newline ()

let () =
  let config = CLI.parse_args () in
  let ast =
    List.map (fun filename -> parse_spec_from_file filename) config.spec_files
    |> List.concat
  in
  let spec = Spec.from_ast ast in
  if config.pp then pp_std ast;
  if config.render then Render.render spec
