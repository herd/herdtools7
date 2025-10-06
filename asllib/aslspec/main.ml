(** A module for parsing command-line arguments and setting command-line
    options. *)
module CLI = struct
  exception CLIError of string

  let usage_msg = "aslspec [options] <spec-file1> [<spec-file2>]..."
  let arg_spec_filenames = ref []
  let arg_pp = ref false
  let arg_render = ref false
  let arg_render_filename = ref "generated_macros.tex"
  let arg_render_debug = ref false
  let arg_render_debug_filename = ref "debug_generated_elements.tex"
  let set_pp () = arg_pp := true
  let set_render () = arg_render := true

  let set_render_out filename =
    set_render ();
    arg_render_filename := filename

  let set_render_debug filename =
    set_render ();
    arg_render_debug := true;
    arg_render_debug_filename := filename

  let speclist =
    [
      ( "--pp",
        Arg.Unit set_pp,
        "Print a pretty-printed version of the specification." );
      ( "--render",
        Arg.Unit set_render,
        "Generate files containing LaTeX macros for all elements of the \
         specification." );
      ( "--render-out",
        Arg.String set_render_out,
        "Specify a filename for the generated LaTeX macros. Implies --render."
      );
      ( "--render-debug",
        Arg.String set_render_debug,
        "Specify a filename for a stand-alone rendering of all generated LaTeX \
         macros. Implies --render." );
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
          let msg = Format.asprintf "%s around %a" msg pp_position lexbuf in
          raise (ParseError msg)
      | SpecParser.Error ->
          let msg = Format.asprintf "Syntax error at %a" pp_position lexbuf in
          raise (ParseError msg)
      | SpecLexer.Error msg ->
          let msg =
            Format.asprintf "Lexical error at %a: %s" pp_position lexbuf msg
          in
          raise (ParseError msg))

(** Pretty-print the specification to standard output. *)
let pp_std spec =
  PP.pp_spec Format.std_formatter spec;
  Format.print_newline ()

let parse_command_line_args_and_execute () =
  let open CLI in
  let config = parse_args () in
  let ast =
    (* Parse the abstract syntax tree (AST) from all specification files. *)
    List.map (fun filename -> parse_spec_from_file filename) config.spec_files
    |> List.concat
  in
  if config.pp then pp_std ast;
  let spec = Spec.from_ast ast in
  if config.render then
    let generated_macros_filename = !arg_render_filename in
    let open AST in
    let file_channel = open_out_bin generated_macros_filename in
    let () =
      Fun.protect
        ~finally:(fun () -> close_out_noerr file_channel)
        (fun () ->
          let file_formatter = Format.formatter_of_out_channel file_channel in
          Render.render spec file_formatter;
          Format.fprintf Format.std_formatter
            "%sGenerated LaTeX macros into %s\n%s" Text.green
            generated_macros_filename Text.reset_color)
    in
    if !arg_render_debug then
      let debug_generated_elements_filename = !arg_render_debug_filename in
      let file_channel = open_out_bin debug_generated_elements_filename in
      Fun.protect
        ~finally:(fun () -> close_out_noerr file_channel)
        (fun () ->
          let file_formatter = Format.formatter_of_out_channel file_channel in
          Render.render_debug spec file_formatter;
          Format.fprintf Format.std_formatter
            "%sGenerated stand-alone LaTeX file into %s\n%s" Text.green
            debug_generated_elements_filename Text.reset_color)

(** Main entry point. Runs aslspec for the command-line options. *)
let () =
  try parse_command_line_args_and_execute () |> fun () -> exit 0
  with error ->
    let error_type, msg =
      match error with
      | CLI.CLIError msg -> ("Usage Error", msg)
      | ParseError msg -> ("Syntax Error", msg)
      | AST.SpecError msg -> ("Specification Error", msg)
      | _ -> raise error
    in
    Format.eprintf "%s%s: %s%s\n" Text.red error_type msg Text.reset_color;
    exit 1
