(** Extracts tokens from SpecLexer.mll and prints TokenStrings.ml *)

(** {[
      Matches a line like
      | "rule"                { RULE }
    ]} *)
let identifier_token_line_regexp =
  Str.regexp
    "^[ \t]*|[ \t]*\"\\([^\"]+\\)\"[ \t]*{[ \t]*\\([A-Z_0-9]+\\)[ \t]*}"

(** [extract_from_line line] tries to extract a token from a line of the lexer
    file. If the line matches the expected format, it returns
    [Some (token_name, string_literal)], otherwise it returns [None]. *)
let extract_from_line line =
  if Str.string_match identifier_token_line_regexp line 0 then
    let lit = Str.matched_group 1 line in
    let tok = Str.matched_group 2 line in
    Some (tok, lit)
  else None

(** [extract_tokens lexer_file] scans [lexer_file] line by line, extracts
    tokens, and return a list of (token_name, string_literal) pairs. *)
let extract_tokens lexer_file =
  let ic = open_in lexer_file in
  let rec scan_lines token_entries =
    match input_line ic with
    | line ->
        let token_entries =
          match extract_from_line line with
          | Some entry -> entry :: token_entries
          | None -> token_entries
        in
        scan_lines token_entries
    | exception End_of_file ->
        close_in ic;
        List.rev token_entries
  in
  scan_lines []

let generate_module tokens =
  let func_prologue =
    {|
(* Auto-generated from SpecLexer.mll - DO NOT EDIT *)
let string_of_token =
  let open SpecParser in
  function
|}
  in
  let func_epilogue =
    {|
  | _ -> failwith "string_of_token: unsupported token"
|}
  in
  let pp_token_entry fmt (token_name, string_literal) =
    Format.fprintf fmt "  | %s -> %S" token_name string_literal
  in
  Format.asprintf "%s%a%s" func_prologue
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_token_entry)
    tokens func_epilogue

let print_usage_and_exit () =
  Printf.eprintf "Usage: %s <lexer_file>\n" Sys.argv.(0);
  exit 1

let () =
  let lexer_file =
    if Array.length Sys.argv = 2 then Sys.argv.(1) else print_usage_and_exit ()
  in
  let tokens = extract_tokens lexer_file in
  let module_content = generate_module tokens in
  print_endline module_content
