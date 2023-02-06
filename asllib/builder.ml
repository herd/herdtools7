open Lexing

type token = Parser.token
type ast_type = [ `Opn | `Ast ]
type version = [ `ASLv0 | `ASLv1 ]
type version_selector = [ `ASLv0 | `ASLv1 | `Any ]

let select_type ~opn ~ast = function
  | Some `Opn -> opn
  | Some `Ast -> ast
  | None -> ast

let _ast_type_to_string = select_type ~opn:"Opn" ~ast:"Ast"

let from_lexbuf ast_type version (lexbuf : lexbuf) =
  let open Error in
  let () =
    if false then
      Format.eprintf "Parsing %s from file %s@."
        (_ast_type_to_string ast_type)
        lexbuf.lex_curr_p.pos_fname
  in
  let cannot_parse lexbuf =
    fatal_here lexbuf.lex_start_p lexbuf.lex_curr_p CannotParse
  in
  let unknown_symbol lexbuf =
    fatal_here lexbuf.lex_start_p lexbuf.lex_curr_p UnknownSymbol
  in
  match version with
  | `ASLv1 -> (
      let parse = select_type ~opn:Parser.opn ~ast:Parser.ast ast_type in
      try parse Lexer.token lexbuf with
      | Parser.Error -> cannot_parse lexbuf
      | Lexer.LexerError -> unknown_symbol lexbuf)
  | `ASLv0 -> (
      let parse = select_type ~opn:Gparser0.opn ~ast:Gparser0.ast ast_type
      and lexer0 = Lexer0.token () in
      try parse lexer0 lexbuf with Parser0.Error -> cannot_parse lexbuf)

let close_after chan f =
  try
    let res = f () in
    close_in chan;
    res
  with e ->
    close_in_noerr chan;
    raise e

let open_file filename =
  let chan = open_in filename in
  let lexbuf = from_channel chan in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  (lexbuf, chan)

let from_file ?ast_type version f =
  let lexbuf, chan = open_file f in
  close_after chan @@ fun () -> from_lexbuf ast_type version lexbuf

let from_file_result ?ast_type version f =
  let lexbuf, chan = open_file f in
  let res =
    Error.intercept (fun () -> from_lexbuf ast_type version lexbuf) ()
  in
  close_in chan;
  res

let from_lexer_lexbuf ?ast_type version _lexer lexbuf =
  Error.intercept (fun () -> from_lexbuf ast_type version lexbuf) ()

let from_file_multi_version ?ast_type = function
  | `Any -> (
      fun fname ->
        match from_file_result ?ast_type `ASLv0 fname with
        | Error e ->
            let () =
              Format.eprintf
                "@[Ignoring error on parser v0: %a.@ Trying with parser v1 \
                 ...@]@."
                Error.pp_error e
            in
            from_file_result ?ast_type `ASLv1 fname
        | Ok ast -> Ok ast)
  | (`ASLv0 | `ASLv1) as version -> from_file_result ?ast_type version

let rec list_first_opt f = function
  | [] -> None
  | h :: t -> ( match f h with Some x -> Some x | None -> list_first_opt f t)

let stdlib =
  let ( / ) = Filename.concat in
  let to_try = [ Version.libdir / "asllib"; "asllib" / "libdir" ] in
  let to_try =
    match Sys.getenv_opt "ASL_LIBDIR" with
    | None -> to_try
    | Some p -> p :: to_try
  in
  let to_try =
    match Sys.getenv_opt "DUNE_SOURCEROOT" with
    | None -> to_try
    | Some p -> (p / "asllib" / "libdir") :: to_try
  in
  let try_one dir_path =
    let file_path = dir_path / "stdlib.asl" in
    if Sys.file_exists file_path then
      match from_file_result ~ast_type:`Ast `ASLv1 file_path with
      | Ok ast -> Some ast
      | Error _ -> None
    else None
  in
  lazy
    (match list_first_opt try_one to_try with
    | Some ast -> ast
    | None -> raise Not_found)
