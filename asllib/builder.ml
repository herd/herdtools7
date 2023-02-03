open Lexing

type version = [ `ASLv0 | `ASLv1 ]
type version_selector = [ `ASLv0 | `ASLv1 | `Any ]

let from_lexbuf version lexbuf =
  let open Error in
  let cannot_parse lexbuf =
    fatal_here lexbuf.lex_start_p lexbuf.lex_curr_p CannotParse
  in
  let unknown_symbol lexbuf =
    fatal_here lexbuf.lex_start_p lexbuf.lex_curr_p UnknownSymbol
  in
  match version with
  | `ASLv1 -> (
      try Parser.ast Lexer.token lexbuf with
      | Parser.Error -> cannot_parse lexbuf
      | Lexer.LexerError -> unknown_symbol lexbuf)
  | `ASLv0 -> (
      try Gparser0.parse Lexer0.token lexbuf
      with Parser0.Error -> cannot_parse lexbuf)

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

let from_file version f =
  let lexbuf, chan = open_file f in
  close_after chan @@ fun () -> from_lexbuf version lexbuf

let from_file_result version f =
  let lexbuf, chan = open_file f in
  let res = Error.intercept (fun () -> from_lexbuf version lexbuf) () in
  close_in chan;
  res

let from_lexer_lexbuf version _lexer lexbuf =
  Error.intercept (fun () -> from_lexbuf version lexbuf) ()

let from_file_multi_version = function
  | `Any -> (
      fun fname ->
        match from_file_result `ASLv0 fname with
        | Error e ->
            let () =
              Format.eprintf
                "@[Ignoring error on parser v0: %a.@ Trying with parser v1 \
                 ...@]@."
                Error.pp_error e
            in
            from_file_result `ASLv1 fname
        | Ok ast -> Ok ast)
  | #version as version -> from_file_result version
