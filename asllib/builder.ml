open Lexing

let from_lexbuf version lexbuf =
  let open Error in
  let cannot_parse lexbuf = fatal @@ CannotParse lexbuf.lex_curr_p in
  let unknown_symbol lexbuf = fatal @@ UnknownSymbol lexbuf.lex_curr_p in
  match version with
  | `ASLv1 -> (
      try Parser.ast Lexer.token lexbuf with
      | Parser.Error -> cannot_parse lexbuf
      | Lexer.LexerError -> unknown_symbol lexbuf)
  | `ASLv0 -> (
      try Gparser0.parse Lexer0.token lexbuf with
      | Parser0.Error -> cannot_parse lexbuf
      | SimpleLexer0.LexerError -> unknown_symbol lexbuf)

let close_after chan f =
  try
    let res = f () in
    close_in chan;
    res
  with e ->
    close_in chan;
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
