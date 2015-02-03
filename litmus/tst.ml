open Lexing
open Printf

let zyva fname =
  Misc.input_protect
    (fun chan ->
      let lexbuf = Lexing.from_channel chan in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname=fname;};
      let pgm =  GenParser.call_parser "progC" lexbuf CLexer.main CParser.main in
      List.iter
        (fun f ->
          printf "Code de %i: '%s'\n" f.CAst.proc f.CAst.body)
        pgm ;
      exit 0)
    fname


let () =
  try zyva Sys.argv.(1) 
  with Misc.Exit -> exit 2
