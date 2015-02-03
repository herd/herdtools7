(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


module Make(O:LexUtils.Config) = struct
  open Lexing

(* A lexbuf that keeps scanned input *)
  let mk_lexbuf chan =
    let buff = Buffer.create 32 in
    buff,
    from_function
      (fun s n ->
        let r = input chan s 0 n in
        Buffer.add_string buff (Bytes.sub_string s 0 r) ;
        r)

  module ML = ModelLexer.Make(O)

  let do_parse fname chan =
    let buff,lexbuf = mk_lexbuf chan in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname=fname;};
    let model = GenParser.call_parser "model" lexbuf ML.token
        ModelParser.main in
    let pp = Buffer.contents buff in
    pp,model

  let parse fname =
    let fname = MyLib.find fname in
    Misc.input_protect (do_parse fname) fname
end
