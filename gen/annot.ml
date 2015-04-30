(*********************************************************************)
(*                        DIY                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Annotation  specification parser *)

module Make(O:LexUtils.Config) =
  struct
    module Lexer = ModelLexer.Make(O)
    open ModelParser

    let error () = raise Parsing.Parse_error

    let do_parse lex lexbuf =

      let rec annot_list_rec = function
        | TAG t|VAR t ->
            begin match lex lexbuf with
            | COMMA -> t::annot_list_rec (lex lexbuf)
            | RACC -> [t]
            | _ -> error ()
            end
        | _-> error () in

      let annot_list = function
        | RACC -> []
        | tok -> annot_list_rec tok in

      let rec annot_list_list_rec = function
        | LACC ->
            let ts = StringSet.of_list (annot_list (lex lexbuf)) in
            begin match lex lexbuf with
            | COMMA -> ts::annot_list_list_rec (lex lexbuf)
            | RBRAC -> [ts]
            | _ -> error ()
            end
        | _ -> error () in

(* Forbid empty annotation specification *)
      let annot_list_list tok = annot_list_list_rec tok in

      let rec event_dec = function
        | VAR n ->
            if StringSet.mem n BellName.all_mem_sets then
              match lex lexbuf with
              | LBRAC ->
                  let ts = annot_list_list (lex lexbuf) in
                  (n,ts)::event_dec (lex lexbuf)
              | _ -> error ()
            else error ()
        | EOF -> []
        | _ -> error () in

      event_dec (lex lexbuf)

    let parse_one s m =
      let to_add =
        GenParser.call_parser
          "_none_" (Lexing.from_string s) Lexer.token do_parse in
      List.fold_right
        (fun (n,al) -> BellModel.add_event_dec n al)
        to_add m

    let parse lines =
      List.fold_right parse_one lines BellModel.event_decs_empty
  end
