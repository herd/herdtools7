(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* memoize *)

let t = Hashtbl.create 17

module type Config = sig
  include LexUtils.Config
  val libfind : string -> string
end

module Make(O:Config) = struct
  open Lexing

(* Replace Test positions by text *)
  open AST

  let get_text pp = function
    | Pos pos ->
        Txt (try String.sub pp pos.pos pos.len with _ -> "????")
    | Txt _ as p -> p

  let set_text pp =
    let f =  get_text pp in
    let rec map_ins ins = match ins with
    | Test ((loc, pos, test, exp, name),ty) ->
        Test  ((loc, f pos, test, exp, name),ty)
    | Procedure (loc,v,p,code) ->
        Procedure (loc,v,p,map_code code)
    | Forall (loc,v,e,code) ->
        Forall  (loc,v,e,map_code code)
    | _ -> ins

    and map_code code = List.map map_ins code in

    map_code

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
    let opts,txt,code = model in
    (opts,txt,set_text pp code)

  let parse fname =
    try Hashtbl.find t fname
    with Not_found ->
      let key = fname in
      let fname = O.libfind fname in
      let r = Misc.input_protect (do_parse fname) fname in
      Hashtbl.add t key r ;
      r
end
