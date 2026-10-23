(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Apply change in test init section *)


module type Config = sig
  type state
  val verbose : int
  val split : string -> in_channel -> Splitter.result
  val rewrite_fst : state -> Name.t -> Lexing.lexbuf -> string option * state
end

module Make(Opt:Config)(Out:OutTests.S) = struct

  module LU =
    LexUtils.Make
      (struct
        let debug = Opt.verbose > 1
      end)

  let split = Opt.split

  type state = Opt.state
  type out = Out.t

  let from_splitted st idx_out splitted in_chan =
    let { Splitter.locs; start; name;_} = splitted in
    let { Name.file; _ } = name in
    try
      let base = Filename.basename file in
      let out = Out.open_file base in
      let st =
        Misc.output_protect_close Out.close
          (fun out ->
             let (init_start,_),_,_,(_,loc_eof) = locs in
             let lexbuf = LU.from_section (start,init_start) in_chan in
             let r,st = Opt.rewrite_fst st name lexbuf in
             begin
               match r with
               | Some new_fst ->
                   Out.fprintf out "%s" new_fst ;
                   let lexbuf = LU.from_section (init_start,loc_eof) in_chan in
                   Echo.echo_fun lexbuf (Out.put_char out)
               | None ->
                   let lexbuf = LU.from_section (start,loc_eof) in_chan in
                   Echo.echo_fun lexbuf (Out.put_char out)
             end ;
             st)
          out in
      Out.fprintf idx_out "%s\n" base ;
      st

    with LexMisc.Error (msg,pos) ->
      Printf.eprintf
        "%a: Lex error %s (in %s)\n" Pos.pp_pos pos msg file ;
      raise Misc.Exit
end
