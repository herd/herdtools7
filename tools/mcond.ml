(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(**********************************)
(* Extract condition from test(s) *)
(**********************************)
open Printf

module type Config = sig
  val check_name : string -> bool
end

module Make(Config:Config) =
  struct
    module D = Splitter.Default
    module LU = LexUtils.Make(D)
    module S = Splitter.Make(D)

    let from_chan chan fname in_chan =
      try
        let { Splitter.locs = locs;  name=name; _} =
          S.split fname in_chan in
        if Config.check_name name.Name.name then begin
          let _,_,(constr_start,constr_end),(_last_start,_loc_eof) = locs in
          let echo sec =
            let lexbuf = LU.from_section sec in_chan in
            Echo.echo_escape lexbuf chan in
          fprintf chan "%s \"" name.Name.name ;
          echo (constr_start,constr_end) ;
          fprintf chan "\"\n" ;
          ()
        end
      with LexMisc.Error (msg,pos) ->
        Printf.eprintf
          "%a: Lex error %s (in %s)\n" Pos.pp_pos pos msg fname ;
        raise Misc.Exit

    let from_file chan name =
      try
        Misc.input_protect
          (fun in_chan -> from_chan chan name in_chan)
          name
      with Misc.Exit -> ()
      | Misc.Fatal msg|Misc.UserError msg ->
          eprintf "Fatal error is not fatal, %s\n" msg

    let from_args args = Misc.iter_argv_or_stdin (from_file stdout) args
  end

(**********)
(* Driver *)
(**********)

let verbose = ref 0

open OptNames

let args = ref []

let opts =
  ("-v", Arg.Unit (fun () -> incr verbose)," be verbose")
  ::parse_noselect

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mcond"

let () =
  Arg.parse opts
    (fun a -> args := a :: !args)
    (sprintf "Usage %s [options] [test]*" prog)

(* Read names *)
module Check =
  CheckName.Make
    (struct
      let verbose = !verbose
      let rename = !rename
      let select = []
      let names = !names
      let oknames = !oknames
      let excl = !excl
      let nonames = !nonames
    end)

module X =
 Make
   (struct
     let check_name = Check.ok
   end)

let () = X.from_args !args
