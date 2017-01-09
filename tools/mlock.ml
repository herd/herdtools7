(****************************************************************************)
(*                          the diy7 toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(***********************************************)
(* Protect memory accesses with locks, linux-C *)
(***********************************************)

open Printf
open Archs

module type Config = sig
  val verbose : int
  val outputdir : string option
end

module Top(O:Config)(Out:OutTests.S) = struct

  module D = CDumper.Make(Out)

  let tr idx_out name parsed =
    let fname = name.Name.file in
    let base = Filename.basename fname in
    let out = Out.open_file base in
     Misc.output_protect_close Out.close
      (fun out ->
        let name = { name with Name.name = name.Name.name ^ "+locked" ; } in
        D.dump out name parsed ;
        Out.fprintf idx_out "%s\n" base)
      out ;
    ()
    
  module LexConf  = struct
    let debug = O.verbose > 2
    let check_rename _ = None
  end

  let from_chan idx_out chan splitted =
    match splitted.Splitter.arch with
    | `C ->
        let module C = CBase in
        let module L = struct
          type pseudo = C.pseudo
          type token = CParser.token
          module Lexer = CLexer.Make(LexConf)
          let shallow_lexer = Lexer.token false
          let deep_lexer = Lexer.token true
          let shallow_parser = CParser.shallow_main
          let deep_parser = CParser.deep_main
(* No macros *)
          type macro = unit
          let macros_parser _ _ = assert false
          let macros_expand _ i = i
        end in
        let module P =
          CGenParser_lib.Make(CGenParser_lib.DefaultConfig)(C)(L) in
        let name =  splitted.Splitter.name in
        let parsed = P.parse chan splitted in
        tr idx_out name parsed 
    | _ -> Warn.fatal "not a C litmus test"        

  module SP = Splitter.Make(LexConf)

  let from_arg idx_out name =
    Misc.input_protect
      (fun chan ->
        let (splitted:Splitter.result) = SP.split name chan in
        from_chan idx_out chan splitted)
      name

  let zyva args =
    let idx_out = Out.open_all () in
    Misc.output_protect_close Out.close
      (fun idx_out ->
        Misc.iter_argv
          (fun fname ->
            try from_arg idx_out fname with
            | Misc.Exit -> ()
            | Misc.Fatal msg ->
                Warn.warn_always "%a %s" Pos.pp_pos0 fname msg ;
                ()
            | e ->
                Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 fname ;
                raise e)
          args) idx_out ;
    Out.tar ()

end

let verbose = ref 0
let args = ref []
let outputdir = ref None

let opts =
  [
   "-v",Arg.Unit (fun () -> incr verbose), " be verbose";
   "-o", Arg.String (fun s -> outputdir := Some s),
   "<name>  all output in directory <name>";
 ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mprog"

let () =
  Arg.parse opts
    (fun s -> args := !args @ [s])
    (sprintf "Usage: %s [options]* [test]*" prog)

module X =
  Top
    (struct
      let verbose = !verbose
      let outputdir = !outputdir
    end)

let zyva = match !outputdir with
| None ->
    let module Y = X(OutStd) in
    Y.zyva
| Some _ as d ->
    let module T =
      OutTar.Make
        (struct
	  let verbose = !verbose
	  let outname = d
	end) in
    let module Y = X(T) in
    Y.zyva
      
let () = zyva !args
  
