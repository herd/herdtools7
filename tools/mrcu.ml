(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2016-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


(* RCU translation *) 

open Printf

let prog = if Array.length Sys.argv > 0 then Sys.argv.(0) else "mrcu"

module Top
    (O:
       sig
         val verbose : int
         val outputdir : string option
       end) =
  struct

    module LISA = BellBase

    module LISALexParse = struct
      type instruction = LISA.parsedPseudo
      type token = LISAParser.token

      module L = BellLexer.Make(Splitter.Default)
      let lexer = L.token
      let parser = LISAParser.main
    end

    module P = GenParser.Make(GenParser.DefaultConfig)(LISA)(LISALexParse)
    module D = DumperMiscParser.Make(LISA)

    module F = struct
      open LISA

      let map_call = function
        | Pcall _ -> Pfence (Fence (["mb"],None))
        | i -> i
              
      let map_pseudo_call k = pseudo_map map_call k

      let map_code = List.map map_pseudo_call

      let zyva name t =
        let open MiscParser in
        let nprog = List.map (fun (p,c) -> p,map_code c) t.prog in
        D.dump_info stdout name { t with prog = nprog;}
    end

open Archs

let from_chan chan splitted =  match splitted.Splitter.arch with
| LISA ->
    let name = splitted.Splitter.name in
    let parsed = P.parse chan splitted in
    F.zyva name parsed
| arch -> Warn.user_error "Bad arch for %s: %s" prog (Archs.pp arch)



module SP = Splitter.Make(Splitter.Default)

let from_file fname =
  Misc.input_protect
    (fun chan ->
      let (splitted:Splitter.result) = SP.split fname chan in
      from_chan chan splitted) fname
end

let verbose = ref 0
let outputdir = ref None
let args = ref []

let opts =
  [
   "-v",Arg.Unit (fun () -> incr verbose), " be verbose";
    "-o", Arg.String (fun s -> outputdir := Some s),
   "<name>  all output in directory <name>";
  ]

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


let () =
  Misc.iter_argv
    (fun fname ->
      try X.from_file fname with
      | Misc.Exit -> ()
      | Misc.Fatal msg ->
          Warn.warn_always "%a %s" Pos.pp_pos0 fname msg ;
          ()
      | e ->
          Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 fname ;
          raise e)
    !args
