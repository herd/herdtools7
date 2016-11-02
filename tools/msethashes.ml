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

(* Set test hashes *)

open Printf

module type Opt = sig
  val verbose : int
  val get_hash : string -> string
  val hexa : bool
end

module Make(Opt:Opt)(Out:OutTests.S)(A:ArchBase.S)
    =
  struct

    module D = DumperMiscParser.Make(Opt)(Out)(A)

    let zyva name parsed =
      let tname =  name.Name.name in
      let old_h =
        match MiscParser.get_hash parsed with
        | Some h -> h
        | None -> assert false in
      let parsed =
        try 
          let new_h = Opt.get_hash tname in
          if old_h <> new_h then begin
            if Opt.verbose > 0 then
              eprintf "Changing hash for test %s: %s\n" tname new_h ;
            MiscParser.set_hash parsed new_h
          end else parsed
        with Not_found -> parsed in
      let base = Filename.basename name.Name.file in
      let out = Out.open_file base in
      Misc.output_protect_close
        Out.close
        (fun chan -> D.dump_info chan name parsed)
        out ;      
      base
  end


let verbose = ref 0
let hexa = ref false
let tar = ref None
let arg = ref []
let hashes = ref []
let set_hashes c = hashes := !hashes @ [c]
let set_tar x = tar := Some x

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "msethash"

let opts = 
  [ "-v",Arg.Unit (fun () -> incr verbose)," be verbose";
    "-hexa",Arg.Bool (fun b -> hexa := b),
    sprintf "<bool> hexadecimal output, default %b" !hexa;
    "-hashes",Arg.String set_hashes, "<name> specify hashes";
    "-o", Arg.String set_tar,
    "<name> output to directory or tar file <name>" ;]

let () =
  Arg.parse
    opts
    (fun s -> arg := !arg @ [s])
    (sprintf "Usage: %s [options]* [test]*" prog)

let tests = List.rev !arg

module LR = LexRename.Make(struct let verbose = !verbose end)
let hashes = LR.read_from_files !hashes (fun s ->Some s)

module Top(Opt:Opt)(Out:OutTests.S)
    =
  struct
    module X = Make(Opt)(Out)
    module Z =  ToolParse.Top(String)(X)

    let zyva args =
      let idx_chan = Out.open_all () in
      Misc.iter_argv
        (fun fname ->
          let r = Z.from_file fname in
          Out.fprintf idx_chan "%s\n" r)
        args ;
      Out.close idx_chan ;
      Out.tar ()
  end

module X =
  Top
    (struct
      let verbose = !verbose
      let get_hash = TblRename.find_value hashes
      let hexa = !hexa
    end)

let zyva = match !tar with
| None ->
    let module Z = X(OutStd) in
    Z.zyva
| Some _ as t ->
    let module Out =
      OutTar.Make
        (struct
	  let verbose = !verbose
	  let outname = t
	end) in
    let module Z = X(Out) in
    Z.zyva


let () = zyva !arg


  
