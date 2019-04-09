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
  val check_name : string -> bool
end

module Make(Opt:Opt)(Out:OutTests.S) =
  struct
    module D = Splitter.Default
    module LU = LexUtils.Make(D)
    module S = Splitter.Make(D)

    let from_chan idx_out fname in_chan =
      try
        let { Splitter.locs = locs; start = start; name=name; _} =
          S.split fname in_chan in
        if Opt.check_name name.Name.name then begin
          let base = Filename.basename fname in
          let out = Out.open_file base in
          Misc.output_protect_close Out.close       
            (fun out ->
              let echo sec =
                let lexbuf = LU.from_section sec in_chan in
                Echo.echo_fun lexbuf (Out.put_char out) in
              let (init_start,_),_,_,(_,loc_eof) = locs in
              try
                let new_v = Opt.get_hash name.Name.name in (* raise Not_found *)
                let lexbuf = LU.from_section (start,init_start) in_chan in
                let new_init = S.rehash new_v lexbuf in
                Out.fprintf out "%s" new_init ;
                echo (init_start,loc_eof)
              with Not_found ->
                echo (start,loc_eof)) out ;
          Out.fprintf idx_out "%s\n" base
        end
      with LexMisc.Error (msg,pos) ->
        Printf.eprintf
	  "%a: Lex error %s (in %s)\n" Pos.pp_pos pos msg fname ;
        raise Misc.Exit  
 let from_file idx_chan name =
      try
        Misc.input_protect
          (fun in_chan -> from_chan idx_chan name in_chan)
          name
      with Misc.Exit -> ()
      | Misc.Fatal msg ->
          eprintf "Fatal error is not fatal, %s\n" msg

    let from_args args =
      let idx_out = Out.open_all () in
      Misc.output_protect_close Out.close
        (fun idx_out ->
          Misc.iter_argv_or_stdin (from_file idx_out) args)
        idx_out ;
      Out.tar ()

  end


let verbose = ref 0
let tar = ref None
let arg = ref []
let hashes = ref []
let names = ref []

let set_list r c = r := !r @ [c]
let set_tar x = tar := Some x


let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mrehash"

let opts = 
  [ "-v",Arg.Unit (fun () -> incr verbose)," be verbose";
    "-names",
    Arg.String (set_list names),
    "<name> select tests whose names are listed in file <name>";
    "-hashes",Arg.String (set_list hashes), "<name> specify hashes";
    "-o", Arg.String set_tar,
    "<name> output to directory or tar file <name>" ;]

let () =
  Arg.parse
    opts
    (fun s -> arg := !arg @ [s])
    (sprintf "Usage: %s [options]* [test]*" prog)

let tests = List.rev !arg

let names = match !names with
| [] -> None
| names -> Some (ReadNames.from_files names StringSet.add StringSet.empty)

module LR = LexRename.Make(struct let verbose = !verbose end)
let hashes = LR.read_from_files !hashes (fun s -> Some s)

(* Call *)
let from_args = 
  let module X =
    Make
      (struct
        let verbose = !verbose
        let check_name = match names with
        | None -> fun _ -> true
        | Some names -> (fun name -> StringSet.mem name names)

        let get_hash name = TblRename.find_value hashes name

      end) in
  match !tar with
  | None ->
      let module Y = X(OutStd) in
      Y.from_args
  | Some _ as t ->
      let module T =
        OutTar.Make
          (struct
	    let verbose = !verbose
	    let outname = t
	  end) in
       let module Y = X(T) in
       Y.from_args

let () = from_args tests
