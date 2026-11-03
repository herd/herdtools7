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
module Misc = Base.Misc


(* Set test hashes *)

open Printf

module type Config = sig
  val verbose : int
  val get_hash : string -> string
  val check_name : string -> bool
end

module Make(Opt:Config) =
  struct
    module D = struct let debug = Opt.verbose > 1  end
    module S = Splitter.Make(D)

    module Copy =
      ChangeFst.Make
        (struct
          type state =  unit
          let verbose = Opt.verbose
          let split = S.split
          let rewrite_fst (st:unit) name lexbuf =
            let { Name.name; _ } = name in
            begin
              try
                let hash = Opt.get_hash name in
                Some (S.rehash hash lexbuf)
              with Not_found -> None
            end,st
        end)

    module OptGen = struct
      type state = unit
      include Opt
    end

    module Gen = GenTests.Make(OptGen)(Copy)

    let from_tokens = Gen.from_tokens

  end

open OptNames

let verbose = ref 0
let tar = ref None
let arg = ref []
let hashes = ref []


let set_list r c = r := !r @ [c]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "rehash"

let opts =
  LibOpts.parse_verbose verbose
  @(LibOpts.parse_dest tar::parse_noselect)
  @["-hashes",Arg.String (set_list hashes), "<name> specify hashes" ; ]

let () =
  Arg.parse
    opts
    (fun tok -> arg := tok :: !arg)
    (sprintf "Usage: %s [options]* [test]*" prog)

module Check =
  CheckName.Make
    (struct
      let verbose = !verbose
      let rename = []
      let select = []
      let names = !names
      let oknames = !oknames
      let excl = !excl
      let nonames = !nonames
    end)

module LR = LexRename.Make(struct let verbose = !verbose end)
let hashes = LR.read_from_files !hashes (fun s -> Some s)

(* Call *)

module Zyva =
  Make
    (struct
      let verbose = !verbose
      let check_name = Check.ok
      let get_hash name = TblRename.find_value hashes name
    end)

let () = Zyva.from_tokens !arg () !tar
