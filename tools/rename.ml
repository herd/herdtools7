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

(* Change the internam names of tests. *)

open Printf
open OptNames

(* Rename tests *)

type mode =
  | File (* Default mode, internal name from file name *)
  | Num of int (* Numerical suffix, int is padding *)
  | Suffixes of string array (* Array of suffixes *)

module type Config = sig
  val verbose : int
  val check_name : string -> bool
  val get_rename : string -> string
  val mode : mode
end

module Make(Opt:Config) =
  struct
    module D = struct let debug = Opt.verbose > 1  end
    module S = Splitter.Make(D)

    let name_from_file file =
      Filename.basename file |> Filename.remove_extension

    let rename_file  st { Name.name; file; _ } lexbuf =
      let name2 = name_from_file file in
      if String.equal name name2 then None,st
      else if not (S.testname_valid name2) then
        Warn.user_error
          "The filename %s is not a valid test name" name2
      else Some (S.rename name2 lexbuf),st

    let rename_num pad st { Name.name; _ } lexbuf =
      let idx = StringMap.safe_find 0 name st in
      (match idx with
      | 0 -> None
      | _ ->
          let name =  Printf.sprintf "%s%0*i" name pad idx in
          Some (S.rename name lexbuf)),
      StringMap.add name (idx+1) st

    let rename_explicit t st { Name.name; _ } lexbuf =
      let idx = StringMap.safe_find 0 name st in
      (match idx with
      | 0 -> None
      | _ ->
          if idx >= Array.length t+1 then
            Warn.user_error
               "Suffix list exhausted by name %s" name ;
          let name =
            Printf.sprintf "%s%s" name t.(idx-1) in
          Some (S.rename name lexbuf)),
      StringMap.add name (idx+1) st

    let rename =
      match Opt.mode with
      | File -> rename_file
      | Num pad -> rename_num pad
      | Suffixes t -> rename_explicit t


    module Copy  =
      ChangeFst.Make
        (struct
          type state = int StringMap.t
          let verbose = Opt.verbose
          let split = S.split

          let rewrite_fst st name lexbuf =
            try
              let { Name.name; _ } = name in
              let name = Opt.get_rename name in
              Some (S.rename name lexbuf),st
            with Not_found ->
              rename st name lexbuf

        end)

    module OptGen = struct
      type state = int StringMap.t
      include Opt
    end

    module Gen = GenTests.Make(OptGen)(Copy)

    let from_tokens = Gen.from_tokens

  end

(* Interface *)

let verbose = ref 0
let tar = ref None
let arg = ref []
let renames = ref []
let mode = ref File

let set_list r c = r := !r @ [c]
let set_mode m =
  match !mode,m with
  | (File,_)|(Num _,Num _) ->
      mode := m
  | Suffixes t1,Suffixes t2 ->
      mode := Suffixes (Array.append t1 t2)
  | (Num _,Suffixes _)
  | (Suffixes _,Num _) ->
      raise
        (Arg.Bad "options -suffixes and -addnum are incompatible")
  | _,File -> assert false

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "rename"

let opts =
  LibOpts.parse_verbose verbose
  @(LibOpts.parse_dest tar::parse_noselect)
  @
    ["-rename",Arg.String (set_list renames),
     "<name> specify rename mapping" ;
     "-addnum",Arg.Int (fun n -> set_mode (Num n)),
     "<n> numeric mode, <n> is the size of integer added to name in case of duplicates";
     ArgUtils.parse_stringlistfun
       "-suffixes"
       (fun xs -> set_mode (Suffixes (Array.of_list xs)))
       "explicit suffix mode, argument is the list of suffixes"; ]

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
let names = LR.read_from_files !renames (fun s -> Some s)

module Zyva =
  Make
    (struct
      let verbose = !verbose
      let check_name = Check.ok
      let get_rename name = TblRename.find_value names name
      let mode = !mode
    end)

let () = Zyva.from_tokens !arg StringMap.empty !tar
