(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Debug tags *)

type debug_flag = Lexer | Top | Generator | Model | Files | Cycle

module DebugSet = MySet.Make(struct type t = debug_flag let compare = compare end)

type t = {
  flag: DebugSet.t ;
  verbose: int ;
}

let tags =
[
  "lexer";
  "top";
  "generator";"gen";
  "model"; "files";
  "cycle";
]

let none = {
  flag = DebugSet.empty ;
  verbose = 0 ;
}

let parse t tag =
  let tag = match tag with
  | "lexer" -> Some Lexer
  | "top" -> Some Top
  | "generator"|"gen" -> Some Generator
  | "model" -> Some Model
  | "files"|"file" -> Some Files
  | "cycle" -> Some Cycle
  | _ -> None in
  Option.map ( fun tag -> { t with flag = DebugSet.add tag t.flag } ) tag

let contain_flag flag t = DebugSet.mem flag t.flag

module type S = sig
  val debug : debug_flag -> ('a, out_channel, unit) format -> 'a
  val verbose : int -> ('a, out_channel, unit) format -> 'a
  val contain_flag : debug_flag -> bool
  val verbose_level : int
end

module Make(I: sig val debug: t end) : S = struct
  (* Logging function that takes a verbosity level and a formatted message *)
  let pp flag channel fmt =
    if flag then
      Printf.kfprintf (fun oc -> flush oc) channel fmt
    (* ignores the output *)
    else Printf.ifprintf channel fmt

  let debug flag fmt = pp (DebugSet.mem flag I.debug.flag) stderr fmt
  let verbose level fmt = pp (level<=I.debug.verbose) stderr fmt
  let contain_flag flag  = DebugSet.mem flag I.debug.flag
  let verbose_level = I.debug.verbose
end
