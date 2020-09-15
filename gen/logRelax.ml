(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


module type I = sig
  type relax
  val parse : LexUtil.t -> relax
end

module type S = sig
  type relax

  type outcome = {
      name : string ;
      validates : bool ;
      relaxs : relax list ;
      safes : relax list ;
      cycle : string ;
    }

  val add_files : string list -> outcome list
end

module Make(R:I) : S with type relax = R.relax
= struct

  type relax = R.relax

  type outcome = {
      name : string ;
      validates : bool ;
      relaxs : relax list ;
      safes : relax list ;
      cycle : string ;
    }

  let rec parse_relaxs = function
    | [] -> []
    | r::rs ->
        try
          let r = R.parse r in
          r::parse_relaxs rs
        with Misc.Fatal msg  ->
          Warn.warn_always "%s" msg ;
          assert false

  let add_outcome env name validates rs ss cy =
    let rs = parse_relaxs rs
    and ss = parse_relaxs ss in
    {name=name; validates=validates; relaxs=rs; safes=ss; cycle=cy; }::env


  let add_file env name =
    try
      Misc.input_protect
        (fun chan ->
          LexLog_gen.tokens add_outcome env
            (Lexing.from_channel chan))
        name
    with Misc.Fatal msg ->
      Warn.warn_always "%s" msg ;
      env

  let add_files names =
    let all = List.fold_left add_file [] names in
    let all = List.sort (fun o1 o2 -> String.compare o1.name o2.name) all in
    all
end
