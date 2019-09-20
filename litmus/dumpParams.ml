(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type Config = sig
  val size : int
  val runs : int
  val avail : int option
  val stride : Stride.t
  val timeloop : int
  val mode : Mode.t
end

module Make (O:Config) =
  struct
    open Printf

    let dump out =
      let dump_def var x = out (sprintf "#define %s %s" var x) in
      dump_def "SIZE_OF_TEST" (sprintf "%i" O.size) ;
      dump_def "NUMBER_OF_RUN" (sprintf "%i" O.runs) ;
      dump_def "AVAIL"
        (match O.avail with
        | None -> "1" | Some n -> sprintf "%i" n) ;
      begin match O.mode with
      | Mode.Std ->
          begin
            let open Stride in
            match O.stride with
            | No -> dump_def "STRIDE" "(-1)"
            | Adapt ->  dump_def "STRIDE" "N"
            | St i ->  dump_def "STRIDE" (sprintf "%i" i)
          end ;
          dump_def "MAX_LOOP"
            (let x = O.timeloop in if x > 0 then sprintf "%i" x else "0")
      | Mode.PreSi|Mode.Kvm -> ()
      end ;
      ()
  end
