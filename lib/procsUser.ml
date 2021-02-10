(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Threads in user mode *)

let get info =
  let get k = MiscParser.get_info_on_info k info in
  let procs = match get MiscParser.el0_key with
      | None -> get MiscParser.user_key
      | Some _ as r -> r in
    match procs with
    | None -> []
    | Some p ->
        begin
          try LexScan.procs p
          with
          | LexScan.Error ->
              Warn.user_error "'%s' is not a list of thread names" p
        end
