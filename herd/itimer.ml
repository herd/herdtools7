(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

let start timeout =
  match timeout with
  | None -> ()
  | Some t ->
      let open Unix in
      let it =
        {it_value=t; it_interval=0.0;} in
      ignore (setitimer ITIMER_VIRTUAL it)

and stop timeout =
  match timeout with
  | None -> ()
  | Some _ ->
      let open Unix in
      let it =
        {it_value=0.0; it_interval=0.0;} in
      ignore (setitimer ITIMER_VIRTUAL it)

