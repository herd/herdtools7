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

open Unix

let dbg = false

let name = ref ""

let set_signal timeout f dbg =
  match timeout with
  | None -> ()
  | Some _ ->
     let g =
       if dbg then
         fun s ->
           let n = !name in
           if n <> "" then Printf.eprintf "Timeout for %s\n%!" n ;
           f s
       else f in
     Sys.set_signal
       26 (* SIGVTALARM *)
       (Sys.Signal_handle g)

let start n timeout =
  match timeout with
  | None -> ()
  | Some t ->
     name := n ;
     if dbg then Printf.eprintf "Start %s\n%!" n ;
     let it =
       {it_value=t; it_interval=0.0;} in
     ignore (setitimer ITIMER_VIRTUAL it)

and stop timeout =
  match timeout with
  | None -> ()
  | Some _ ->
     if dbg then Printf.eprintf "Stop %s\n%!" !name ;
     name := "" ;
     let it =
       {it_value=0.0; it_interval=0.0;} in
     ignore (setitimer ITIMER_VIRTUAL it)
