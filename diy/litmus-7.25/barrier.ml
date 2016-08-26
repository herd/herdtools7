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


(* Barrier option *)

type t = User | Pthread | NoBarrier | User2 | TimeBase | UserFence | UserFence2

let tags = ["user";"userfence";"user2";"userfence2";"pthread";"none";"timebase";]

let parse tag = match Misc.lowercase tag with
| "user" -> Some User
| "userfence" -> Some UserFence
| "user2" -> Some User2
| "userfence2" -> Some UserFence2
| "pthread" -> Some Pthread
| "none" -> Some NoBarrier
| "timebase"|"tb" -> Some TimeBase
| _ -> None

let pp = function
| User -> "user"
| User2 -> "user2"
| UserFence -> "userfence"
| UserFence2 -> "userfence2"
| Pthread -> "pthread"
| NoBarrier -> "none"
| TimeBase  -> "timebase"
