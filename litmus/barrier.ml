(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


(* Barrier option *)

type t = User | Pthread | NoBarrier | User2 | TimeBase | UserFence | UserFence2

let tags = ["user";"userfence";"user2";"userfence2";"pthread";"none";"timebase";]

let parse tag = match String.lowercase tag with
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
