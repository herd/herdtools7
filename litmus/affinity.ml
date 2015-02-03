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

open Printf

type t =
  | No
  | Incr of int
  | Random
  | Custom
  | Scan


let incr_pref = "incr"
let pref_len = String.length incr_pref
let tags = ["none"; "random"; "custom" ; sprintf "%s<n>" incr_pref; "scan";]


let parse tag = match String.lowercase tag with
| "none" -> Some No
| "random"|"rand" -> Some Random
| "custom" -> Some Custom
| "scan" -> Some Scan
| _ ->
      try
        let len = String.length tag in
        if len > pref_len then
          let pref =  String.sub tag 0 pref_len in
          if pref <> incr_pref then raise (Failure incr_pref) ;
          let suff = String.sub tag pref_len (len-pref_len) in
          Some (Incr (int_of_string suff))
        else          
          None
      with Failure _ -> None

  let pp = function
    | No -> "none"
    | Incr i -> sprintf "incr%i" i
    | Random -> "random"
    | Custom -> "custom"
    | Scan -> "scan"
        
