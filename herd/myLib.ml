(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Open my files *)

let try_open dir name =
  let rname = Filename.concat dir name in
  try rname,open_in rname
  with _ -> raise Exit

let envlib = try Some (Sys.getenv "HERDLIB") with Not_found -> None

let open_lib name =
  try try_open "." name
  with Exit -> try match envlib with
  | Some lib -> try_open lib name
  | None -> raise Exit
  with Exit -> try try_open Version.libdir name
  with Exit -> Warn.fatal "Cannot find file %s" name
  
let do_find name =
  let r,chan = open_lib name in
  begin try close_in chan with _ -> () end ;
  r

let find path =
  if Filename.is_implicit path then do_find path
  else path
