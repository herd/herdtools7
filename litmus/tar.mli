(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(**************************************************************)
(* Facility for gouping outpout files in directory or archive *)
(**************************************************************)

(* Input *)
module type Option = sig
  val verbose : int
(* Output name, *.tar -> archive, *.tgz -> compressed archive, * -> directory *)
  val outname : string  option
end

module type S = sig

(* Gives actual output name *)
  val outname : string -> string

(* Build archive or not *)
  val is_archive : bool

(* Returns z if O.outname is *.tgz or empty string otherwise *)
  val tarz : unit -> string

(* Produce final tar archive (and remove temporary directory) *)
  val tar : unit -> unit

(* Explicit directory to archive is given.
   Notice that 'dir' is 'path/base' where 'base' is the directory
   to include in archive. *)
  val tar_dir : (*dir*) string -> unit
end

module Make(O:Option) : S

