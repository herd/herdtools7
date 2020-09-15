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

(** Facility for grouping output files in directory or archive *)

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
