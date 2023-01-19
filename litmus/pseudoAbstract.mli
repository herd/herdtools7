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

module type S = sig
  type ins
  type code

  val find_offset : code list -> Proc.t -> string -> int
  val dump_prog : bool -> code -> string list
  val print_prog : out_channel -> code list -> unit
  val dump_prog_lines : code list -> string list
  val code_exists : (ins -> bool) -> code -> bool
  val exported_labels_code :  code list  ->  Label.Full.Set.t
  val from_labels :
      Label.Full.Set.t -> code list  -> (Label.Full.full * ins) list
end
