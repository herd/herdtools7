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
(************************************** UC**************************************)

(* Memory types of global variables *)
module type S = sig
  type t
  val pp : t -> string (* Pretty print *)
  val emit : t -> string (* Emit in code *)

  val parse : MiscParser.info -> t Misc.Simple.bds
end

module X86_64 = struct
  type t = UC | WC | WT | WP | WB

  let pp = function
    | UC -> "UC"
    | WC -> "WC"
    | WT -> "WT"
    | WP -> "WP"
    | WB -> "WB"

  let emit m = Printf.sprintf "PAT_%s" (pp m)

  let parse_t = function
    | "UC" -> UC
    | "WC" -> WC
    | "WT" -> WT
    | "WP" -> WP
    | "WB" -> WB
    | s ->
       Warn.fatal "'%s' is not a X86_64 memory type" s

  let parse infos =
    let open MiscParser in
    let info =
      match get_info_on_info mt_key infos with
      | None -> get_info_on_info memory_type_key infos
      | Some _ as r -> r in
    let ps =
      match info with
      | None -> []
      | Some i ->
         begin
           try LexScan.infos i
         with LexScan.Error ->
           Warn.fatal
             "'%s' is not a proper specificaton of X86_64 memory types"
             i
         end in
    Misc.Simple.map parse_t ps
end

module No = struct

  type t = unit

  let pp () = ""

  let emit = pp

  let parse _ = []

end
