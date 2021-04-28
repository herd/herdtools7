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
  val default : t
  val pp : t -> string (* Pretty print *)
  val emit : t -> string (* Emit in code *)
  val fold : (t -> 'a -> 'a) -> 'a -> 'a
  val equal : t -> t -> bool

  val parse : MiscParser.info -> t Misc.Simple.bds
  (* Cache flush neeed after initialisation *)
  val need_flush : t Misc.Simple.bds -> bool

end

module X86_64 = struct
  type t = UC | WC | WT | WP | WB

  let default = WB

  let pp = function
    | UC -> "UC"
    | WC -> "WC"
    | WT -> "WT"
    | WP -> "WP"
    | WB -> "WB"

  let emit m = Printf.sprintf "PAT_%s" (pp m)

  let fold f k =
    let k = f UC k in
    let k = f WC k in
    let k = f WT k in
    let k = f WB k in
    k

  let equal (m1:t) (m2:t) = m1=m2

  let _ = WP (* Silence warning 37 *)

  let parse_t = function
    | "UC" -> UC
    | "WC" -> WC
    | "WT" -> WT
    | "WB" -> WB
    | "WP" as s ->
       Warn.fatal "%s memory type  is not implemented" s
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
    List.fold_right
      (fun (key,tok) k ->
        match parse_t tok with
        | WB -> k
        | m -> (key,m)::k)
      ps []

  let need_flush =
    List.exists (fun (_,m) -> match m with WC -> true | _ -> false)
end

module No = struct

  type t = unit

  let default = ()

  let pp () = "OneMemtype"

  let emit = pp

  let fold _f k = k

  let equal () () = true

  let parse _ = []

  let need_flush _ = false

end
