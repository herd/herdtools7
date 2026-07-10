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

(* Event components *)
type loc = Data of string | Code of Label.t
val as_data : loc -> string
val is_data : loc -> bool
val pp_loc : loc -> string
val loc_eq : loc -> loc -> bool
val loc_compare : loc -> loc -> int

module LocSet : MySet.S with type elt = loc
module LocMap : MyMap.S with type key = loc

val loc_none : loc
val ok_str : string
val ok : loc
val myok : int -> int -> loc
val myok_proc : int -> loc

type proc = Proc.t
val pp_proc : proc -> string

(* Direction of event *)
type dir = W | R

(* Edges compoments that do not depend on architecture *)

(* Change or proc accross edge *)
type ie = Int|Ext

(* Change of location across edge *)
type sd = Same|Diff

(* Direction of related events *)
type extr = Dir of dir | Irr | NoDir

val equal_ie : ie -> ie -> bool
val equal_sd : sd -> sd -> bool
val equal_extr : extr -> extr -> bool

(* Associated pretty print & generators *)
val pp_ie : ie -> string
val pp_dir : dir -> string
val pp_extr : extr -> string
val pp_sd : sd -> string
val pp_dir_macro : dir option -> string
val pp_sd_macro : sd option -> string
val seq_sd : sd -> sd -> sd
val is_same_loc : sd -> bool
val is_diff_loc : sd -> bool
val fold_ie : (ie -> 'a -> 'a) -> 'a -> 'a
val fold_sd : (sd -> 'a -> 'a) -> 'a -> 'a
val fold_sd_extr_extr : (sd -> extr -> extr -> 'a -> 'a) -> 'a -> 'a
val expand_sd_macro : sd option -> (sd -> 'a -> 'a) -> 'a -> 'a
val expand_dir_macro : dir option -> (dir -> 'a -> 'a) -> 'a -> 'a
val fold_sd_extr_macros :
  (sd option -> dir option -> (sd * dir) list -> 'a -> 'a) -> 'a -> 'a
val fold_sd_extr_extr_macros :
  (sd option -> dir option -> dir option -> (sd * dir * dir) list -> 'a -> 'a) -> 'a -> 'a

type check =
  | Default | Sc | Uni | Thin | Critical | Free
  | Ppo | Transitive | Total | MixedCheck

val pp_check : check -> string
val checks : string list

(* Com *)
type com =  Rf | Fr | Co

val equal_com : com -> com -> bool
val pp_com : com -> string
val fold_com : (com -> 'a -> 'a) -> 'a -> 'a

(* Info in tests *)
type info = (string * string) list

(* Name of plain accesses *)
val plain : string

(* Memory bank (for MTE, KVM)  *)
type 'a bank = Ord | Tag | CapaTag | CapaSeal | Pte | VecReg of 'a | Pair | Instr

val pp_bank : 'a bank -> string

(* TODO consider change the type `v` *)
val add_tag : string -> int -> string

(* TODO consider change the type `v` *)
val add_capability : string -> int -> string

(* TODO consider change the type `v` *)
val add_vector : bool -> int list -> string
