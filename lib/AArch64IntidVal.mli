(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2024-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Target_Mode : sig
  type t
end

module Trigger_Mode : sig
    type t
end

type t = {
  pending : bool;
  active : bool;
  enabled : bool;
  priority : int;
  target : Proc.t;
  target_mode : Target_Mode.t;
  trigger_mode : Trigger_Mode.t;
  }

val default : t

val eq : t -> t -> bool
val compare : t -> t -> int

val pp : t -> string
val get_prio : t -> int option
val get_target : t -> int option

val tr : ParsedIntidVal.t -> t
val pp_norm : ParsedIntidVal.t -> string
