(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Abstract system register values *)
module type S = sig
  type t

  val default : int -> t

  val pp : bool -> t -> string
  val pp_v : t -> string
  val tr : ParsedAddrReg.t -> t
  val pp_norm : ParsedAddrReg.t -> string

  val eq : t -> t -> bool
  val compare : t -> t -> int

  val same_oa : t -> t -> bool

end

module No = struct
  type t = unit

  let default _ = ()

  let pp _ _ = "()"
  let pp_v _ = "()"
  let tr _ = ()
  let pp_norm _ = "()"

  let eq _ _ = true
  let compare _ _ = 0

  let same_oa _ _ = false
end

module ASL = No