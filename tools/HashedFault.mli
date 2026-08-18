(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

include (Hashcons.S with type key = Proc.t * HashedStringOpt.t * HashedStringOpt.t * HashedStringOpt.t)

val as_hashed  : Fault_tools.t -> node
val as_t :  node -> Fault_tools.t
val as_hash :  node -> int


(*
 * MMU fault types changed over time as follows:
 *  + Initially, MMU fault types were simply as "MMU:Translation",
 *     "MMU:Permission" etc.
 *  + Later, a "D-" or "I-" prefix has been added to MMU fault types,
 *    in order to indicate whether the fault originates from data
 *    or instruction memory.
 *
 * Moreover, very old litmus logs contain fault occurrences with no
 * fault type at all. Such occurrence simply flags the presence of a fault.
 * The "equivalent" function abstract on those successive syntax.
 * More precisely it lifts the following equivalence on fault types
 * to faults that proceed from the same thread, code location and
 * faulting location
 *  + No type at all is equivalent to some type.
 *  + A given MMU fault type (such as "MMU:Permission" is equivalent to
 *    the same, prefixed fault fault type (such as "D-MMU:Permission")
 * Notice that this "equivalence" is not transitive:
 * + `Fault(P0:L0,x,"D-MMU:Permission)` and  `Fault(P0:L0,x)`
 *    are equivalent.
 * + `Fault(P0:L0,x)` and `Fault(P0:L0,x,"TagCheck")` are equivalent.
 * + `Fault(P0:L0,x,"D-MMU:Permission)` and `Fault(P0:L0,x,"TagCheck")`
 *    are not equivalent.
 * Thus, such an "equivalence" should be used with care.
 *)

val equivalent : node -> node -> bool

(* Standard compare function, distinguishes equivalent faults *)
val compare :  node ->  node -> int

(* Three kinds of fault names *)

type ft_kind =
  | No (* No name *)
  | DIPrefix (* Prefixed with "D-" or "I-" *)
  | Other    (* All other names *)

val compare_kinds : ft_kind -> ft_kind -> int
val get_fault_type :  node -> ft_kind
