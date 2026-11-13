(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(**
 * Abstract structure to group items by thread.
 * Such a structure facilitates keeping items in order,
 * if one transform the structure by map only.
 *  Furthermore,items are garanteed to be processed
 * by increasing thread identifier.
 *)

type 'a t

val empty : 'a t
val is_empty : 'a t -> bool

val iter : (Proc.t -> 'a -> unit) -> 'a t -> unit
val iteri : (int -> Proc.t -> 'a -> unit) -> 'a t -> unit
val map : (Proc.t -> 'a -> 'b) -> 'a t -> 'b t
val fold : ('b -> Proc.t -> 'a -> 'b) -> 'b -> 'a t -> 'b

val of_list : Label.Full.t list -> Label.t t
val of_set : Label.Full.Set.t -> Label.t t
val to_set :  Label.t t -> Label.Full.Set.t

val to_procs : 'a t -> Proc.t list
val of_proc : Proc.t -> 'a t -> 'a list
