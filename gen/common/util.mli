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


type spec = string * Arg.spec * string

val parse_tag :
    string ->
      (string -> bool) -> string list -> string ->
          string * Arg.spec * string

val parse_tags :
    string ->
      (string -> bool) -> string list -> string ->
          string * Arg.spec * string

val arch_opt : Archs.t ref -> spec


val parse_cmdline : spec list -> (string -> unit) -> unit

module List : sig
  (** [concat_map f l] gives the same result as [List.concat (List.map f l)].

      For compatibility with OCaml < 4.10 *)
  val concat_map : ('a -> 'b list) -> 'a list -> 'b list

  (** [uniq ~eq l] removes duplicates in [l] w.r.t the equality predicate [eq]. *)
  val uniq : eq:('a -> 'a -> bool) -> 'a list -> 'a list

  (** [sequence [xs; ys; ...]] enumerates all possible ways to select an
      element from each input list.

      Example: [sequence [[1; 2]; [3; 4]] = [[1; 3]; [1; 4]; [2; 3]; [2; 4]] *)
  val sequence : 'a list list -> 'a list list

  (** Like [List.fold_left], but with the possibility to interrupt the folding
      by having the folding function return [None]. *)
  val fold_left_opt : ('acc -> 'a -> 'acc option) -> 'acc -> 'a list -> 'acc option

  module Infix : sig
    val (let*) : 'a list -> ('a -> 'b list) -> 'b list
  end
end

module Option : sig
  (** [choice_fn l] traverses [l] and returns [Some x] if there exists
      an element [f] in [l] such that [f () == Some x] and all other elements
      [g] in [l] before [f] are such that [g () == None]. Otherwise, it returns
      [None]. *)
  val choice_fn : (unit -> 'a option) list -> 'a option

  (** [guard true == Some ()] and [guard false == None] *)
  val guard : bool -> unit option

  module Infix : sig
    val (let*) : 'a option -> ('a -> 'b option) -> 'b option
  end
end
