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

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module List : sig
  (* For compatibility with ocaml <= 4.10 *)
  val concat_map : ('a -> 'b list) -> 'a list -> 'b list
  val uniq : eq:('a -> 'a -> bool) -> 'a list -> 'a list
  val sequence : 'a list list -> 'a list list
  val drop_while : ('a -> bool) -> 'a list -> 'a list

  include Monad with type 'a t := 'a list

  module Traversal : (M : Monad) ->
    sig
      val fold_left : ('acc -> 'a -> 'acc M.t) -> 'acc -> 'a list -> 'acc M.t
    end
end

module Option : sig
  (* Returns the first non-None element in the given list,
     or None if all elements are None. *)
  val choice : 'a option list -> 'a option
  val choice_fn : (unit -> 'a option) list -> 'a option
  val guard : bool -> unit option

  include Monad with type 'a t := 'a option
end
