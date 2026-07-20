(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Extending built-in / base modules, either to port future features into
 *  earlier versions of OCaml, or to add extra functionality. *)

module Fun = struct
  let open_out_protect f name =
    let out = open_out name in
    Stdlib.Fun.protect ~finally:(fun () -> close_out out) (fun () -> f out)

  module Syntax = struct
    let ( let@ ) f x = f x
  end
end

module List = struct
  include List

  let to_ocaml_string f xs =
    Printf.sprintf "[%s]" (String.concat "; " (List.map f xs))

  let for_every_element p lst =
    let rec loop acc = function [] -> acc | v :: xs -> loop (p v && acc) xs in
    loop true lst
end

module Option = struct
  include Option

  type 'a t = 'a option

  let to_ocaml_string f o =
    match o with
    | None -> "None"
    | Some a -> Printf.sprintf "Some (%s)" (f a)
end

module String = struct
  include String

  let to_ocaml_string s = Printf.sprintf "%S" s
end

module Iter = struct
  type 'a t = unit -> 'a option

  let of_list xs =
    let r = ref xs in
    fun () ->
      match !r with
      | [] -> None
      | x::xs -> r := xs ; Some x
end
