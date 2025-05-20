(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Before actual Cat code *)

open Printf

type tag = And | Or | Seq

let rec get_tag = function
  | [] ->  Warn.fatal "No tag"
  | w::ws ->
      begin
        match Misc.lowercase w with
        | "one" -> Or
        | "all" -> And
        | _ -> get_tag ws
      end

type reduced =
  | Rel of string * (string * string)
  | Set of string * string

let reduce find ws =
  let name,args = find ws in
  match args with
  | [| e |] -> Set (name,e)
  | [| e1; e2; |] -> Rel (name,(e1,e2))
  | _ -> assert false

type t =
  | Connect of tag * string list * t list
  | Arg of reduced * string list

type d = Def of tag * reduced * string list * t list


let pp_tag = function
  | Or -> "Or"
  | And -> "And"
  | Seq -> "Seq"

let pp_reduced chan = function
  | Rel (name,(e1,e2)) ->
      fprintf chan "%s(%s,%s)" name e1 e2
  | Set (name,e) ->
      fprintf chan "%s(%s)" name e

let rec do_pp_tree i chan  = function
  | Connect (tag,_,args) ->
      fprintf chan "%s<%s>\n" i (pp_tag tag) ;
      do_pp_trees ("  "^i) chan args ;
      if i = "" then fprintf chan "\n%!"
  | Arg (pp,_) -> fprintf chan "%s%a\n" i pp_reduced pp

and do_pp_trees i chan = List.iter (do_pp_tree i chan)


let pp_def chan = function
  | Def (tag,pp,_,args) ->
      fprintf chan "<%s>%a\n" (pp_tag tag) pp_reduced pp ;
      do_pp_trees "  " chan args

let pp_defs chan = List.iter (fprintf chan "%a\n" pp_def)
