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

type arg = ANone | ASet of string | ARel of string * string

type name =
  | Plus of name
  | Inverse of string
  | Name of string
  | Neg of name
  | Names of string list

let rec get_name = function
  |Plus n|Neg n -> get_name n
  |Inverse n|Name n -> n
  |Names ns -> String.concat "" ns

let rec pp_name = function
  | Plus (Name n) -> sprintf "%s+" n
  | Plus n -> sprintf "(%s)+" (pp_name n)
  | Inverse n -> sprintf "%s^-1" n
  | Name n -> n
  | Neg (Name n) -> sprintf "~%s" n
  | Neg n -> sprintf "~(%s)" (pp_name n)
  | Names ns -> String.concat " & " ns

let rec map_name f = function
  | Name n -> Name (f n)
  | Plus n -> Plus (map_name f n)
  | Inverse n -> Inverse (f n)
  | Neg n -> Neg (map_name f n)
  | Names ns -> Names (List.map f ns)

type reduced =
  | Rel of name * (string * string)
  | Set of name * string

let reduce find ws =
  let name,args = find ws in
  match args with
  | [| e |] -> Set (name,e)
  | [| e1; e2; |] -> Rel (name,(e1,e2))
  | _ -> assert false

type t =
  | Connect of tag * arg * t list * string list
  | Arg of reduced * string list

type d = Def of tag * reduced * t list  * string list


let pp_tag = function
  | Or -> "Or"
  | And -> "And"
  | Seq -> "Seq"

let pp_args = function
  | ANone -> ""
  | ASet e -> sprintf "(%s)" e
  | ARel (e1,e2) -> sprintf "(%s,%s)" e1 e2

let pp_reduced chan = function
  | Rel (name,(e1,e2)) ->
      fprintf chan "%s(%s,%s)" (pp_name name) e1 e2
  | Set (name,e) ->
      fprintf chan "%s(%s)" (pp_name name) e

let rec do_pp_tree i chan  = function
  | Connect (tag,args,ts,_) ->
      fprintf chan "%s<%s%s>\n" i (pp_tag tag) (pp_args args);
      do_pp_trees ("  "^i) chan ts ;
      if i = "" then fprintf chan "\n%!"
  | Arg (pp,_) -> fprintf chan "%s%a\n" i pp_reduced pp

and do_pp_trees i chan = List.iter (do_pp_tree i chan)

let pp_trees chan = do_pp_trees "  "chan

let pp_tree chan t= pp_trees chan [t]

let pp_def chan = function
  | Def (tag,pp,args,_) ->
      fprintf chan "<%s>%a\n" (pp_tag tag) pp_reduced pp ;
      do_pp_trees "  " chan args

let pp_defs chan = List.iter (fprintf chan "%a\n" pp_def)
