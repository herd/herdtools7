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

(** Parse herd output logs. *)

module Option = Base.Option

type t = {
  name : string ;
  kind : ConstrGen.kind option ;
}

let to_ocaml_string l =
  OcamlString.record [
    "name", Base.String.to_ocaml_string l.name ;
    "kind", Option.to_ocaml_string ConstrGen.pp_kind l.kind ;
  ]

let compare a b =
  Compare.chain [
    String.compare a.name b.name ;
    Option.compare ConstrGen.compare_kind a.kind b.kind ;
  ]

let split = LexSplit.words

let rec next_test k = function
  | [] -> List.rev k
  | line::lines ->
      match split line with
      | ["Test"; name ; ("Forbidden"|"Forbid"|"Allowed");] ->
          in_test k name true lines
      | ["Test"; name ; "Required";] ->
          in_test k name false lines
      | _ -> next_test k lines

and in_test k name exists = function
  | [] -> List.rev k
  | line::lines ->
      match split line with
      | "Observation"::name2::r::_ ->
          if name <> name2 then next_test k lines
          else begin
            let kind =
              let open ConstrGen in
              match r with
              | "Always" ->
                  if exists then Allow else Require
              | "Sometimes" -> Allow
              | "Never" -> Forbid
              | _ -> assert false in
            next_test ({ name; kind=Some kind; }::k) lines
          end
      | "Test"::_ -> next_test k (line::lines)
      | _ -> in_test k name exists lines

let of_string_list ls = next_test [] ls
