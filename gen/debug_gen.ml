(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Debug tags *)

type t = {
  lexer : bool ;
  top : bool ;
  generator : bool ;
  model : bool ;
  }

let tags =
[
  "lexer";
  "top";
  "generator";"gen";
  "model";
]

let none =
  {
   lexer = false ;
   top = false ;
   generator = false ;
   model = false ;
 }

let parse t tag = match tag with
  | "lexer" -> Some { t with lexer = true; }
  | "top" -> Some { t with top = true; }
  | "generator"|"gen" -> Some { t with generator = true; }
  | "model" -> Some { t with model = true; }
  | _ -> None


