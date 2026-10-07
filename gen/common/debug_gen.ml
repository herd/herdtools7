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
  parser : bool ;
  search : bool ;
  searchsteps : bool ;
  cycle : bool ;
  model : bool ;
  io : bool ;
  }

let tags =
[
  "lexer";
  "parser";
  "cycle";
  "search";
  "searchsteps";
  "model";
  "io";
]

let none =
  {
   lexer = false ;
   parser = false ;
   search = false ;
   searchsteps = false ;
   cycle = false ;
   model = false ;
   io = false ;
 }

let parse t tag = match tag with
  | "lexer" -> Some { t with lexer = true; }
  | "parser" -> Some { t with parser = true; }
  | "search" -> Some { t with search = true; }
  | "searchsteps" -> Some { t with searchsteps = true; }
  | "cycle" -> Some { t with cycle = true; }
  | "model" -> Some { t with model = true; }
  | "io"|"IO" -> Some { t with io = true; }
  | _ -> None
