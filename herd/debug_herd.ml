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

(* Debug tags *)

type t = {
    solver : bool ;
    lexer : bool ;
    top : bool ;
    mem : bool ;
    monad : bool ;
    barrier : bool ;
    res : bool ;
    rfm : bool ;
    pretty : bool ;
    mixed : bool ;
    files : bool ;
    timeout : bool ;
    profile_cat: bool ;
    exc : bool ;
  }

let tags =
[
  "solver";
  "lexer";
  "top";
  "mem";
  "monad";
  "barrier"; "model"; (* handier synonymous *)
  "res";
  "rfm";
  "pretty";
  "mixed";
  "files";
  "timeout";
  "profile_cat";
  "exception";
]

let none =
  {
   solver = false ;
   lexer = false ;
   top = false ;
   mem = false ;
   monad = false ;
   barrier = false ;
   res = false ;
   rfm = false ;
   pretty = false ;
   mixed = false ;
   files = false ;
   timeout = false ;
   profile_cat = false;
   exc = false ;
 }

let parse t tag = match tag with
  | "solver" -> Some { t with solver = true; }
  | "lexer" -> Some { t with lexer = true; }
  | "top" -> Some { t with top = true; }
  | "mem" -> Some { t with mem = true; }
  | "monad" -> Some { t with monad = true; }
  | "barrier"|"model" -> Some { t with barrier = true; }
  | "res" -> Some { t with res = true; }
  | "rfm" -> Some { t with rfm = true; }
  | "pretty" -> Some { t with pretty = true ;}
  | "mixed" -> Some { t with mixed = true ;}
  | "files"|"file" -> Some { t with files = true ;}
  | "timeout" -> Some { t with timeout = true ;}
  | "exception"|"exc" -> Some { t with exc = true ;}
  | "profile_cat" -> Some { t with profile_cat = true ;}
  | _ -> None
