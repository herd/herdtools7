(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Debug tags *)

type t = {
    solver : bool ;
    lexer : bool ;
    top : bool ;
    mem : bool ;
    barrier : bool ;
    res : bool ;
    rfm : bool ;
  }

let tags =
[
  "solver";
  "lexer";
  "top";
  "mem";
  "barrier"; "model"; (* handier synonymous *)
  "res";
  "rfm";
]

let none =
  {
   solver = false ;
   lexer = false ;
   top = false ;
   mem = false ;
   barrier = false ;
   res = false ;
   rfm = false
 }

let parse t tag = match tag with
  | "solver" -> Some { t with solver = true; }
  | "lexer" -> Some { t with lexer = true; }
  | "top" -> Some { t with top = true; }
  | "mem" -> Some { t with mem = true; }
  | "barrier"|"model" -> Some { t with barrier = true; }
  | "res" -> Some { t with res = true; }
  | "rfm" -> Some { t with rfm = true; }
  | _ -> None


