(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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


