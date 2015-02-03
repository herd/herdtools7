(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


type t = LaTeX | Txt | HeVeA | HeVeANew

let tags = ["text";"latex";"hevea";"heveanew"]

let parse s = 
  match String.lowercase s with
  | "text" -> Some Txt
  | "latex" -> Some LaTeX
  | "hevea" -> Some HeVeA
  | "heveanew" -> Some HeVeANew
  | _ -> None 

let pp mode =
  match mode with
  | Txt -> "text"
  | LaTeX -> "latex"
  | HeVeA -> "hevea"
  | HeVeANew -> "heveanew"
