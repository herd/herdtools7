(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


type t = LaTeX | Txt | HeVeA | HeVeANew

let tags = ["text";"latex";"hevea";"heveanew"]

let parse s =
  match Misc.lowercase s with
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
