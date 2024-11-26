(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2024-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Attr = struct
  type t = {
    name: string;
    value: string;
  }

  let pp attr =
    Printf.sprintf "%s=\"%s\"" attr.name attr.value
end

module Stmt = struct
  type t = Attr of Attr.t

  let pp = function
  | Attr a -> Printf.sprintf "%s;" (Attr.pp a)
end

type t = {
  name: string;
  stmts: Stmt.t list
}

let pp g =
  let printed_stmts = String.concat "\n" (List.map Stmt.pp g.stmts) in
  Printf.sprintf "digraph %s {\n%s\n}" g.name printed_stmts  
