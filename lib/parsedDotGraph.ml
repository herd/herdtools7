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

module Node = struct
  type t = {
    name: string;
    attrs: Attr.t list;
  }

  let pp node =
    let printed_attrs = String.concat ", " (List.map Attr.pp node.attrs) in
    Printf.sprintf "%s [%s]" node.name printed_attrs
end

module Edge = struct
  type t = {
    left : string;
    right : string;
    attrs : Attr.t list;
  }

  let pp edge =
    let printed_attrs = String.concat ", " (List.map Attr.pp edge.attrs) in
    Printf.sprintf "%s -> %s [%s]" edge.left edge.right printed_attrs
end

module rec Stmt : sig
  type t = Node of Node.t | Attr of Attr.t | Edge of Edge.t | Subgraph of Subgraph.t
  val pp : t -> string
end = struct
  type t = Node of Node.t | Attr of Attr.t | Edge of Edge.t | Subgraph of Subgraph.t

  let pp = function
  | Node n -> Printf.sprintf "%s;" (Node.pp n)
  | Attr a -> Printf.sprintf "%s;" (Attr.pp a)
  | Edge e -> Printf.sprintf "%s;" (Edge.pp e)
  | Subgraph g -> Subgraph.pp g
end

and Subgraph : sig
  type t = {
    name: string;
    stmts : Stmt.t list;
  }
  val pp : t -> string
end = struct
  type t = {
    name: string;
    stmts : Stmt.t list;
  }

  let pp g =
    let printed_stmts = (String.concat "\n" (List.map Stmt.pp g.stmts)) in
    Printf.sprintf "subgraph %s {\n%s\n}" g.name printed_stmts
end

type t = {
  name: string;
  stmts: Stmt.t list
}

let pp g =
  let printed_stmts = String.concat "\n" (List.map Stmt.pp g.stmts) in
  Printf.sprintf "digraph %s {\n%s\n}" g.name printed_stmts  
