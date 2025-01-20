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

module Edge : sig
  type kind = Data | Control | Order
  type t = {
    left: string;
    right: string;
    desc: string -> string -> string;
    kind: kind;
  }
end
  
module Node : sig
  type kind = Fault | Mem | Reg_Data | Branching | Reg_Other | Empty
  type t = {
    desc: string;
    kind: kind;
  }
end

type t = {
  nodes: Node.t StringMap.t;
  edges: Edge.t list;
}

val tr: ParsedDotGraph.t -> string option -> t * ParsedDotGraph.t
val describe : t -> string
