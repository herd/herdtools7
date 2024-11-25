%{
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

open ParsedDotGraph
%}

%token EOF
%token <string> NAME

%token LCURLY RCURLY
%token GRAPH

%type <ParsedDotGraph.t> graph
%type <ParsedDotGraph.t list> graph_list
%type <ParsedDotGraph.t list> main
%start main
%%

main:
| graph_list EOF { $1 }

graph_list:
| { [] }
| graph graph_list { $1 :: $2 }

graph:
| GRAPH name=NAME LCURLY content=NAME RCURLY { { name=name; content=content; } }
