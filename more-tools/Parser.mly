(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

%parameter<O:FindDef.Config>

%{
open PreCat

module FD = FindDef.Make(O)

let reduce_arg = PreCat.reduce FD.find
and reduce_def = PreCat.reduce FD.find_def

%}

%type <PreCat.d list> defs
%start defs

%%

let define :=
|ws=words; COLON; args=arg0+; { Def (get_tag ws, reduce_def ws,args,ws) }

let arg0 :=
| ROUND; ws=words; DOT; { Arg (reduce_arg ws,ws) }
| ROUND; ws=words; COLON; args=arg1+; { Connect (get_tag ws,ANone,args,ws) } 

let arg1 :=
| DASH; ws=words; DOT; { Arg (reduce_arg ws,ws) }
| DASH; ws=words; COLON; args=arg2+; { Connect (get_tag ws,ANone,args,ws) }


let arg2 :=
| PLUS; ws=words; DOT; { Arg (reduce_arg ws,ws) }

let words == ws=WORD+; { ws }

let defs := ds=define+; EOF;  { ds }
