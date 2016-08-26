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

open Printf

module type Config = sig val verbose : int end

module Make(O:Config) = struct


let prerr_ifverb lvl s =
  if O.verbose > lvl then  begin
    prerr_string "Warning: " ;
    prerr_endline s
  end

let warn fmt = ksprintf  (prerr_ifverb 0) fmt
let warn1 fmt = ksprintf  (prerr_ifverb 1) fmt


end
let warn_always fmt =
  kfprintf
    (fun chan -> output_char chan '\n' ; flush chan)
    stderr fmt

let prerr_exit s =
  prerr_string "Stop here: " ;
  prerr_endline s ;
  raise Misc.Exit

let exit fmt = ksprintf prerr_exit fmt  

let user_error fmt =
  ksprintf (fun msg ->raise  (Misc.UserError msg))  fmt 

let fatal fmt =
  ksprintf (fun msg ->raise  (Misc.Fatal msg))  fmt 
  
