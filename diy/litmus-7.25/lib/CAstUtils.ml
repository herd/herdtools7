(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open CType
open CAst

(*
   Strip away initial pointer of argument types,
   as litmus considers the type of a location
   to be the type of what is in the location.
   Code is shared as herd uses litmus parse tree to compute test
   hashes
*)

let strip_pointers f =
  match f with
  | CAst.Global _ -> f
  | Test t ->
      let ps =
        List.map
          (fun p -> match p.param_ty with
          | Pointer ty -> { p with param_ty=ty; }
          | _ ->
              Warn.fatal
                "parameter %s of function P%i is not a pointer"
                p.param_name t.proc) t.params in
      Test { t with params = ps; }
