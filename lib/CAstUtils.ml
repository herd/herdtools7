(*********************************************************************)
(*                          Litmus/Herd                              *)
(*                                                                   *)
(*     Luc Maranget, INRIA Paris-Rocquencourt, France.               *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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
