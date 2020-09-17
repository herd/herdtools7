(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Simple signal handler managment *)

let handlers = ref []

let set_handlers () =
  let doit signal =
    Sys.set_signal
      signal
      (Sys.Signal_handle
         (fun _ -> List.iter (fun f -> f ()) !handlers ; exit 1))  in
  doit Sys.sigint ;
  doit Sys.sigterm ;
  doit Sys.sighup ;
  ()

let () = set_handlers ()

let push f = handlers := f :: !handlers
let pop () = match !handlers with
| [] -> ()
| _::rem -> handlers := rem
