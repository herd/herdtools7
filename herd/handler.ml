(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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

