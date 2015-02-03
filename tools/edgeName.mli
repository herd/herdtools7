(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Definition order gives sorting order *)

type bigatom = PP | AA

type e =
  | Po | PoS
  | Addr
  | Data
  | Ctrl
  | CtrlISync
  | CtrlISB
  | Rfi
  | Fri
  | Wsi
  | ISync
  | ISB
  | Eieio
  | LwSync
  | Sync
  | DMBST
  | DMB
  | DSBST
  | DSB
  | Atom of bigatom list
  | Custom of string

type a = P | R | A

type p = N|S

type t = e * a * a * p



val pp : t -> string
val dbg : t -> string

