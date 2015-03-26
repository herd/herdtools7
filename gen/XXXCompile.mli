(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

module type S = sig
  include CompileCommon.S

  val ppo : (R.relax -> 'a -> 'a) -> 'a -> 'a

  val emit_load :
      A.st -> Code.proc -> A.init -> Code.loc ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_load_not_zero :
      A.st -> Code.proc -> A.init -> Code.loc ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_load_one :
      A.st -> Code.proc -> A.init -> Code.loc ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_load_not_eq :
      A.st -> Code.proc -> A.init -> Code.loc -> A.reg ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_load_not_value :
      A.st -> Code.proc -> A.init -> Code.loc -> int ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_access : A.st -> Code.proc -> A.init -> C.event ->
    A.reg option * A.init * A.pseudo list * A.st

  val emit_exch : A.st -> Code.proc -> A.init -> C.event ->  C.event ->
    A.reg * A.init * A.pseudo list * A.st

  val emit_access_dep :
      A.st -> Code.proc -> A.init -> C.event -> A.dp ->
        A.reg -> A.reg option * A.init * A.pseudo list * A.st

  val emit_exch_dep :
      A.st -> Code.proc -> A.init -> C.event ->  C.event ->
        A.dp -> A.reg ->
          A.reg * A.init * A.pseudo list * A.st

  val emit_fence : A.fence -> A.pseudo

  val stronger_fence : A.fence

  val emit_fno :
      A.st -> Code.proc -> A.init -> Code.loc ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_fno2 :
      A.st -> Code.proc -> A.init -> Code.loc ->
        A.reg * A.init * A.pseudo list * A.st

  val emit_open_fno :
      A.st -> Code.proc -> A.init -> Code.loc ->
        A.reg * A.init * A.pseudo list * string * A.st
      
  val emit_close_fno :
      A.st -> Code.proc -> A.init -> string -> A.reg -> Code.loc ->
        A.init * A.pseudo list * A.st

  val check_load :
      Code.proc -> A.reg -> C.event -> A.pseudo list -> A.pseudo list
  val postlude : A.st -> Code.proc -> A.init -> A.pseudo list ->
    A.init * A.pseudo list * A.st
end
