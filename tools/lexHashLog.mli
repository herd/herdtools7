(*********************************************************************)
(*                       DIY                                         *)
(*                                                                   *)
(*               Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type Config = sig
  val verbose : bool  
  val env : string StringMap.t
end

module Make :
 functor (C:Config) -> sig

   val check : string -> unit
   val rewrite : string -> unit

   val check_chan : in_channel -> unit
   val rewrite_chan : in_channel -> unit

 end
