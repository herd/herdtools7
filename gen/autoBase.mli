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

(* Description of a test subdirectory *)
type base_st =
    {
     id : string ;      (* directory name *)
     num : int ;        (* rank in base sequence *)
     ntests : int ;     (* number of (diy) tests generated *)
     next_log : int ;   (* number of run logs present *)
   }

val alloc_log : string -> base_st -> string * base_st


module Make(A:AutoArch.S) : sig
  open A

(* Key to bases, too complex ? *)
  module K : sig
    type t =
        { cur : R.Set.t ; rel : R.Set.t ; saf : R.Set.t ; }
  end

  module Key : sig
    type t = { phase : AutoPhase.t ; key : K.t ; }
    val pp : out_channel -> t -> unit
  end

  type t

  val empty : t

(* Look/create a new base *)
  val look : Key.t -> t -> base_st * t

(* Change base value *)
  val change : Key.t -> base_st -> t -> t

end
