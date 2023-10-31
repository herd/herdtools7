(****************************************************************************)
(*                           The diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make :
  functor
    (S:SemExtra.S with type A.reg = AArch64Base.reg)
  ->  functor
    (U:sig
         open S

         val mops_size : MachSize.sz
         val endian : Endian.t

         val nzcv : A.reg

         val read_reg_ord :
             A.reg -> A.inst_instance_id -> A.V.v M.t
         val read_reg_data :
           MachSize.sz -> A.reg -> A.inst_instance_id -> A.V.v M.t
         val write_reg :
           A.reg -> A.V.v -> A.inst_instance_id -> unit M.t
         val write_reg_dest :
           A.reg -> A.V.v -> A.inst_instance_id -> A.V.v M.t
         val do_append_commit :
           'a M.t -> string option -> A.inst_instance_id -> 'a M.t
         val is_this_reg :
           A.reg -> event -> bool

         val read_mem :
           MachSize.sz -> A.V.v -> A.inst_instance_id -> A.V.v M.t
         val write_mem :
           MachSize.sz -> A.V.v -> A.V.v -> A.inst_instance_id
           -> unit M.t
       end) ->
  sig
    open S

    val cpyf :
      AArch64Base.stage ->
      A.reg ->
      A.reg ->
      A.reg ->
      A.inst_instance_id -> (M.A.V.v * M.A.V.v * A.V.v) M.t

    val cpy :
      AArch64Base.stage ->
      A.reg ->
      A.reg ->
      A.reg ->
      A.inst_instance_id ->
      (int option * (M.A.V.v * M.A.V.v * A.V.v)) M.t

    val mset :
      AArch64Base.stage ->
      A.reg ->
      A.reg ->
      A.reg -> A.inst_instance_id -> (M.A.V.v * A.V.v) M.t
  end
