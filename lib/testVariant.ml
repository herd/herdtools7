(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Parse in-test variant info *)

module
  Make
    (Var:sig
      module Opt:sig
        include ParseTag.Opt
        val compare : t -> t -> int
      end
      val info : MiscParser.info
      val variant : Opt.t -> bool
      val mte_precision : Precision.t
      val set_mte_precision : Precision.t ref -> Opt.t -> bool
      val fault_handling : Fault.Handling.t
      val set_fault_handling : Fault.Handling.t ref -> Opt.t -> bool
    end) : sig
      type t = Var.Opt.t
      val mte_precision : Precision.t
      val fault_handling : Fault.Handling.t
      val variant : Var.Opt.t -> bool
    end= struct
      type t = Var.Opt.t

      let mte_pref = ref Var.mte_precision
      let fault_href = ref Var.fault_handling
      and vref = ref Var.variant

      let () =
        match
          MiscParser.get_info_on_info
            MiscParser.variant_key Var.info
        with
        | None -> ()
        | Some tags ->
            let tags = LexSplit.strings_spaces tags in
            let module Opt = struct
              include Var.Opt
              let setnow t =
                Var.set_fault_handling fault_href t ||
                Var.set_mte_precision mte_pref t
            end in
            let module P = ParseTag.MakeS(Opt) in
            try
              List.iter (P.parse_tag_set "variant" vref) tags
            with Arg.Bad msg ->  Warn.user_error "%s" msg

      let mte_precision = !mte_pref
      let fault_handling = !fault_href
      let variant = !vref
    end
