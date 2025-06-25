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
      module Opt:ParseTag.SArg
      val info : MiscParser.info
      val variant : Opt.t -> bool
      val mte_precision : Precision.t
      val mte_store_only : bool
      val fault_handling : Fault.Handling.t
      val sve_vector_length : int
      val sme_vector_length : int
    end) =
    struct
      type t = Var.Opt.t

      module Refs = struct

        let mte_precision = ref Var.mte_precision
        let mte_store_only = ref Var.mte_store_only
        and fault_handling = ref Var.fault_handling
        and sve_vector_length = ref Var.sve_vector_length
        and sme_vector_length = ref Var.sme_vector_length

        let variant = ref Var.variant
    end

      let () =
        match
          MiscParser.get_info_on_info
            MiscParser.variant_key Var.info
        with
        | None -> ()
        | Some tags ->
            let tags = LexSplit.strings_spaces tags in
            let module Opt = ParseTag.MakeOptS(Var.Opt)(Refs) in
            let module P = ParseTag.MakeS(Opt) in
            try
              List.iter (P.parse_tag_set "variant" Refs.variant) tags
            with Arg.Bad msg ->  Warn.user_error "%s" msg

       let mte_precision = !Refs.mte_precision
       let mte_store_only = !Refs.mte_store_only
       and fault_handling = !Refs.fault_handling
       and sve_vector_length = !Refs.sve_vector_length
       and sme_vector_length = !Refs.sme_vector_length
       and variant = !Refs.variant

    end
