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
      val precision : Precision.t
      val variant : Opt.t -> bool
      val set_precision : Precision.t ref -> Opt.t -> bool
    end) : sig
      type t = Var.Opt.t
      val precision : Precision.t
      val variant : Var.Opt.t -> bool
    end= struct
      type t = Var.Opt.t

      let pref = ref Var.precision
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
              let setnow = Var.set_precision pref
            end in
            let module P = ParseTag.MakeS(Opt) in
            try
              List.iter (P.parse_tag_set "variant" vref) tags
            with Arg.Bad msg ->  Warn.user_error "%s" msg

      let precision = !pref
      let variant = !vref
    end
