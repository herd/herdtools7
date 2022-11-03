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

(* Simple pretty printer for parsed tests (no alloc..) *)

module type Opt = sig
  val hexa : bool
end

module Make(Opt:Opt)(A:ArchBase.S) : CoreDumper.S
  with type test =  A.pseudo MiscParser.t
= struct
  include
   SimpleDumper.Make
      (struct

        module A = A

        type v = MiscParser.maybev
        let dump_v = ParsedConstant.pp Opt.hexa

        let dump_loc = MiscParser.dump_location

        let dump_state_atom a =
          MiscParser.dump_state_atom
            MiscParser.is_global dump_loc dump_v  a

        type state = MiscParser.state

        let dump_state st =
          DumpUtils.dump_state
            dump_state_atom
            (MiscParser.env_for_pp st)

        type prop = MiscParser.prop

        let dump_atom a =
          ConstrGen.dump_atom
            dump_loc MiscParser.dump_location_brk ParsedConstant.pp_v MiscParser.dump_fault_type
            a

        let dump_prop = ConstrGen.prop_to_string dump_atom
        let dump_constr = ConstrGen.constraints_to_string dump_atom

        type location = MiscParser.location
        let dump_location = dump_loc

        type fault_type = MiscParser.fault_type
        let dump_fault_type = MiscParser.dump_fault_type
      end)
end
