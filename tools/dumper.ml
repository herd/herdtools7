(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make(A:Arch_tools.S) = struct
  include
      SimpleDumper.Make
      (struct
        module A = A

        type v = A.v
        let dump_v = A.pp_v

        type state_atom = (A.location * (TestType.t * A.v))

        let dump_state_atom a =
          MiscParser.dump_state_atom
            A.is_global A.pp_location dump_v a

        type state = state_atom list

        let dump_state st =
          DumpUtils.dump_state
            dump_state_atom
            (A.env_for_pp st)

        type prop = (A.location,A.v) ConstrGen.prop

        let dump_atom a =
          ConstrGen.dump_atom
            A.pp_location A.pp_location_brk A.pp_v a

        let dump_prop = ConstrGen.prop_to_string dump_atom
        let dump_constr = ConstrGen.constraints_to_string dump_atom

        type location = A.location
        let dump_location loc = A.pp_location loc
      end)
end
