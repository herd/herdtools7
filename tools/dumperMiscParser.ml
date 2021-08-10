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

module Make(Opt:Opt)(Out:SimpleDumper.Out)(A:ArchBase.S) : sig
  val dump_info :
      Out.t -> Name.t -> A.pseudo MiscParser.t -> unit
end = struct
  include
   SimpleDumper.Make(Out)
      (struct
        open Printf

        module A = A

        type v = MiscParser.maybev
        let dump_v = ParsedConstant.pp Opt.hexa

        let dump_loc = MiscParser.dump_location

        type state_atom = MiscParser.state_atom
        type state = MiscParser.state

        let env_for_pp = MiscParser.env_for_pp

        let dump_state_atom a =
          MiscParser.dump_state_atom
            MiscParser.is_global dump_loc dump_v  a


        type prop = MiscParser.prop

        let dump_atom a =
          let open ConstrGen in
          match a with
          | LV (rloc,v) ->
              sprintf "%s=%s"
                (ConstrGen.dump_rloc dump_loc rloc)
                (ParsedConstant.pp_v v)
          | LL (loc1,loc2) ->
              sprintf "%s=%s" (dump_loc loc1) (MiscParser.dump_rval loc2)
          | FF f ->
              Fault.pp_fatom ParsedConstant.pp_v f

        let dump_prop = ConstrGen.prop_to_string dump_atom
        let dump_constr = ConstrGen.constraints_to_string dump_atom

        type location = MiscParser.location
        let dump_location = dump_loc
      end)
end
