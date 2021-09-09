(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Dump a test, litmus sources *)

module type I = sig
  val hexa : bool
  module A : Arch_litmus.Base
  module C : Constr.S with
  module V = A.V and
  type location = A.location and
  module LocSet = A.LocSet (* and
  type rlocation = A.location ConstrGen.rloc and
  module RLocSet = A.RLocSet *)
  module P : PseudoAbstract.S
end

module Make(I:I) : sig
  type code =  I.P.code list
  type test =  (I.A.fullstate, code, I.C.prop, I.A.location, I.A.V.v)  MiscParser.result
  val dump : out_channel -> Name.t -> test -> unit
  val lines : Name.t -> test -> string list
end = struct
  type code =  I.P.code list
  type test =  (I.A.fullstate, code, I.C.prop, I.A.location, I.A.V.v)  MiscParser.result

  include
    CoreDumper.Make
      (struct
        open I

        module Out = struct
          type t = out_channel
          let fprintf chan fmt = Printf.fprintf chan fmt
        end

        let arch = A.arch

        type prog = P.code list
        let print_prog = P.print_prog
        let dump_prog_lines = P.dump_prog_lines

        type v = A.V.v
        let dump_v = A.V.pp hexa

        let dump_state_atom =
          MiscParser.dump_state_atom A.is_global A.pp_location dump_v

        type state = A.fullstate

        let dump_state st =
          DumpUtils.dump_state
            dump_state_atom
            (A.env_for_pp st)

        type prop = C.prop

        let dump_atom a =
          ConstrGen.dump_atom A.pp_location A.pp_location_brk dump_v a

        let dump_prop = ConstrGen.prop_to_string dump_atom
        let dump_constr = ConstrGen.constraints_to_string dump_atom

        type location = A.location
        let dump_location loc = A.pp_location loc

      end)

end
