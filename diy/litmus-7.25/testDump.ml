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
  module A : Arch.Base
  module C : Constr.S with module A = A
  module P : PseudoAbstract.S
end

module Make(I:I) : sig
  type code =  I.P.code list
  type test =  (I.A.fullstate, code, I.C.prop, I.A.location)  MiscParser.result
  val dump : out_channel -> Name.t -> test -> unit
  val lines : Name.t -> test -> string list
end = struct
  type code =  I.P.code list
  type test =  (I.A.fullstate, code, I.C.prop, I.A.location)  MiscParser.result
  include SimpleDumper_prime.Make
      (struct
        open Printf

        module A = I.A

        module P = I.P

        let dump_state_atom =
          MiscParser.dump_state_atom A.pp_location (A.V.pp I.hexa)

        type state = A.fullstate

        let dump_state st =
          String.concat " "
            (List.map
               (fun a -> sprintf "%s;" (dump_state_atom a))
               st)

        type prop = I.C.prop
        let dump_atom a =
          let open ConstrGen in
          match a with
          | LV (loc,v) ->
              sprintf "%s=%s" (A.pp_location loc) (A.V.pp I.hexa v)
          | LL (loc1,loc2) ->
              sprintf "%s=%s" (A.pp_location loc1) (A.pp_rval loc2)

        let dump_prop = ConstrGen.prop_to_string dump_atom
        let dump_constr = ConstrGen.constraints_to_string dump_atom

        type location = A.location
        let dump_location loc = A.pp_location loc
      end)
end
