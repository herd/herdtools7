(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Dump a test, litmus sources *)

module type I = sig
  val hexa : bool
  module A : Arch.Base
  module C : Constr.S with module A = A
  module P : PseudoAbstract.S
end

module Make(I:I) : sig
  type code =  I.P.code list
  type test =  (I.A.fullstate, code, I.C.constr, I.A.location)  MiscParser.result
  val dump : out_channel -> Name.t -> test -> unit
  val lines : Name.t -> test -> string list
end = struct
  type code =  I.P.code list
  type test =  (I.A.fullstate, code, I.C.constr, I.A.location)  MiscParser.result
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

        type constr = I.C.constr
        let dump_atom a =
          let open ConstrGen in
          match a with
          | LV (loc,v) ->
              sprintf "%s=%s" (A.pp_location loc) (A.V.pp I.hexa v)
          | LL (loc1,loc2) ->
              sprintf "%s=%s" (A.pp_location loc1) (A.pp_rval loc2)

        let dump_constr = ConstrGen.constraints_to_string dump_atom

        type location = A.location
        let dump_location loc = A.pp_location loc
      end)
end
