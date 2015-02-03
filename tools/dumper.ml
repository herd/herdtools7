(*********************************************************************)
(*                          DIY                                      *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

module Make(A:Arch.S) = struct
  include
      SimpleDumper.Make
      (struct
        module A = A
            
        type atom = (A.location * (MiscParser.run_type * A.v))
        type state = atom list
              
        let dump_atom_state a =
          MiscParser.dump_state_atom A.pp_location A.pp_v a


        let dump_state st =
          String.concat " "
            (List.map
               (fun a -> sprintf "%s;" (dump_atom_state a))
               st)

        type prop = (A.location,A.v) ConstrGen.prop
        type constr = prop ConstrGen.constr

        let dump_atom a =
          let open ConstrGen in
          match a with
          | LV (loc,v) ->
              sprintf "%s=%s"
                (A.pp_location loc) (A.pp_v v)
          | LL (loc1,loc2) ->
              sprintf "%s=%s"
                (A.pp_location loc1)
                (A.pp_rval loc2)

        let dump_constr = ConstrGen.constraints_to_string dump_atom

        type location = A.location
        let dump_location loc = A.pp_location loc
      end)
end
