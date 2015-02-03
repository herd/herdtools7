(*********************************************************************)
(*                        DIY                                        *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(**********)
(* Digest *)
(**********)

module type Input = sig
  type code

  val dump_prog : code -> string list
end

module Make(P:Input)
    = struct

      open Printf
      open MiscParser

      let verbose = 0

      let debug tag s =
        if verbose > 0 then eprintf "%s:\n%s\n" tag s
        else ()



(* Code digest *)

      let digest_code code =
        let code = List.map P.dump_prog code in
        let pp =  Misc.string_of_prog code in
        debug "CODE" pp ;
        Digest.string pp


(* Observed locations digest *)
      let digest_observed locs =
        let locs = MiscParser.LocSet.elements locs in
        let pp = String.concat "; " (List.map dump_location locs) in
        debug "LOCS" pp ;
        Digest.string pp


      let digest init code observed =
        Digest.to_hex
          (Digest.string
             (TestHash.digest_init debug init ^ digest_code code ^
              digest_observed observed))
    end
