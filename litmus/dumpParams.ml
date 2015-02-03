(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type Config = sig
  val size : int
  val runs : int
  val avail : int option
  val stride : int option
  val timeloop : int
  val mode : Mode.t
end

module Make (O:Config) =
  struct
    open Printf

    let dump out =
      let dump_def var x = out (sprintf "#define %s %s" var x) in
      dump_def "SIZE_OF_TEST" (sprintf "%i" O.size) ;
      dump_def "NUMBER_OF_RUN" (sprintf "%i" O.runs) ;
      dump_def "AVAIL"
        (match O.avail with
        | None -> "1" | Some n -> sprintf "%i" n) ;
      begin match O.mode with
      | Mode.Std ->
          dump_def "STRIDE"
            (match O.stride with
            | None -> "(-1)" | Some i -> sprintf "%i" i) ;
            dump_def "MAX_LOOP"
            (let x = O.timeloop in if x > 0 then sprintf "%i" x else "0")
      | Mode.PreSi -> ()
      end ;
      ()
  end
