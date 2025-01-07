(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
open Printf

module type Config = sig
  val debug : bool
  val debug_files : bool
  val verbose : int
  val libfind : string -> string
  val compat : bool
  val variant : string -> bool
end

module Make (C: Config) = struct

  let interpret_bell model =
(* phew, a lot of work to set up the interpreter *)

    let module InterpreterConfig = struct
(* Model *)
      let m = model
(* Bell stuff *)
      let bell = true
      let bell_fname = None
(* Model, restricted *)
      let showsome = false
      let debug = C.debug
      let debug_files = C.debug_files
      let profile = false
      let verbose = C.verbose
      let skipchecks = StringSet.empty
      let strictskip = false
      let cycles = StringSet.empty
      let compat = C.compat
(* Show control, useless.. *)
      let doshow = StringSet.empty
      let showraw = StringSet.empty
      let symetric = StringSet.empty
      let libfind = C.libfind
(* Variant, coming from outer world *)
      let variant = C.variant
    end in

 (* A dummy semantics! *)

    let module UnitS = struct
      module E = struct

        type event = unit
        let event_compare () () = 0
        let pp_eiid () = "eiid"
        let pp_instance () = "instance"
        let is_store () = false
        let is_pt () = false

        module Ordered = struct
          type t = unit
          let compare = event_compare
        end

        module EventSet = MySet.Make(Ordered)
        module EventRel = InnerRel.Make(Ordered)
        module EventMap = MyMap.Make(Ordered)
      end

      type test = unit
      type concrete = unit

      type event = E.event
      type event_set = E.EventSet.t
      type event_rel = E.EventRel.t
      type rel_pp = (string * event_rel) list
      type set_pp = event_set StringMap.t
    end in

    let module I = Interpreter.Make
        (InterpreterConfig)
        (UnitS)
        (struct
          (* Should not be called *)
          let partition_events _ = assert false
          let loc2events _ _ = assert false
          let check_through _ = assert false
          let pp_failure _ _ msg _ =
            if C.debug then eprintf "%s\n" msg
          let pp _ _ _ _ = ()
          let fromto _ _ = assert false
          let same_value _ _ = assert false
          let same_oa _ _ = assert false
          let writable2 _ _ = assert false
        end) in

    let empty_test = () in

    (* construct an empty ks *)
    let conc = () in
    let evts = UnitS.E.EventSet.empty
    and id = lazy UnitS.E.EventRel.empty
    and unv = lazy UnitS.E.EventRel.empty
    and po = UnitS.E.EventRel.empty in
    let ks = {I.id; unv; evts; conc; po;} in
    let vb_pp = lazy [] in

    (* Continuation: notice that it should be called once at most *)
    let function_arg st res = match res with
    | None -> Some st.I.out_bell_info
    | Some _ -> assert false in

    (* call the interpreter  and collect bell info *)
    match I.interpret
        empty_test Misc.identity ks I.init_env_empty vb_pp
        function_arg None with
    | None -> assert false (* Continuation must be called at least once *)
    | Some i ->
        if C.debug then begin
          eprintf "Bell file execute, result:\n" ;
          eprintf "%s" (BellModel.pp_info i)
        end ;
        i
end
