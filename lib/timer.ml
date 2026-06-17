(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type S = sig
  val create : string -> unit
  val start : string -> unit
  val stop : string -> unit
  val pp : Format.formatter -> unit
end

let sem_key = "semantics"
and model_key = "model"
and run_key = "run"

module Ok = struct

  let _dbg = false

  type timer = { start:float; total:float; }

  let zero = { start=0.0; total=0.0; }

  let error = Warn.fatal "Non-existant timer: %s"

  let table = Hashtbl.create 17
  let seen_t = Hashtbl.create 17

  let seen key = Hashtbl.mem seen_t key
  let push key = Hashtbl.add seen_t key ()
  let pop key = Hashtbl.remove seen_t key

  let create key =
    if not (Hashtbl.mem table key) then
      Hashtbl.add table key zero

  let start_timer key t =
    if seen key then begin
      if _dbg then Warn.warn_always "Recursive timer start: %s\n%!" key ;
      push key ;
      t
    end else begin
      push key ;
      { t with start=Sys.time (); }
    end

  and stop_timer key t =
    if seen key then begin
      pop key ;
      if seen key then t
      else { t with total=Sys.time () -. t.start +. t.total; }
    end else
      Warn.fatal "Timer %s has not been started!\n" key
  let map_timer key f =
    try
      let t = Hashtbl.find table key in
      Hashtbl.replace table key (f key t)
    with Not_found -> error key


  let start key = map_timer key start_timer
  and stop key = map_timer key stop_timer

  open Format

  let do_pp fmt key t = fprintf fmt "%s=%0.2f" key t.total

  let pp fmt =
    let fst = ref true in
    fprintf fmt "Timers: " ;
    Hashtbl.iter
          (fun key t ->
          if not !fst then fprintf fmt ", " ;
          do_pp fmt key t ;
          fst := false)
          table ;
    fprintf fmt "\n"
end

module No = struct
  let nofun _ = ()
  let create = nofun
  let start = nofun
  let stop = nofun
  let pp = nofun
end
