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
  type t

  val start : t -> unit
  val stop : t -> unit

  val pp : Format.formatter -> t -> unit

  val semantics : t
  val model : t
  val run : t
end

module Ok = struct

  type t = {
    name : string;
    mutable start_time : float option;
    mutable total : float;
    mutable depth : int;
  }

  let create name = { name; start_time = None; total = 0.0; depth = 0; }

  let start t =
    if t.depth = 0 then t.start_time <- Some (Sys.time ());
    t.depth <- t.depth + 1

  let stop t =
    match t.start_time with
    | None -> Warn.fatal "Timer %s has not been started" t.name
    | Some start_time ->
        let depth = t.depth - 1 in
        if depth < 0 then Warn.fatal "Timer %s has been stopped too many times" t.name;
        t.depth <- depth;
        if depth = 0 then begin
          t.total <- t.total +. Sys.time () -. start_time;
          t.start_time <- None
        end

  let pp fmt t = Format.fprintf fmt "%s=%0.2f" t.name t.total

  let semantics = create "semantics"
  let model = create "model"
  let run = create "run"
end

module No = struct
  type t = unit

  let start _ = ()
  and stop _ = ()

  let pp _ _ = ()

  let semantics = ()
  let model = ()
  let run = ()
end
