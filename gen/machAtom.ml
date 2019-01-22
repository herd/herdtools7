(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Atomicity of events *)
module type Config = sig
  val naturalsize : MachSize.sz option
  val endian : Endian.t
end

module Make(C:Config) = struct

  module Mixed = MachMixed.Make(C)

  let bellatom = false

  type hidden_atom = Atomic | Reserve | Mixed of MachMixed.t
  type atom = hidden_atom

  let default_atom = Atomic

  open Code

  let applies_atom a d = match a,d with
  | Reserve,W -> false
  | _,_ -> true

  let applies_atom_rmw ar aw = match ar,aw with
  | None,None -> true
  | _,_ -> false

  let pp_plain = Code.plain
  let pp_as_a = None

  let pp_atom = function
    | Atomic -> "A"
    | Reserve -> "R"
    | Mixed mix -> Mixed.pp_mixed mix

  let compare_atom = Pervasives.compare

  let fold_mixed f r = Mixed.fold_mixed (fun mix r -> f (Mixed mix) r) r
  let fold_non_mixed f r =  f Reserve (f Atomic r)

  let fold_atom f r =
    let r = fold_mixed f r in
    fold_non_mixed f r

  let worth_final = function
    | Atomic -> true
    | Reserve -> false
    | Mixed _ -> false

  let varatom_dir _d f = f None

  let merge_atoms a1 a2 = if a1 = a2 then Some a1 else None

  let tr_value ao v = match ao with
  | None| Some (Atomic|Reserve) -> v
  | Some (Mixed (sz,_)) -> Mixed.tr_value sz v


  module ValsMixed =
    MachMixed.Vals
      (struct
        let naturalsize () = Misc.as_some C.naturalsize
        let endian = C.endian
      end)

  let overwrite_value v ao w = match ao with
  | None| Some (Atomic|Reserve) -> w (* total overwrite *)
  | Some (Mixed (sz,o)) ->
      ValsMixed.overwrite_value v sz o w

  let extract_value v ao = match ao with
  | None| Some (Atomic|Reserve) -> v
  | Some (Mixed (sz,o)) ->
      ValsMixed.extract_value v sz o
end
