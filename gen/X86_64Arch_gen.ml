(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make
         (C:sig
              val naturalsize : MachSize.sz
              val moreedges : bool
              val fullmixed : bool
              val variant : Variant_gen.t -> bool
              val mach_size : X86_64Base.reg_part
            end) = struct
  open Code
  include X86_64Base
  let tr_endian = Misc.identity

  module ScopeGen = ScopeGen.NoGen
  module Mixed =
    MachMixed.Make
      (struct
        let naturalsize = Some C.naturalsize
        let fullmixed = C.fullmixed
      end)

  let bellatom = false

  type atom = Atomic | Mixed of MachMixed.t

  let default_atom = Atomic

  let applies_atom a d = match a,d with
    | Atomic, Code.W | Mixed _, (Code.W|Code.R) -> true
    | _,_ -> false

  let compare_atom = compare

  let merge_atoms a1 a2 = if a1 = a2 then Some a1 else None

  let pp_plain = Code.plain

  let pp_as_a = None

  let pp_atom = function
    | Atomic -> "A"
    | Mixed mix -> Mixed.pp_mixed mix

  let fold_mixed f r = Mixed.fold_mixed (fun mix r -> f (Mixed mix) r) r
  let fold_non_mixed f k = f Atomic k

  let fold_atom f r =
    let r = fold_mixed f r in
    fold_non_mixed f r

  let worth_final _ = true

  let varatom_dir _d f = f None

  module ValsMixed =
    MachMixed.Vals
      (struct
        let naturalsize () = C.naturalsize
        let endian = endian
      end)

  let tr_value ao v = match ao with
    | None | Some (Atomic) -> v
    | Some (Mixed (sz, _)) -> Mixed.tr_value sz v

  let overwrite_value v ao w = match ao with
    | None | Some (Atomic) -> w
    | Some (Mixed (sz, o)) ->
       ValsMixed.overwrite_value v sz o w

  let extract_value v ao = match ao with
    | None | Some (Atomic) -> v
    | Some (Mixed (sz, o)) ->
       ValsMixed.extract_value v sz o

  (**********)
  (* Fences *)
  (**********)

  type fence = MFence

  let is_isync _ = false

  let compare_fence = compare

  let default = MFence
  let strong = default

  let pp_fence = function
    | MFence -> "MFence"

  let fold_all_fences f r = f MFence r
  let fold_cumul_fences f r = f MFence r
  let fold_some_fences f r =  f MFence r

  let orders f d1 d2 = match f,d1,d2 with
    | MFence,_,_ -> true

  let var_fence f r = f default r

  (********)
  (* Deps *)
  (********)

  type dp

  let pp_dp _ = assert false

  let fold_dpr _f r =  r
  let fold_dpw _f r =  r

  let ddr_default = None
  let ddw_default = None
  let ctrlr_default = None
  let ctrlw_default = None

  let is_ctrlr _ = assert false
  let fst_dp _ = assert false
  let sequence_dp _ _ = assert false

  (*******)
  (* RWM *)
  (*******)

  include OneRMW
  include NoEdge

  include
    ArchExtra_gen.Make
      (struct
        type arch_reg = reg
        let is_symbolic = function
          | Symbolic_reg _ -> true
          | _ -> false
        let pp_reg = pp_reg
        let free_registers = allowed_for_symb
      end)
end
