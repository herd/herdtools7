(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)


module PPC = struct
  module P = PPCArch.Make(SymbConstant)(PPCArch.Config)
  module A = AutoArch.Make(P)
  let testing =
    "Pod**, DpAddrdR,DpAddrdW, DpDatadW,\
     DpCtrldW,DpCtrldR,DpCtrlIsyncdR,\
     ISyncd**,\
     LwSyncd**, Syncd**, ACLwSyncdR*,\
     ACSyncdR*, ABCLwSyncdRW,ABCSyncdRW, BCLwSyncd*W, BCSyncd*W,\
     Rfe,[Rfi,DpdR],[Rfi,CtrldR],\
     [DpAddrdW,Wsi],[DpDatadW,Wsi],[DpAddrdR,Fri]"
  let safe ="Wse,Fre" 
  let safe_conform =
    "Fre, Wse, DpAddrdR,DpAddrdW,DpDatadW, DpCtrlIsyncdR,DpCtrldW,\
     Syncd**, ACSyncdR*, BCSyncd*W, ABCSyncdRW,\
     LwSyncdWW, LwSyncdR*,\
     ACLwSyncdRW, BCLwSyncdRW,\
     [DpAddrdW,Wsi],[DpDatadW,Wsi],[DpAddrdR,Fri]"
end

module X86 = struct
  module X = X86Arch.Make(SymbConstant)
  module A = AutoArch.Make(X)
  let testing = "Rfe,Pod**,MFenced**,[Rfi,MFencedR*],[Rfi,PodR*]"
  let safe = "Fre,Wse"
  let safe_conform = "Rfe,Fre,Wse,PodR*,PodWW,MFencedWR"
end

module MIPS = struct
  module X = MIPSArch.Make(SymbConstant)
  module A = AutoArch.Make(X)
  let testing = "Rfe,Pod**,Syncd**,[Rfi,SyncdR*],[Rfi,PodR*]"
  let safe = "Fre,Wse"
  let safe_conform = "Rfe,Fre,Wse,PodR*,PodWW,SyncdWR"
end

module ARM = struct
  module P = ARMArch.Make(SymbConstant)
  module A = AutoArch.Make(P)
  let testing =
    "Pod**, DpAddrdR,DpAddrdW, DpDatadW,\
     DpCtrldW,DpCtrldR,CtrldR,\
     DMBd**,\
     ACDMBdR*, ABCDMBdRW, BCDMBd*W,\
     DSBd**,\
     ACDSBdR*, ABCDSBdRW, BCDSBd*W,\
     Rfe,[Rfi,DpdR],[Rfi,CtrldR],\
     [DpAddrdW,Wsi],[DpDatadW,Wsi],[DpAddrdR,Fri]"
  let safe ="Wse,Fre" 
  let safe_conform =
    "Fre, Wse, DpdR,DpdW, CtrldR,CtrldW,DpDatadW,\
     DMBd**, ACDMBdR*, BCDMBd*W, ABCDMBdRW,\
     DSBd**, ACDSBdR*, BCDSBd*W, ABCDSBdRW,\
     [DpAddrdW,Wsi],[DpDatadW,Wsi],[DpAddrdR,Fri]"
end

module type InitialSets = sig 
  val testing : string val safe : string
end

module type ArchConf = sig
  module A : AutoArch.S
  include InitialSets
  val safe_conform : string
end

let get_arch a =
  let open Archs in
  match a with
  | X86 -> (module X86 : ArchConf)
  | PPC -> (module PPC : ArchConf)
  | ARM -> (module ARM : ArchConf)
  | MIPS -> (module MIPS : ArchConf)
  | AArch64|C|CPP -> Warn.fatal "architecture %s not implemented" (Archs.pp a)

open AutoOpt 

module type B = sig
  val mode : mode
  val mach : mach
  val nprocs : int
  val diy_sz : int
  val litmus_opts : string
  val my_dir : string
  val dist_dir : string
  val run_opts : string list
  val verbose : int
  val interactive : bool
  val build : string
end

let copy_b opt =
  let module B = struct
    let mode = opt.mode
    let mach = opt.mach
    let nprocs = get_nprocs opt.arch opt
    let diy_sz = match opt.diy_sz with Some n -> n | None -> 2*nprocs
    let litmus_opts = opt.litmus_opts
    let my_dir = opt.output
    let dist_dir = opt.work_dir
    let run_opts = opt.run_opts
    let verbose = opt.verbose
    let interactive = opt.interactive
    let build = opt.build
end in
  (module B : B)

module type T = sig

  module A : AutoArch.S
  val testing : string
  val safe : string

  include B
end

module type S = sig
  include T

  module I : AutoInterpret.S
      with type outcome = A.L.outcome
      and type relax = A.R.relax
      and type relax_set = A.R.Set.t
      and type count = int A.R.Map.t

  val opt : AutoOpt.t
end

module MakeInitialSets
    (I:
       sig
         val opt : AutoOpt.t
         val testing : string
         val safe : string
         val safe_conform : string
       end) =
  struct

    let testing = match I.opt.testing with
    | None -> I.testing
    | Some tst -> tst

    let safe = match I.opt.safe with
    | None ->
        begin match I.opt.mode with
        | Explo -> I.safe
        | Conform -> I.safe_conform
        end
    | Some safe -> safe
  end


let mk_config opt =
  let module X = struct
    include (val (get_arch opt.arch) : ArchConf)        
    include
        (MakeInitialSets
           (struct
             let opt = opt
             let testing = testing
             let safe = safe
             let safe_conform = safe_conform
           end))        
    include (val (copy_b opt): B)  end in
  match opt.interpretation with
  | Single ->
      let module C = struct
        include X
        module I = AutoSingle.Make(A)            
        let opt = opt
      end in
      (module C : S)
  | Multi ->
      let module C = struct
        include X
        module I = AutoMulti.Make(A)            
        let opt = opt
      end in
      (module C : S)
        
