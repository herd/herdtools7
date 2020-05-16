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

open Misc
open Printf

(* Configuration *)
let use_eieio = ref true
let norm = ref false

let () = Config.addnum := false
let () = Config.numeric := false
let () = Config.nprocs := 1000

let opts =
  Config.common_specs @
  ("-num", Arg.Bool (fun b -> Config.numeric := b),
   sprintf "<bool> use numeric names, default %b" !Config.numeric)::
  ("-noeieio", Arg.Clear use_eieio,
   " ignore eieio fence (backward compatibility)")::
  Config.varatomspec::[]




module type Config = sig  
  include DumpAll.Config
  val varatom : string list
  val unrollatomic : int option
end

module Make (Config:Config) (M:Builder.S) =
  struct
    module D = DumpAll.Make (Config) (M)
    open M.E

    let norm = match Config.family with
    | None -> true
    | Some _ -> false

    let gen ess kont r =
      Misc.fold_cross ess
        (fun es r ->
          let es = List.flatten es in
          kont es D.no_info D.no_name D.no_scope r)
        r

    open Code

    let er e = M.R.ERS [plain_edge e]

    let all_fences sd d1 d2 =
      M.A.fold_all_fences
        (fun f k -> er (M.E.Fenced (f,sd,Dir d1,Dir d2))::k)

    let some_fences sd d1 d2 =
      M.A.fold_some_fences
        (fun f k -> er (M.E.Fenced (f,sd,Dir d1,Dir d2))::k)
        
(* Limited variations *)
    let app_def_dp o f r = match o with
    | None -> r
    | Some dp -> f dp r

    let someR sd d =
      er (Po (sd,Dir R,Dir d))::
      app_def_dp
        (match d with R|J -> M.A.ddr_default | W -> M.A.ddw_default)
        (fun dp k -> er (Dp (dp,sd,Dir d))::k)
        (some_fences sd R d [])      

    let someW sd d =
      er (Po (sd,Dir W,Dir d))::
      (some_fences sd W d [])      

        
(* ALL *)
    let allR sd d =
      er (Po (sd,Dir R,Dir d))::
	      (match d with R|J -> M.A.fold_dpr | W -> M.A.fold_dpw)
        (fun dp k -> er (Dp (dp,sd,Dir d))::k)
        (all_fences sd R d [])      

    let allW sd d =
      er (Po (sd,Dir W,Dir d))::
      (all_fences sd W d [])      

    let parse_relaxs s = match s with
    | "allRR" -> allR Diff R
    | "allRW" -> allR Diff W
    | "allWR" -> allW Diff R
    | "allWW" -> allW Diff W
    | "someRR" -> someR Diff R
    | "someRW" -> someR Diff W
    | "someWR" -> someW Diff R
    | "someWW" -> someW Diff W
    | _ ->
        let es = LexUtil.split s in
        List.map M.R.parse_relax es

    let parse_edges s =
(*      let rs = parse_relaxs s in *)
      let rs = M.R.expand_relax_macros (LexUtil.split s) in
      List.fold_right (fun r k -> M.R.edges_of r :: k) rs []



    let varatom_ess =
      if M.A.bellatom then Misc.identity
      else match Config.varatom with
      | [] -> fun ess -> ess
      | ["all"] ->
          let module Fold = struct
            type atom = M.E.atom
            let fold = M.E.fold_atomo
          end in
          let module V = VarAtomic.Make(M.E)(Fold)  in
          List.map V.varatom_es
      | atoms ->
          let atoms = M.E.parse_atoms atoms in
          let module Fold = struct
            type atom = M.E.atom
            let fold f k = M.E.fold_atomo_list atoms f k
          end in
          let module V = VarAtomic.Make(M.E)(Fold)  in
          List.map V.varatom_es

    let expand_edge es = M.E.expand_edges es Misc.cons
    let expand_edges ess =
      List.flatten (List.map (fun e -> expand_edge e []) ess)

    let zyva pp_rs =
      try
        let ess = List.map parse_edges pp_rs in
        let ess = List.map expand_edges ess in
        let ess = varatom_ess ess in
        D.all (gen ess)
      with Fatal msg ->
        eprintf "Fatal error: %s\n" msg ;
        exit 2
  end

let pp_es = ref []

let () =
  Util.parse_cmdline
    opts
    (fun x -> pp_es := x :: !pp_es)

let pp_es = List.rev !pp_es

let () =
  try
    let module C = struct
(* Dump all *)
      let verbose = !Config.verbose
      let generator = Config.baseprog
      let debug = !Config.debug
      let hout = match !Config.hout with
      | None -> Hint.none
      | Some n -> Hint.open_out n
      let family = !Config.name
      let canonical_only = !Config.canonical_only
      let fmt = !Config.fmt
      let no = match !Config.no with
      | None -> []
      | Some fname -> Config.read_no fname
      let cond = !Config.cond
      let tarfile = !Config.tarfile
      let sufname = !Config.sufname
      let addnum = !Config.addnum
      let numeric = !Config.numeric
     let lowercase = !Config.lowercase
(* Specific *)
      let varatom = !Config.varatom
      let same_loc =
         !Config.same_loc ||
         (match cond with
         | Config.Unicond -> true
         | _ -> false)
      let unrollatomic = !Config.unrollatomic
      let show = !Config.show
      let overload = !Config.overload
      let poll = !Config.poll
      let docheck = !Config.docheck
      let optcoherence = !Config.optcoherence
      let optcond = !Config.optcond
      let obs_type = !Config.obs_type
      let do_observers = !Config.do_observers
      let eprocs = !Config.eprocs
      let nprocs = !Config.nprocs
      let neg = !Config.neg
      let allow_back = true
      let typ = !Config.typ
      let hexa = !Config.hexa
      let moreedges = !Config.moreedges
      let realdep = !Config.realdep
      let cpp = match !Config.arch with `CPP -> true | _ -> false
      let scope = !Config.scope
      let variant = !Config.variant
      let addret = !Config.addret
    end in
    let module T = Top_gen.Make(C) in
    begin match !Config.arch with
    | `X86 ->
        let module M = Make(C)(T(X86Compile_gen.Make(C))) in
        M.zyva
    | `X86_64 ->
        let module M = Make(C)(T(X86_64Compile_gen.Make(C))) in
        M.zyva
    | `PPC -> 
        let module PPCConf = struct let eieio = !use_eieio end in
        let module M = Make(C)(T(PPCCompile_gen.Make(C)(PPCConf))) in
        M.zyva
    | `ARM ->
        let module M = Make(C)(T(ARMCompile_gen.Make(C))) in
        M.zyva
    | `AArch64 ->
        let module M = Make(C)(T(AArch64Compile_gen.Make(C))) in
        M.zyva
    | `MIPS ->
        let module M = Make(C)(T(MIPSCompile_gen.Make(C))) in
        M.zyva
    | `RISCV ->
        let module M = Make(C)(T(RISCVCompile_gen.Make(C))) in
        M.zyva
    | `LISA ->
        let module BellConfig =
        struct
          let debug = !Config.debug
          let verbose = !Config.verbose
          let libdir = Version_gen.libdir
          let prog = Config.prog
          let bell = !Config.bell
          let varatom = !Config.varatom
        end in
        let module M = Make(C)(T(BellCompile.Make(C)(BellConfig))) in
        M.zyva
    | `C | `CPP ->
        let module CoC = struct
          include C
          let typ = !Config.typ
          let novolatile = !Config.variant Variant_gen.NoVolatile
        end in
        let module T = CCompile_gen.Make(CoC) in
        let module M = Make(C)(T) in
        M.zyva
    end pp_es
        with
        | Misc.Exit -> ()
        | (Misc.Fatal msg|Misc.UserError msg) ->
            eprintf "%s: Fatal error: %s\n" Config.prog msg

