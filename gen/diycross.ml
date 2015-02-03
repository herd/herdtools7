(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Misc
open Printf

(* Configuration *)
let varatom = ref false
let use_eieio = ref true
let norm = ref false

let () = Config.addnum := false
let () = Config.numeric := false
let () = Config.nprocs := 1000

let opts =
  Config.common_specs @
  ("-num", Arg.Bool (fun b -> Config.numeric := b),
   sprintf "<bool> use numeric names, default %b" !Config.numeric)::
  ("-varatom", Arg.Unit (fun () ->  varatom := true),
   " include all atomic variations of relaxations")::
  ("-noeieio", Arg.Clear use_eieio,
   " ignore eieio fence (backward compatibility)")::[]




module type Config = sig  
  include DumpAll.Config
  val varatom : bool
  val sta : bool
  val unrollatomic : int option
end

module Make (Config:Config) (M:Builder.S) =
  struct
    module D = DumpAll.Make (Config) (M)
    open M.E

    let norm = match Config.family with
    | None -> true
    | Some _ -> false

    module Name = Namer.Make(M.A)(M.E)
    module Norm = Normaliser.Make(Config)(M.E)

    let gen ess kont r =
      Misc.fold_cross ess
        (fun es r -> kont (List.flatten es) D.no_info D.no_name r)
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
        (match d with R -> M.A.ddr_default | W -> M.A.ddw_default)
        (fun dp k -> er (Dp (dp,sd,Dir d))::k)
        (some_fences sd R d [])      

    let someW sd d =
      er (Po (sd,Dir W,Dir d))::
      (some_fences sd W d [])      

        
(* ALL *)
    let allR sd d =
      er (Po (sd,Dir R,Dir d))::
      (match d with R -> M.A.fold_dpr | W -> M.A.fold_dpw)
        (fun dp k -> er (Dp (dp,sd,Dir d))::k)
        (all_fences sd R d [])      

    let allW sd d =
      er (Po (sd,Dir W,Dir d))::
      (all_fences sd W d [])      

    let atoms_key = "atoms"
    let atoms_length = String.length atoms_key

    let parse_atoms s =
      if
        String.length s >= atoms_length &&
        String.sub s 0 atoms_length = atoms_key
      then
        let suf = String.sub s atoms_length (String.length s - atoms_length) in
        try Some (M.E.parse_edge suf)
        with _ -> None
      else None

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
        match parse_atoms s with
        | Some r ->
            let module V = VarAtomic.Make(M.E)  in
            let es = V.var_both r in
            List.map (fun r -> M.R.ERS [r]) es
            
        | None ->
            let es = LexUtil.split s in
            List.map M.R.parse_relax es

    let parse_edges s =
(*      let rs = parse_relaxs s in *)
      let rs = M.R.expand_relax_macros (LexUtil.split s) in
      List.fold_right (fun r k -> M.R.edges_of r :: k) rs []



    let varatom_ess =
      if Config.varatom then
        let module V = VarAtomic.Make(M.E)  in
        List.map V.varatom_es
      else
        fun ess -> ess

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
      let addnum = !Config.addnum
      let numeric = !Config.numeric
      let lowercase = !Config.lowercase
(* Specific *)
      let varatom = !varatom
      let coherence_decreasing = !Config.coherence_decreasing
      let same_loc =
         !Config.same_loc ||
         (match cond with
         | Config.Unicond -> true
         | _ -> false)
      let sta = !Config.sta
      let unrollatomic = !Config.unrollatomic
      let list_edges = !Config.list_edges
      let overload = !Config.overload
      let poll = !Config.poll
      let docheck = !Config.docheck
      let optcoherence = !Config.optcoherence
      let optcond = !Config.optcond
      let fno = !Config.fno
      let obs_type = !Config.obs_type
      let do_observers = !Config.do_observers
      let eprocs = !Config.eprocs
      let nprocs = !Config.nprocs
      let neg = !Config.neg
      let allow_back = false
      let cpp = match !Config.arch with Archs.CPP -> true | _ -> false
    end in
    let module V = SymbConstant in
    let open Archs in
    let module T = Top.Make(C) in
    begin match !Config.arch with
    | X86 ->
        let module M = Make(C)(T(X86Compile.Make(V)(C))) in
        M.zyva
    | PPC -> 
        let module PPCConf = struct let eieio = !use_eieio end in
        let module M = Make(C)(T(PPCCompile.Make(V)(C)(PPCConf))) in
        M.zyva
    | ARM ->
        let module M = Make(C)(T(ARMCompile.Make(V)(C))) in
        M.zyva
    | MIPS ->
        let module M = Make(C)(T(MIPSCompile.Make(V)(C))) in
        M.zyva
    | C|CPP ->
        let module CoC = struct
          include C
          let typ = !Config.typ
        end in
        let module T = CCompile.Make(CoC) in
        let module M = Make(C)(T) in
        M.zyva
    end pp_es
        with
        | Misc.Exit -> ()
        | (Misc.Fatal msg|Misc.UserError msg) ->
            eprintf "%s: Fatal error: %s\n" Config.prog msg

