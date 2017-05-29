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

open Printf
open Code
open LexUtil
open Archs

module type DiyConfig = sig
  include DumpAll.Config
  val choice : Code.check
  val prefix : LexUtil.t list option
  val cumul :   LexUtil.t list Config.cumul
  val max_ins : int
  val upto : bool
  val varatom : string list
end

module Make(C:Builder.S)(O:DiyConfig) = struct

open C.E
open C.R

let parse_fence s k =  match s with
  | One f -> C.E.parse_fence f::k
  | Seq [] -> k
  | Seq (fs) ->
      List.fold_right
        (fun s k -> C.E.parse_fence s::k)
        fs k

let parse_relaxs = List.map parse_relax
let parse_edges = List.map parse_edge
let parse_fences fs = List.fold_right parse_fence fs []
  

  module AltConfig = struct
    include O

    type relax = C.R.relax

    let prefix = match O.prefix with
    | None -> []
    | Some ps -> parse_relaxs ps

    type fence = C.A.fence

    let cumul =
      let open Config in
      match O.cumul with
      | Empty -> Empty
      | All -> All
      | Set ps -> Set (parse_fences ps)

  end


  module M =  Alt.Make(C)(AltConfig)

  let var_relax fold rs = function
    | PPO as r -> r::rs
    | ERS es ->
        let ess = fold es in
        List.fold_left
          (fun k es -> ERS es::k) rs ess


  let var_atom = 
    if C.A.bellatom then Misc.identity
    else match O.varatom with
    | [] -> Misc.identity
    | ["all"] ->
        let module Fold = struct
          type atom = C.E.atom
          let fold = C.E.fold_atomo
        end in
        let module V = VarAtomic.Make(C.E)(Fold) in
        List.fold_left
          (var_relax V.varatom_one) []
    | atoms ->
        let atoms = C.E.parse_atoms atoms in
        let module Fold = struct
          type atom = C.E.atom
          let fold f k = C.E.fold_atomo_list atoms f k
        end in
        let module V = VarAtomic.Make(C.E)(Fold) in
        List.fold_left
          (var_relax V.varatom_one) []

  let gen lr ls n =
    let lr = C.R.expand_relax_macros lr
    and ls = C.R.expand_relax_macros ls in
    let lr = var_atom lr
    and ls = var_atom ls in
    M.gen ~relax:lr ~safe:ls n

  let er e = ERS [plain_edge e]

  let gen_thin n =
    let lr = [er (Rf Int); er (Rf Ext)]
    and ls = [PPO] in
    M.gen ~relax:lr ~safe:ls n


  let gen_uni n =
    let lr = [er (Rf Int); er (Rf Ext)]
    and ls =
      [er (Ws Int); er (Ws Ext); er (Fr Int);
       er (Fr Ext); er (Po (Same,Irr,Irr))] in
    M.gen ~relax:lr ~safe:ls n


  let parse_edges_opt = function
    | None -> None
    | Some xs -> Some (parse_edges xs)

  let go n (*size*) olr ols (*relax and safe lists*) _olc (*one + arg*) =
    match O.choice with
    | Sc|Critical|Free|Ppo|Transitive|Total|MixedCheck ->
        begin match olr,ols with
        | None,None -> M.gen n
        | None,Some ls -> gen [] ls n
        | Some lr,None -> gen lr [] n
        | Some lr,Some ls -> gen lr ls n
        end
    | Thin -> gen_thin n 
    | Uni ->
        begin match olr,ols with
        | None,None -> gen_uni n
        | None,Some ls -> gen [] ls n
        | Some lr,None -> gen lr [] n
        | Some lr,Some ls -> gen lr ls n
        end
end


let split s = match s with
  | None -> None
  | Some s ->
  let splitted = LexUtil.split s in
  Some splitted

let lc = ref []

let get_arg s =
  lc := (!lc)@[s]

let norm_cmd cmd = 
  match cmd with
  | [] -> assert false
  | _::s -> 
    let rec no_conf l = 
      match l with
      | [] -> []
      | "-conf"::_::l -> no_conf l
      | s::l -> s::(no_conf l)
    in no_conf s

let exec_conf s =
  let conf = Misc.input_protect LexConf_gen.conf s in
  let prog = Sys.argv.(0) in
  let cmd = Array.to_list Sys.argv in
  let cmd = norm_cmd cmd in
  if !Config.verbose > 1 then
    eprintf "EXEC: %s %s\n%!" prog (String.concat " " (conf @ cmd)) ;
  ignore (Unix.execvp prog (Array.of_list (prog::conf@cmd))) ;
  ()

let speclist =
  Config.speclist @ [Config.varatomspec]

let () =
  Arg.parse speclist get_arg Config.usage_msg;
  begin
  match !Config.conf with
  | None -> ()
  | Some s -> exec_conf s
  end;
  let relax_list = split !Config.relaxs
  and safe_list = split !Config.safes 
  and one_list = if !Config.one then Some !lc else None in

  let cpp = match !Config.arch with `CPP -> true  |  _ -> false in

  let module Co = struct
(* Dump all *)
    let verbose = !Config.verbose
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
    open Config
    let choice = !Config.mode
    let prefix =  split !Config.prefix
    let cumul = match !Config.cumul with
    | Empty -> Empty
    | All -> All
    | Set s -> Set (LexUtil.split s)
    let coherence_decreasing = !Config.coherence_decreasing
    let upto = !Config.upto
    let varatom = !varatom
    let max_ins = !Config.max_ins
    let overload = !Config.overload
    let poll = !Config.poll
    let optcoherence = !Config.optcoherence
    let optcond = !Config.optcond
    let obs_type = !Config.obs_type
    let do_observers = !Config.do_observers
    let eprocs = !Config.eprocs
    let nprocs = !Config.nprocs
    let neg = !Config.neg
    let cpp = cpp
    let scope = !scope
    let docheck = !Config.docheck
    let typ = !Config.typ
    let hexa = !Config.hexa
 end in
  let module C = struct
    let verbose = !Config.verbose
    let list_edges = !Config.list_edges
    let coherence_decreasing = !Config.coherence_decreasing 
    let same_loc =
      !Config.same_loc ||
      (match Co.choice  with Uni -> true | _ -> false)
    let unrollatomic = !Config.unrollatomic
    let allow_back = match !Config.mode with
    | Sc|Critical|Thin -> false
    | _ -> true
    let typ = !Config.typ
    let hexa = !Config.hexa
    let moreedges = !Config.moreedges
    let realdep = !Config.realdep
  end in
  let module T = Top_gen.Make(Co) in
  let f = match !Config.arch with
  | `PPC ->
      let module M = Make(T(PPCCompile_gen.Make(C)(PPCArch_gen.Config)))(Co) in
    M.go
  | `X86 ->
    let module M = Make(T(X86Compile_gen.Make(C)))(Co) in
    M.go
  | `ARM ->
      let module M = Make(T(ARMCompile_gen.Make(C)))(Co) in
      M.go
  | `AArch64 ->
      let module M = Make(T(AArch64Compile_gen.Make(C)))(Co) in
      M.go
  | `MIPS ->
      let module M = Make(T(MIPSCompile_gen.Make(C)))(Co) in
      M.go
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
      let module M = Make(T(BellCompile.Make(C)(BellConfig)))(Co) in
      M.go
  | `C | `CPP ->
      let module CoC = struct
        include Co
        include C
        let typ = !Config.typ
      end in
      let module M = Make(CCompile_gen.Make(CoC))(Co) in
      M.go in
  try
    f !Config.size relax_list safe_list one_list ;
    exit 0
  with
  | Misc.Fatal msg | Misc.UserError msg->
    eprintf "%s: Fatal error: %s\n" Config.prog msg ;
    exit 2
