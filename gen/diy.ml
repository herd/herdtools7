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

module type DiyConfig = sig
  include DumpAll.Config
  val choice : Code.check
  val variant : Variant_gen.t -> bool
  val prefix : LexUtil.t list list
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
    let mix = !Config.mix
    let max_relax = !Config.max_relax
    let min_relax = !Config.min_relax

    let prefix =
      match List.map parse_relaxs O.prefix with
      | [] -> [[]] (* No prefix <=> one empty prefix *)
      | pss -> pss

    let variant = O.variant

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

  let gen lr ls rl n =
    let lr = C.R.expand_relax_macros lr
    and ls = C.R.expand_relax_macros ls
    and rl = C.R.expand_relax_macros rl in
    let lr = var_atom lr
    and ls = var_atom ls
    and rl = var_atom rl in
    if O.verbose > 0 then begin
      Printf.eprintf
        "expanded relax=%s\n" (C.R.pp_relax_list lr)
    end ;
    M.gen ~relax:lr ~safe:ls ~reject:rl n

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

  let orl_opt = function
    | None -> []
    | Some xs -> xs

  let go n (*size*) orl olr ols (*relax and safe lists*) =
    let orl = orl_opt orl in
    match O.choice with
    | Default|Sc|Critical|Free|Ppo|Transitive|Total|MixedCheck ->
        begin match olr,ols with
        | None,None -> M.gen n
        | None,Some ls -> gen [] ls orl n
        | Some lr,None -> gen lr [] orl n
        | Some lr,Some ls -> gen lr ls orl n
        end
    | Thin -> gen_thin n
    | Uni ->
        begin match olr,ols with
        | None,None -> gen_uni n
        | None,Some ls -> gen [] ls orl n
        | Some lr,None -> gen lr [] orl n
        | Some lr,Some ls -> gen lr ls orl n
        end
end


let split s = match s with
  | None -> None
  | Some s ->
  let splitted = LexUtil.split s in
  Some splitted

let get_arg s =
  raise (Arg.Bad (Printf.sprintf "%s takes no argument, argument %s is present" Config.prog s))

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
  Config.speclist () @ [Config.varatomspec]

let split_cands xs =
  match
    List.concat (List.map LexUtil.split xs)
  with
  | [] -> None
  | _::_ as xs -> Some xs

let () =
  Arg.parse speclist get_arg Config.usage_msg;
  begin
  match !Config.conf with
  | None -> ()
  | Some s -> exec_conf s
  end;
  let relax_list = split_cands !Config.relaxs
  and safe_list = split_cands !Config.safes
  and reject_list = split !Config.rejects in

  let () =
    if !Config.verbose > 0 then begin
      let relaxs =
        match relax_list with
        | None -> []
        | Some xs -> xs in
      Printf.eprintf "parsed relax=%s\n"
        (String.concat " " (List.map LexUtil.pp relaxs))
    end in
  let cpp = match !Config.arch with `CPP -> true  |  _ -> false in

  let module Co = struct
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
    let stdout = if !Config.cycleonly then true else !Config.stdout
    let cycleonly = !Config.cycleonly
(* Specific *)
    open Config
    let choice = !Config.mode
    let prefix =  List.rev_map LexUtil.split !Config.prefix
    let variant = !Config.variant
    let cumul = match !Config.cumul with
    | Empty -> Empty
    | All -> All
    | Set s -> Set (LexUtil.split s)
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
    let info = !Config.info
    let docheck = !Config.docheck
    let typ = !Config.typ
    let hexa = !Config.hexa
 end in
  let module C = struct
    let verbose = !Config.verbose
    let show = !Config.show
    let same_loc =
      !Config.same_loc ||
      (match Co.choice  with Uni -> true | _ -> false)
    let unrollatomic = !Config.unrollatomic
    let allow_back = match !Config.mode with
    | Default|Sc|Critical|Thin -> false
    | _ -> true
    let typ = !Config.typ
    let hexa = !Config.hexa
    let moreedges = !Config.moreedges
    let realdep = !Config.realdep
    let variant = !Config.variant
  end in
  let module T = Top_gen.Make(Co) in
  let go = match !Config.arch with
  | `PPC ->
      let module M = Make(T(PPCCompile_gen.Make(C)(PPCArch_gen.Config)))(Co) in
    M.go
  | `X86 ->
    let module M = Make(T(X86Compile_gen.Make(C)))(Co) in
    M.go
  | `X86_64 ->
    let module M = Make(T(X86_64Compile_gen.Make(C)))(Co) in
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
  | `RISCV ->
      let module M = Make(T(RISCVCompile_gen.Make(C)))(Co) in
      M.go
  | `LISA ->
      let module BellConfig = Config.ToLisa(Config) in
      let module M = Make(T(BellCompile.Make(C)(BellConfig)))(Co) in
      M.go
  | `C | `CPP ->
      let module CoC = struct
        include Co
        include C
        let typ = !Config.typ
      end in
      let module M = Make(CCompile_gen.Make(CoC))(Co) in
      M.go 
  | `JAVA | `ASL -> assert false 
  in
  try
    go !Config.size reject_list relax_list safe_list ;
    exit 0
  with
  | Misc.Fatal msg | Misc.UserError msg->
    eprintf "%s: Fatal error: %s\n" Config.prog msg ;
    exit 2
