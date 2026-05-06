(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf
open Code

module type DiyConfig = sig
  include DumpAll.Config
  val choice : Code.check
  val variant : Variant_gen.t -> bool
  val prefix : string list
  val cumul : string Config.cumul
  val max_ins : int
  val upto : bool
  val varatom : string list
end

module Make(C:Builder.S)(O:DiyConfig) = struct

open C.E
open C.R

  let parse_argument_ast input =
    String.trim input |> parse_ast Parser.main_top_level_choice

  (* Parse the `-cumul` argument, which should be a list of individual fences. *)
  let parse_fences input_fences =
    let ast = String.trim input_fences |> parse_ast Parser.cumul in
    (* Note that `parse_fence` might fail *)
    let fences = Ast.bind ast ( fun input -> Ast.One(C.E.parse_fence input) )
    |> Ast.expand ( fun _ -> Warn.user_error "cumul input: %s contains predicate." input_fences ) in
    (* Each expanded alternative must contain exactly one fence. *)
    if List.for_all ( function [_] -> true | _ -> false ) fences then
      List.flatten fences
    else Warn.user_error "%s is not a list of fence" input_fences

  let varatom_ess =
    if C.A.bellatom then Misc.identity
    else match O.varatom with
    | [] -> Misc.identity
    | ["all"] ->
        let module Fold = struct
          type atom = C.E.atom
          let fold = C.E.fold_atomo
        end in
        let module V = VarAtomic.Make(C.E)(Fold) in
        V.varatom_es
    | atoms ->
        let atoms = C.E.parse_atoms atoms in
        let module Fold = struct
          type atom = C.E.atom
          let fold f k = C.E.fold_atomo_list atoms f k
        end in
        let module V = VarAtomic.Make(C.E)(Fold) in
        V.varatom_es

  (* Parse an input relaxation expression such as "[Po DpAddr] Fre".
     In canonical form this is "[Po,DpAddr]|Fre": top-level whitespace denotes
     a choice for backward compatibility. The result is
     a list of unfolded relaxations, each wrapped in `ERS` and containing one
     or more edges in sequence. *)
  let parse_argument input_argument =
    parse_argument_ast input_argument
    |> parse_expand_relaxs ~ppo:C.ppo
    |> List.map edges_of
    |> varatom_ess
    |> List.map ( fun edges -> ERS edges )
    |> remove_invalid_relaxes

  let parse_argument_list input_argument_list =
    List.map parse_argument input_argument_list
    |> List.flatten
    |> remove_invalid_relaxes

  module AltConfig = struct
    include O

    type relax = C.R.relax
    let mix = !Config.mix
    let max_relax = !Config.max_relax
    let min_relax = !Config.min_relax

    let prefix =
      (* Parse each `-prefix` argument separately, then combine them as one
         top-level choice. Thus `-prefix A -prefix B` is interpreted as
         `-prefix [A|B]`. *)
      let prefixes =
        List.map parse_argument_ast O.prefix
        |> fun prefixes -> Ast.Choice prefixes
        |> parse_expand_relaxs ~ppo:C.ppo
        (* Wrap each relax into a list *)
        |> List.map (fun prefix -> [prefix]) in
      match prefixes with
      | [] -> [[]] (* No prefix <=> one empty prefix *)
      | prefixes -> prefixes

    let variant = O.variant

    type fence = C.A.fence

    let cumul =
      let open Config in
      match O.cumul with
      | Empty -> Empty
      | All -> All
      | Set ps -> Set (parse_fences ps)

    let wildcard = true
  end


  module M =  Alt.Make(C)(AltConfig)

  let gen lr ls rl n =
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

  let go n (*size*) orl olr ols (*relax and safe lists*) =
    match O.choice with
    | Default|Sc|Critical|Free|Ppo|Transitive|Total|MixedCheck ->
        begin match olr,ols with
        | [],[] -> M.gen n
        | _ -> gen olr ols orl n
        end
    | Thin -> gen_thin n
    | Uni ->
        begin match olr,ols with
        | [],[] -> gen_uni n
        | _ -> gen olr ols orl n
        end
end

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

let () =
  Util.parse_cmdline
    ~usage_suffix:Config.parser_syntax_doc
    (Config.diy_spec ())
    get_arg;
  begin
  match !Config.conf with
  | None -> ()
  | Some s -> exec_conf s
  end;
  Config.valid_stdout_flag false ;

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
    let metadata = !Config.metadata
(* Specific *)
    open Config
    let choice = !Config.mode
    let prefix = !Config.prefix
    let variant = !Config.variant
    let cumul = !Config.cumul
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
    let same_loc = !Config.same_loc
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
    let wildcard = true
  end in
  let module T = Top_gen.Make(Co) in
  let builder = match !Config.arch with
  | `PPC -> (module T(PPCCompile_gen.Make(C)(PPCArch_gen.Config)) : Builder.S)
  | `X86 -> (module T(X86Compile_gen.Make(C)) : Builder.S)
  | `X86_64 -> (module T(X86_64Compile_gen.Make(C)) : Builder.S)
  | `ARM -> (module T(ARMCompile_gen.Make(C)) : Builder.S)
  | `AArch64 -> (module T(AArch64Compile_gen.Make(C)) : Builder.S)
  | `MIPS -> (module T(MIPSCompile_gen.Make(C)) : Builder.S)
  | `RISCV -> (module T(RISCVCompile_gen.Make(C)) : Builder.S)
  | `LISA ->
      let module BellConfig = Config.ToLisa(Config) in
      (module T(BellCompile.Make(C)(BellConfig)) : Builder.S)
  | `C | `CPP ->
      let module CoC = struct
        include Co
        include C
        let typ = !Config.typ
      end in
      (module CCompile_gen.Make(CoC) : Builder.S)
  | `JAVA | `ASL | `BPF -> assert false
  in
  let module Builder = (val builder : Builder.S) in
  let module M = Make(Builder)(Co) in
  try
    (* Parse inputs `relax` `safe` and `reject` *)
    let relax = M.parse_argument_list !Config.relaxs in
    let safe = M.parse_argument_list !Config.safes in
    let reject = M.parse_argument_list !Config.rejects in
    match !Config.filter_check with
    | [lhs;rhs] ->
        let lhs_unfold = M.parse_argument lhs in
        let rhs_unfold = M.parse_argument rhs in
        List.map ( fun l ->
          List.map ( fun r ->
            l,r,M.M.filter_check ~relax ~safe (Builder.R.edges_of l) (Builder.R.edges_of r)
          ) rhs_unfold
        ) lhs_unfold
        |> List.flatten
        |> List.iter ( fun (l, r, result) ->
            printf "Sequence `%s` `%s` %s the internal filter in mode `%s`\n"
            (Builder.R.pp_relax l) (Builder.R.pp_relax r)
            ( if result then "passes" else "is prohibited in" )
            ( Code.pp_check Co.choice )
        )
    | _ -> (* The common path to generate tests *)
      if !Config.unfold_only then
        eprintf "***relax***\n%s\n***safe***\n%s\n***reject***\n%s\n"
        (Builder.R.pp_relax_list relax) (Builder.R.pp_relax_list safe) (Builder.R.pp_relax_list reject)
      else
        M.go !Config.size reject relax safe;
    exit 0
  with
  | Misc.Fatal msg ->
    eprintf "%s: Fatal error: %s\n" Config.prog msg ;
    exit 2
  | Misc.UserError msg->
    eprintf "%s: %s\n" Config.prog msg ;
    exit 2
