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
  val cumul :   Ast.t list Config.cumul
  val max_ins : int
  val upto : bool
  val varatom : string list
end

module Make(C:Builder.S)(O:DiyConfig) = struct

open C.E
open C.R
open Ast

let parse_fence s k =  match s with
  | One f -> C.E.parse_fence f::k
  | Seq [] -> k
  | Seq (fs) ->
      List.fold_right
        (fun s k ->
          match s with
          | One s -> C.E.parse_fence s::k
          | _ -> assert false)
        fs k
  | Choice _ -> assert false
  | Multi _ -> assert false

let parse_fences fs = List.fold_right parse_fence fs []

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

  (* Parse a string such as "[Po DpAddr] Fre"
     which is "[Po,DpAddr]|Fre" in canonical form.
     That is the top level white space become Choice.
     The result is a list of relaxation that has been unfolded.
     Each relaxation contains, wrapped in `ERS`,
     one or several edges in sequence. *)
  let parse_argument input_argument =
    String.trim input_argument
    |> (fun s -> Lexing.from_string s)
    |> Parser.main LexUtil.token
    (* Manually convert the top level to Choice *)
    |> ( function
      | Seq sq -> Choice sq
      | ast -> ast
    )
    |> Ast.flatten
    |> List.map (parse_expand_relaxs ~ppo:C.ppo)
    |> List.flatten
    |> List.map edges_of
    |> varatom_ess
    |> List.map ( fun edges -> ERS edges )

  module AltConfig = struct
    include O

    type relax = C.R.relax
    let mix = !Config.mix
    let max_relax = !Config.max_relax
    let min_relax = !Config.min_relax

    let prefix =
      List.map ( fun segment ->
        String.trim segment
        |> (fun s -> Lexing.from_string s)
        |> Parser.main LexUtil.token
      ) O.prefix
      |> fun e -> Ast.Choice e
      |> Ast.flatten
      |> List.map (parse_expand_relaxs ~ppo:C.ppo)
      |> List.flatten
      |> List.map (fun l -> [l])
      |> ( function
        | [] -> [[]] (* No prefix <=> one empty prefix *)
        | pss -> pss
      )

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
    let parse_argument_opt argument =
      Option.map parse_argument argument
      |> Option.value ~default:[] in
    let lr = parse_argument_opt lr
    and ls = parse_argument_opt ls
    and rl = parse_argument_opt rl in
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
        | None,None -> M.gen n
        | _ -> gen olr ols orl n
        end
    | Thin -> gen_thin n
    | Uni ->
        begin match olr,ols with
        | None,None -> gen_uni n
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

let split_cands xs =
  if xs = [] then None
  else (Some (String.concat " " xs))


let () =
  Arg.parse (Config.diy_spec ()) get_arg Config.usage_msg;
  begin
  match !Config.conf with
  | None -> ()
  | Some s -> exec_conf s
  end;
  Config.valid_stdout_flag false ;
  let relax_list = split_cands !Config.relaxs
  and safe_list = split_cands !Config.safes
  and reject_list = !Config.rejects in


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
    let cumul = match !Config.cumul with
    | Empty -> Empty
    | All -> All
    | Set s -> Set ( Lexing.from_string s
                     |> Parser.main LexUtil.token
                     |> Ast.to_list )
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
  | `JAVA | `ASL | `BPF -> assert false
  in
  try
    go !Config.size reject_list relax_list safe_list ;
    exit 0
  with
  | Misc.Fatal msg | Misc.UserError msg->
    eprintf "%s: Fatal error: %s\n" Config.prog msg ;
    exit 2
