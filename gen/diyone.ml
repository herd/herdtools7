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

open Misc
open Printf

module type Config = sig
  include Top_gen.Config
  include DumpAll.Config
  val norm : bool
  val cpp : bool
  val docheck : bool
end

module Make(O:Config) (M:Builder.S) =
  struct

    let dump_stdout t = M.dump_test_channel stdout t

    let litmus =
      if O.cpp then sprintf "%s.c"
      else sprintf "%s.litmus"

    module Tar =
      Tar.Make
        (struct
          let verbose = O.verbose
          let outname = O.tarfile
        end)

    let tar_output_protect t filename =
      if O.verbose > 0 then eprintf "File name: %s\n" filename ;
      Misc.output_protect
        (fun chan -> M.dump_test_channel chan t) (Tar.outname filename)

    let gen_one_scope gen n =
      try
        gen n
          (fun st r -> match r with
          | Some _ -> raise Exit
          | None -> Some st)
          None
      with Exit -> None

    let get_scope n = match O.scope with
    | Scope.No -> None
    | Scope.One st -> Some st
    | Scope.Default -> Some (M.A.ScopeGen.default n)
    | Scope.Gen scs ->
        begin match gen_one_scope (M.A.ScopeGen.gen scs) n with
        | None ->
            Warn.fatal
              "scope enumeration yields several scopes"
        | Some _ as st -> st
        end
    | Scope.All ->
        begin match gen_one_scope M.A.ScopeGen.all n with
        | None ->
            Warn.fatal
              "scope enumeration yield several scopes"
        | Some _ as st -> st
        end

    let dump name es =
	    let es = M.E.resolve_edges es in
      let module Normer = Normaliser.Make(O)(M.E) in
      (* Default scope *)
      let scope = Normer.get_nprocs es |> get_scope in
      (* normalised name and scope *)
      let normalised_name_scope es =
        let module Namer = Namer.Make(M.A)(M.A)(M.E) in
        let es = fst @@ M.C.resolve_edges es in
        let base,es,nprocs = Normer.normalise_family es in
        let scope = get_scope nprocs in
        let name = Namer.mk_name base ?scope es in
        name,scope in
      (* Decide the test name *)
      let test_name,scope = match O.norm,name with
        (* User specified name *)
        | false, Some name -> name,scope
        | true, _
        (* Default test name *)
        | false, None -> normalised_name_scope es in
      let test_name = test_name ^ (Option.value ~default:"" O.sufname) in
      (* generate test from `es` *)
      let t = M.make_test test_name ~info:O.info ?scope es in

      (* Output to a stdout or file *)
      if not O.stdout && ( O.norm || name <> None || O.tarfile <> None ) then begin
        tar_output_protect t (litmus test_name);
        Tar.tar()
      end else
        dump_stdout t


    module P = LineUtils.Make(M.E)

    let parse_line s = P.parse s


(********)
    let rec read_line_no_comment  () =
      let line = read_line () in
      if
        String.length line = 0 ||
        (String.length line > 0 && line.[0] = '#')
      then
        read_line_no_comment ()
      else line

(********)
    let do_zyva name_opt pp_rs =
      try begin
        let es = List.map LexUtil.split pp_rs
          |> List.concat
          |> List.map M.R.parse_relax
          |> List.map M.R.edges_of
          |> List.flatten in
        if O.verbose > 0 then
          Printf.eprintf
            "Parsed edges: %s\n" (M.E.pp_edges es) ;
        match es with
        | [] ->
            let dump_names =
              O.norm ||
              (match O.family with Some _ -> true | None -> false) in
            let module D = DumpAll.Make(O)(M) in
            let gen kont =
              let rec do_rec k0 =
                let k =
                  try
                    let line = read_line_no_comment () in
                    try
                      let name,es,st = parse_line line in
                      let mk_name =
                        if dump_names then D.no_name
                        else
                          let name =
                            match O.sufname with
                            | None -> name
                            | Some suf -> name ^ suf in
                          fun _ -> Some name in
                      let mk_scope _ = st in
                      Some (kont es D.no_info mk_name mk_scope k0)
                    with
                    | Fatal msg | UserError msg ->
                        Warn.warn_always "%s on line '%s'" msg line ;
                        Some k0
                  with
                  | End_of_file -> None in
                match k with
                | None -> k0
                | Some k -> do_rec k in
              do_rec in
            D.all gen
        | _ -> dump name_opt es
      end with Fatal msg ->
        eprintf "%s: Fatal error: %s\n" Config.prog msg ;
        exit 2

    let zyva = do_zyva O.family

  end

let pp_es = ref []

let () =
  Util.parse_cmdline
    (Config.diyone_spec ())
    (fun x -> pp_es := x :: !pp_es) ;
  (* stdout should only be used with norm or name *)
  if !Config.stdout && !Config.name = None && not !Config.norm then
    Warn.user_error "-stdout will be ignored."

let pp_es = List.rev !pp_es
let cpp = match !Config.arch with
| `CPP -> true
| _ -> false

let () =
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
    let optcoherence = !Config.optcoherence
    let optcond = !Config.optcond
    let poll = !Config.poll
    let overload = !Config.overload
    let obs_type = !Config.obs_type
    let do_observers = !Config.do_observers
    let eprocs = !Config.eprocs
    let nprocs = !Config.nprocs
    let neg = !Config.neg
    let typ = !Config.typ
    let hexa = !Config.hexa
    let stdout = if !Config.cycleonly then true else !Config.stdout
    let cycleonly = !Config.cycleonly
    let metadata = !Config.metadata
(* Specific *)
    let norm = !Config.norm
    let cpp = cpp
    let scope = !Config.scope
    let docheck = !Config.docheck
    let info = !Config.info
    let variant = !Config.variant
    let same_loc = !Config.same_loc
  end in
  let module Build = Make(Co) in
  let module C = struct
    let verbose = !Config.verbose
    let debug = !Config.debug
    let show = !Config.show
    let same_loc =
      !Config.same_loc ||
      (match Co.cond with
      | Config.Unicond -> true
      | _ -> false)
    let unrollatomic = !Config.unrollatomic
    let allow_back = true
    let typ = !Config.typ
    let hexa = !Config.hexa
    let moreedges = !Config.moreedges
    let realdep = !Config.realdep
    let variant = !Config.variant
    let wildcard = false
  end in
  (match !Config.arch with
  | `X86 ->
      let module T = Top_gen.Make(Co) in
      let module M = Build(T(X86Compile_gen.Make(C))) in
      M.zyva
  | `X86_64 ->
      let module T = Top_gen.Make(Co) in
      let module M = Build(T(X86_64Compile_gen.Make(C))) in
      M.zyva
  | `PPC ->
      let module T = Top_gen.Make(Co) in
      let module M = Build(T(PPCCompile_gen.Make(C)(PPCArch_gen.Config))) in
      M.zyva
  | `ARM ->
      let module T = Top_gen.Make(Co) in
      let module M = Build(T(ARMCompile_gen.Make(C))) in
      M.zyva
  | `AArch64 ->
      let module T = Top_gen.Make(Co) in
      let module M = Build(T(AArch64Compile_gen.Make(C))) in
      M.zyva
  | `MIPS ->
      let module T = Top_gen.Make(Co) in
      let module M = Build(T(MIPSCompile_gen.Make(C))) in
      M.zyva
  | `RISCV ->
      let module T = Top_gen.Make(Co) in
      let module M = Build(T(RISCVCompile_gen.Make(C))) in
      M.zyva
  | `LISA ->
      let module BellConfig = Config.ToLisa(Config) in
      let module T = Top_gen.Make(Co) in
      let module M = Build(T(BellCompile.Make(C)(BellConfig))) in
      M.zyva
  | `C | `CPP as a ->
      let module CoC = struct
        include Co
        include C
        let typ = !Config.typ
        let cpp = match a with `CPP -> true | _ -> false
      end in
      let module T = CCompile_gen.Make(CoC) in
      let module M = Build(T) in
      M.zyva
  | `JAVA | `ASL | `BPF -> assert false
)
    pp_es
