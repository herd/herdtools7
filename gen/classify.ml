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

let arch = ref `PPC
let diyone = ref false
let lowercase = ref false
let uniq = ref false
let map = ref None
let bell = ref None

let opts =
  ("-diyone", Arg.Set diyone," generate input for diyone")::
  ("-lowercase", Arg.Bool (fun b -> lowercase := b),
   sprintf "<bool> use lowercase familly names, default %b" !lowercase)::
  ("-u", Arg.Set uniq," reject duplicate normalised names")::
  ("-map", Arg.String (fun s -> map := Some s)," <name> save renaming map into file <name>")::
  ("-bell",Arg.String (fun f -> bell := Some f; arch := `LISA),
   "<name> read bell file <name>")::
  Util.parse_tag
    "-arch"
    (fun tag -> match Archs.parse tag with
    | None -> false
    | Some a -> arch := a ; true)
    Archs.tags "specify architecture"::
  []



module type Config = sig
  val diyone : bool
  val uniq : bool
  val outmap : string option
  val lowercase : bool
  val sufname : string option
end

module Make(Co:Config) (A:Fence.S) = struct
  module E = Edge.Make(Edge.Config)(A)
  module N = Namer.Make(A)(E)
  module Norm = Normaliser.Make(Co)(E)
  module P = LineUtils.Make(E)

  let parse_line s = P.parse s

  let skip_line s = match s with
  | "" -> true
  | _ -> match s.[0] with
    | '#'|'%' -> true
    | _ -> false

  let add name (key,ps,_) st k =
    let xs =
      try StringMap.find  key k
      with Not_found -> [] in
    StringMap.add key ((name,(ps,st))::xs) k

  let scan chan =
    let k = ref StringMap.empty in
    let rec do_rec () =
      let line = input_line chan in
      if skip_line line then do_rec ()
      else begin
        let name,es,st = parse_line line in
        let ps = Norm.normalise_family (E.resolve_edges es) in
        k := add name ps st !k ;
        do_rec ()
    end in
    try do_rec ()
    with End_of_file ->  !k

  let pp_scope_opt = function
    | None -> ""
    | Some st -> " " ^ BellInfo.pp_scopes st

  let dump_map outmap m =
    StringMap.iter
      (fun k xs ->
        let base = k in
        if not Co.diyone then printf "%s\n" base ;

        let rec do_rec seen = function
          | [] -> ()
          | (name,(es,scope))::rem ->
              let new_name = N.mk_name base ?scope es in              
              if Co.uniq &&  StringSet.mem new_name seen then
                Warn.fatal "Duplicate name: %s" new_name ;
              if Co.diyone then
                printf "%s: %s%s\n"
                  new_name
                  (E.pp_edges es)
                  (pp_scope_opt scope)
              else
                printf "  %s -> %s: %s%s\n"
                  name new_name
                  (E.pp_edges es)
                  (pp_scope_opt scope) ;
              fprintf outmap "%s %s\n" name new_name ;
              let seen = StringSet.add new_name seen in
              do_rec seen rem in

        do_rec StringSet.empty  (List.rev xs))
      m

  let zyva chan =
    try
      let k = scan chan in
      Misc.output_protect
        (fun chan ->  dump_map chan k)
        (match Co.outmap with
        | None -> "/dev/null"
        | Some s -> s)
    with Misc.Fatal msg ->
      eprintf "Fatal error: %s\n" msg ;
      exit 2

end

let () =
  Util.parse_cmdline
    opts
    (fun _ -> raise (Arg.Bad  "No argument"))

let () =
  let module Co = struct
    let diyone = !diyone
    let uniq = !uniq
    let outmap = !map
    let lowercase = !lowercase
    let sufname = None
  end in
  let module Build = Make(Co) in
  (match !arch with
  | `X86 ->
      let module M = Build(X86Arch_gen) in
      M.zyva
  | `X86_64 -> assert false
  | `PPC ->
      let module M = Build(PPCArch_gen.Make(PPCArch_gen.Config)) in
      M.zyva
  | `ARM ->
      let module M = Build(ARMArch_gen.Make(ARMArch_gen.Config)) in
      M.zyva
  | `AArch64 ->
      let module A =
        AArch64Arch_gen.Make
          (struct
            include AArch64Arch_gen.Config
            let moreedges = !Config.moreedges
          end) in
      let module M = Build(A) in
      M.zyva
  | `MIPS ->
      let module M = Build(MIPSArch_gen.Make(MIPSArch_gen.Config)) in
      M.zyva
  | `RISCV ->
      let module M = Build(RISCVArch_gen.Make(RISCVArch_gen.Config)) in
      M.zyva
  | `LISA ->
      let module BellConfig =
        struct
          let debug = !Config.debug
          let verbose = !Config.verbose
          let libdir = Version_gen.libdir
          let prog = Config.prog
          let bell = !bell
          let varatom = []
        end in
      let module M = Build(BellArch_gen.Make(BellConfig)) in
      M.zyva
  | `C | `CPP ->
      let module M = Build(CArch_gen) in
      M.zyva)
    stdin
