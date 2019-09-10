(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** select  cycles with at least one atomic specification *)

open Printf

(* Configuration *)
let arch = ref `PPC
 
let opts = [Util.arch_opt arch]

module Make (A:Fence.S) =
    struct
      module E = Edge.Make(Edge.Config)(A)

      let is_atom es =
        List.exists
          (fun e -> match e.E.a1,e.E.a2 with
          | None,None -> false
          | Some _,_
          | _,Some _ -> true)
          es &&
        List.for_all
          (fun e -> match e with
          | {E.edge=E.Po _; a1=None; a2=None; _} -> false
          | {E.edge=E.Fenced (f,_,_,_); _ } -> not (A.is_isync f)
          | _ -> true)
          es

      let parse_line s =
        try          
          let r = String.index s ':' in
          let name  = String.sub s 0 r
          and es = String.sub s (r+1) (String.length s - (r+1)) in
          let es = E.parse_edges es in
          name,es
        with
        | Not_found | Invalid_argument _ ->
            Warn.fatal "bad line: %s" s

      let pp_edges es = String.concat " " (List.map E.pp_edge es)

      let rec next_line () =
        let line = read_line () in
        match line with
        | "" -> next_line ()
        | _ -> begin match line.[0] with
          | '%'|'#' -> next_line ()
          | _ -> line
        end
        
      let zyva () =
        try while true do
          try
            let line = next_line () in
            let _,es = parse_line line in
            if is_atom es then  printf "%s\n" line
(*           else eprintf "No: '%s'\n" line *)
          with Misc.Fatal msg -> Warn.warn_always "%s" msg
        done with End_of_file -> ()
    end

let () =
  Util.parse_cmdline
    opts
    (fun _ -> raise (Arg.Bad "No argument"))

let () =
  (match !arch with
  | `X86 ->
      let module M = Make(X86Arch_gen) in
      M.zyva
  | `X86_64 -> assert false
  | `PPC ->
      let module M = Make(PPCArch_gen.Make(PPCArch_gen.Config)) in
      M.zyva
  | `ARM ->
      let module M = Make(ARMArch_gen.Make(ARMArch_gen.Config)) in
      M.zyva
  | `AArch64 ->
      let module M = Make(AArch64Arch_gen.Make(AArch64Arch_gen.Config)) in
      M.zyva
  | `MIPS ->
      let module M = Make(MIPSArch_gen.Make(MIPSArch_gen.Config)) in
      M.zyva 
 | `RISCV ->
      let module M = Make(RISCVArch_gen.Make(RISCVArch_gen.Config)) in
      M.zyva
  | `LISA ->
      let module BellConfig =
        struct
          let debug = !Config.debug
          let verbose = !Config.verbose
          let libdir = Version_gen.libdir
          let prog = Config.prog
          let bell = !Config.bell
          let varatom = []
        end in
      let module M = Make(BellArch_gen.Make(BellConfig)) in
      M.zyva 
  | `C ->
      let module M = Make(CArch_gen) in
      M.zyva
  | `CPP -> Warn.fatal "CCP arch in atoms")      
     ()
