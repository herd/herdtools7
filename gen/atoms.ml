(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* select  cycles with at least one atomic specification *)
open Archs
open Printf

(* Configuration *)
let arch = ref PPC
 
let opts = [Util.arch_opt arch]

module Make (A:Fence.S) =
    struct
      module E = Edge.Make(A)

      let is_atom es =
        List.exists
          (fun e -> match e.E.a1,e.E.a2 with
          | None,None -> false
          | Some _,_
          | _,Some _ -> true)
          es &&
        List.for_all
          (fun e -> match e with
          | {E.edge=E.Po _; a1=None; a2=None;} -> false
          | {E.edge=E.Fenced (f,_,_,_) } -> not (A.is_isync f)
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
  let module V = SymbConstant in
  (match !arch with
  | X86 ->
      let module M = Make(X86Arch.Make(V)) in
      M.zyva
  | PPC ->
      let module M = Make(PPCArch.Make(V)(PPCArch.Config)) in
      M.zyva
  | ARM ->
      let module M = Make(ARMArch.Make(V)) in
      M.zyva
  | MIPS ->
      let module M = Make(MIPSArch.Make(V)) in
      M.zyva 
  | C ->
      let module M = Make(CArch) in
      M.zyva
  | CPP -> Warn.fatal "CCP arch in atoms")      
     ()
