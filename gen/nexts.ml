(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Filter cycles list by number of accesses *)
open Code
open Archs
open Printf

let arch = ref PPC
let n = ref None
let verbose = ref 0

let opts =
  ("-v", Arg.Unit (fun () -> incr verbose),"be verbose")::
  ("-n", Arg.Int (fun x -> n := Some x),
   "<n> filter cyles by number of accesses (mandatory)")::
  Util.arch_opt arch::
  []


module type Config = sig
  val verbose : int
  val nacc : int
end

module Make (Co:Config) (A:Arch.S) = struct
  module E = Edge.Make(A)

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

  let count_ext es =
    List.fold_left
      (fun k e ->
        match E.get_ie e with
        | Ext -> k+1
        | Int -> k)
      0 es

  let filter chan =
    let rec do_rec () =
      let line = input_line chan in
      let name,es = parse_line line in
      let c = count_ext es in
      if Co.verbose > 0 then
        eprintf "%s: %i\n" name c ;
      if c = Co.nacc then
        printf "%s: %s\n"name (E.pp_edges es) ;
      do_rec () in
    try do_rec ()
    with End_of_file ->  ()

  let zyva chan = filter chan
end

let () =
  Util.parse_cmdline
    opts
    (fun _ -> raise (Arg.Bad  "No argument"))

let () =
  let module Co = struct
    let verbose = !verbose
    let nacc = match !n with
    |  None ->
        eprintf "Option -n <n> is mandatory\n" ;
        exit 2
    | Some n -> n
  end in
  let module Build = Make(Co) in
  (match !arch with
  | X86 ->
      let module M = Build(X86Arch) in
      M.zyva
  | PPC ->
      let module M = Build(PPCArch.Make(PPCArch.Config)) in
      M.zyva
  | ARM ->
      let module M = Build(ARMArch) in
      M.zyva
  | AArch64 ->
      let module M = Build(AArch64Arch) in
      M.zyva
 | MIPS ->
      let module M = Build(MIPSArch) in
      M.zyva
  | C|CPP -> assert false)
    stdin
