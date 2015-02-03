(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Expand candidate relaxation macros in cycle list *)
open Archs
open Printf

let arch = ref PPC
let opts =  [Util.arch_opt arch]


module type Config = sig

end

module Make(Co:Config) (A:Arch.S) = struct
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


  let add name (key,ps) k =
    let xs =
      try StringMap.find  key k
      with Not_found -> [] in
    StringMap.add key ((name,ps)::xs) k

  let scan chan =
    let rec do_rec () =
      let line = input_line chan in
      let name,es = parse_line line in
      printf "%s: %s\n" name (E.pp_edges es) ;        
      do_rec () in
    try do_rec ()
    with End_of_file ->  ()

  let zyva chan = scan chan
end

let () =
  Util.parse_cmdline
    opts
    (fun _ -> raise (Arg.Bad  "No argument"))

let () =
  let module V = SymbConstant in
  let module Co = struct
  end in
  let module Build = Make(Co) in
  (match !arch with
  | X86 ->
      let module M = Build(X86Arch.Make(V)) in
      M.zyva
  | PPC ->
      let module M = Build(PPCArch.Make(V)(PPCArch.Config)) in
      M.zyva
  | ARM ->
      let module M = Build(ARMArch.Make(V)) in
      M.zyva
  | MIPS ->
      let module M = Build(MIPSArch.Make(V)) in
      M.zyva
  | C|CPP -> assert false)
    stdin
