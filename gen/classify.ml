(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Archs
open Printf

let arch = ref PPC
let diyone = ref false
let lowercase = ref false
let uniq = ref false
let map = ref None
let opts =
  ("-diyone", Arg.Set diyone," generate input for diyone")::
  ("-lowercase", Arg.Bool (fun b -> lowercase := b),
   sprintf "<bool> use lowercase familly names, default %b" !lowercase)::
  ("-u", Arg.Set uniq," reject duplicate normalised names")::
  ("-map", Arg.String (fun s -> map := Some s)," <name> save renaming map into file <name>")::
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
end

module Make(Co:Config) (A:Fence.S) = struct
  module E = Edge.Make(A)
  module N = Namer.Make(A)(E)
  module Norm = Normaliser.Make(Co)(E)

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

  let skip_line s = match s with
  | "" -> true
  | _ -> match s.[0] with
    | '#'|'%' -> true
    | _ -> false

  module StringSet = MySet.Make(String)

  let add name (key,ps) k =
    let xs =
      try StringMap.find  key k
      with Not_found -> [] in
    StringMap.add key ((name,ps)::xs) k

  let scan chan =
    let k = ref StringMap.empty in
    let rec do_rec () =
      let line = input_line chan in
      if skip_line line then do_rec ()
      else begin
        let name,es = parse_line line in
        let ps = Norm.normalise_family (E.resolve_edges es) in
        k := add name ps !k ;
        do_rec ()
    end in
    try do_rec ()
    with End_of_file ->  !k

  let dump_map outmap m =
    StringMap.iter
      (fun k xs ->
        let base = k in
        if not Co.diyone then printf "%s\n" base ;

        let rec do_rec seen = function
          | [] -> ()
          | (name,es)::rem ->
              let new_name = N.mk_name base es in              
              if Co.uniq &&  StringSet.mem new_name seen then
                Warn.fatal "Duplicate name: %s" new_name ;
              if Co.diyone then
                printf "%s: %s\n"
                  new_name
                  (E.pp_edges es)
              else
                printf "  %s -> %s : %s\n"
                  name new_name
                  (E.pp_edges es) ;
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
  let module V = SymbConstant in
  let module Co = struct
    let diyone = !diyone
    let uniq = !uniq
    let outmap = !map
    let lowercase = !lowercase
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
  | AArch64 ->
      let module M = Build(AArch64Arch.Make(V)) in
      M.zyva
  | MIPS ->
      let module M = Build(MIPSArch.Make(V)) in
      M.zyva
  | C|CPP ->
      let module M = Build(CArch) in
      M.zyva)
    stdin
