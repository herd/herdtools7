(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
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

module type Config = sig
  val debug : Debug_gen.t
  val verbose : int
  val libdir : string
  val prog : string
  val bell : string option
  val varatom : string list
end

module Make(O:Config) = struct

include BellBase

(* Assume little endian *)
let tr_endian = Misc.identity

let bi = match O.bell with
| Some fname ->
    let module R =
    ReadBell.Make
      (struct
        let debug_lexer = O.debug.Debug_gen.lexer
        let debug_model = O.debug.Debug_gen.model
        let verbose = O.verbose
        let libfind =
          let module ML =
            MyLib.Make
              (struct
                let includes = []
                let env = None
                let libdir = O.libdir
                let debug = O.debug.Debug_gen.files
              end) in
          ML.find
        let compat = false
        let prog = O.prog
      end) in
  Some (R.read fname)
| None -> None

(* Workaround, as we cannot simply write
module ScopeGen =  (val scopegen : ScopeGen.S)
in OCaml pre-4.02.1 *)

module ScopeGen =
  struct
    let scopegen = match bi with
    | None ->
        let module M = ScopeGen.NoGen in
        (module M : ScopeGen.S)
    | Some bi ->
        let module M =
          ScopeGen.Make
            (struct
              let debug = false
              let info = bi
            end) in
        (module M : ScopeGen.S)

    let default,gen,all =
      let module M = (val scopegen : ScopeGen.S) in
      M.default,M.gen,M.all
  end

(* Should check non-ambiguity *)
let pp_annot a = match a with
| "atomic" -> "A"
| "ordinary" -> "P"
| _ ->
    let len = String.length a in
    match len with
    | 0 -> assert false
    | _ ->
        let fst = a.[0] in
        sprintf "%c%s"
          (Misc.char_uppercase fst)
          (String.sub a 1 (len-1))

(* No atoms yet *)
let bellatom = true
type atom = string list

let default_atom = [] (* Wrong, extract from bell file? *)

let tr_dir = function
  | R -> BellName.r
  | W -> BellName.w
  | J -> BellName.j

let applies_atom = match bi with
| None -> (fun a _d -> match a with [] -> true | _ -> false)
| Some bi -> (fun a d -> BellModel.check_event (tr_dir d) a bi)

let pp_plain = "P"
let pp_as_a = None

let pp_annots a = match a with
| [] -> ""
| _ ->
    String.concat "" (List.map pp_annot a)

let pp_atom a =  pp_annots a

let compare_atom a1 a2 =
  Misc.list_compare String.compare a1 a2


let fold_annots eg f r =
  List.fold_left
    (fun r ag -> match ag with
    | [] -> r
    | _  ->
        Misc.fold_cross (List.map StringSet.elements ag) f r)
    r eg


let fold_annots_dir bi d f r =
  let eg = BellModel.get_events (tr_dir d) bi in
  fold_annots eg f r


let fold_non_mixed = match bi with
| None -> fun _f r -> r
| Some bi ->
    fun f r ->
      fold_annots_dir bi R f (fold_annots_dir bi W f r)

let fold_atom = fold_non_mixed

let worth_final _ = false

(* Atomic variation *)

(* No atomic variation *)
let no_varatom f r = f None r

(* Some atomic variation *)
let fold_from_gen all f =
  List.fold_right
    (fun al -> Misc.fold_cross (List.map StringSet.elements al) f)
    all

let fold_from all f = fold_from_gen all (fun al -> f (Some al))


let varatom = match  O.varatom with
| [] -> None
| lines ->
    let module P =
      Annot.Make
        (struct
          let debug = O.debug.Debug_gen.lexer
        end) in
    Some
      begin
        let x = P.parse lines in
        if O.debug.Debug_gen.generator then
          eprintf "Variations:\n%s\n" (BellModel.pp_event_decs x) ;
        x
      end

let varatom_dir = match varatom with
| None -> fun _ -> no_varatom
| Some va ->
    fun d ->
      try
        let at =  StringMap.find  (tr_dir d) va in
        fold_from at
      with Not_found -> no_varatom

let merge_atoms a1 a2 = if a2 = a1 then Some a1 else None

let atom_to_bank _ = Code.Ord

let varatom_rmw = match varatom with
| None -> no_varatom
| Some _va -> fun _ -> assert false

include NoMixed

(* End of atoms *)

(**********)
(* Fences *)
(**********)

type fence = barrier

let is_isync _ = false

let compare_fence = barrier_compare

let default = Fence ([],None)

let strong = match bi with
| None -> Fence ([],None)
| Some bi ->
    try Fence (BellModel.get_default BellName.f bi,None)
    with Not_found -> Fence ([],None)

let pp_fence (Fence (a,_)) = sprintf "Fence%s" (pp_annots a)

let fold_fences = match bi with
| None -> fun _f k -> k
| Some bi ->
    fun f k ->
      let eg = BellModel.get_events BellName.f bi in
      fold_annots eg (fun a k -> f (Fence (a,None)) k)  k

let fold_cumul_fences _f k = k
let fold_all_fences  = fold_fences
let fold_some_fences f k = f strong k

let orders _ _ _ = true

let no_varfence f r = f strong r

let var_fence f = match varatom with
| None -> no_varfence f
| Some va ->
    try
        let at =  StringMap.find  BellName.f va in
        fold_from_gen at (fun al -> f (Fence (al,None)))
    with Not_found -> no_varfence f


(********)
(* Deps *)
(********)

include ClassicDep
include OneRMW
include NoEdge
include
    ArchExtra_gen.Make
    (struct
      type arch_reg = reg

      let is_symbolic _ = false

      let pp_reg = pp_reg
      let free_registers = allowed_for_symb
    end)
end
