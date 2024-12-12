(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(**************)
(* Digest Env *)
(**************)

type hinfo = { hash : string ; filename : string; }

type env = hinfo StringMap.t

exception Seen

let check_env env name filename hash =
  try
    let ohash = StringMap.find name env in
    if hash = ohash.hash then raise Seen
    else Warn.user_error "Different hashes for test %s (previous file %s)"
        name ohash.filename
  with Not_found ->
    StringMap.add name {hash;filename;} env

(***************************)
(* Digest of initial state *)
(***************************)

open Printf

module type PteVal = sig
  val hash_pteval : ParsedPteVal.t -> string
end

module HashUtils(P:PteVal) = struct

(* Backward compatible identifier and value printing functions *)

  open MiscParser

  let dump_location = function
  | Location_reg (i,r) -> Printf.sprintf "%i:%s" i r
  | Location_sreg s -> s
  | Location_global v -> ParsedConstant.pp_v_old v

  and pp_v v =
    let open Constant in
    match v with
    | PteVal p -> P.hash_pteval p
    | _ -> ParsedConstant.pp_v_old v


  let digest_init debug init =
    let open TestType in
    let init =
      List.sort
        (fun (loc1,(t1,v1)) (loc2,(t2,v2)) ->
          match location_compare loc1 loc2 with
          | 0 ->
             if
               ParsedConstant.compare v1 v2 <> 0 &&
                 compare t1 t2 <> 0
             then begin
                 Warn.fatal
                   "Location %s non-unique in init state"
                   (dump_location loc1)
               end ;
             0
          | c -> c)
        init in

    let init =
      Misc.rem_dups
        (fun (loc1,_) (loc2,_) -> location_compare loc1 loc2 = 0)
        init in

(* We perform explicit printing to be  more robust
   against pretty printer changes *)

    let pp =
      (String.concat "; "
         (List.map
            (fun (loc,(t,v)) ->
              match t with
              | TyDef ->
                 sprintf "%s=%s"
                   (dump_location loc) (pp_v v)
              | TyDefPointer ->
                 sprintf "*%s=%s"
                   (dump_location loc) (pp_v v)
              | Ty t ->
                 sprintf "%s %s=%s" t
                   (dump_location loc) (pp_v v)
              | Atomic t ->
                 sprintf "_Atomic %s %s=%s" t
                   (dump_location loc) (pp_v v)
              | Pointer t ->
                 sprintf "%s *%s=%s" t
                   (dump_location loc) (pp_v v)
              | TyArray (t,sz) ->
                 sprintf "%s %s[%i]" t (dump_location loc) sz)
            init)) in
    debug "INIT" pp ;
    Digest.string pp

end

module NoPteValHU = HashUtils(struct let hash_pteval _ = assert false end)

let digest_init debug init = NoPteValHU.digest_init debug init

let key_compare k1 k2 =
  if MiscParser.key_match k1 k2 then
    Warn.user_error "Duplicated meta-data on key %s\n" k2 ;
  String.compare k1 k2

let do_digest_info verbose i =
  let i = List.stable_sort (fun (k1,_) (k2,_) -> key_compare k1 k2) i in
  let ds =
    List.fold_left
      (fun ds (k,i) ->
        if MiscParser.digest_mem k then
          sprintf "%s=%s" (Misc.lowercase k) i::ds
        else ds)
      [] i in
  match ds with
  | [] -> "" (* Backward compatibility *)
  | _::_ ->
     let i = String.concat "" ds in
     let d = Digest.string i in
     if verbose > 0 then
       eprintf "INFO:\n%s\nDigest=\"%s\"\n\n"
         i (Digest.to_hex d) ;
     d
let digest_info = do_digest_info 0

(**********)
(* Digest *)
(**********)

module Make(A:ArchBase.S)
    = struct

      type init = MiscParser.state
      type prog = (MiscParser.proc * A.pseudo list) list
      type rlocations =  MiscParser.RLocSet.t

      let verbose = 0

      let debug tag s =
        if verbose > 0 then
          eprintf "%s:\n%s\nDigest=\"%s\"\n\n"
            tag s (Digest.to_hex (Digest.string s))
        else ()

      let digest_info = do_digest_info verbose

      module HU=HashUtils(A)

      let digest_init init = HU.digest_init debug init

(* Code digest *)

      let all_labels =
        List.fold_left
          (fun k (_,code) ->
            List.fold_left
              (A.fold_labels (fun k lbl -> StringSet.add lbl k))
              k code)
          StringSet.empty

      let change_labels f =
        List.map
          (fun (p,code) ->
            let code = List.map (A.map_labels f) code in
            p,code)

      let refresh_labels pref prog =
        let lbls = all_labels prog in
        let next = ref 0 in
        let env =
          StringSet.fold
            (fun lbl env ->
              let new_lbl = sprintf "L%s%02i" pref !next in
              incr next ;
              StringMap.add lbl new_lbl env)
            lbls StringMap.empty in
        change_labels
          (fun lbl ->
            try StringMap.find lbl env
            with Not_found -> assert false)
          prog

      let norm_labels = refresh_labels ""

      let norm_instructions =
        List.map
          (fun (p,code) ->
            let code = List.map (A.pseudo_map A.norm_ins) code in
            p,code)

      let dump_pseudo =
        let rec dump_rec p k = match p with
        | A.Nop -> k
        | A.Instruction i -> A.dump_instruction_hash i::k
        | A.Label (lbl,p) -> sprintf "%s:" lbl::dump_rec p k
        | A.Symbolic s -> sprintf "codevar:%s" s::k
        | A.Macro _ -> assert false (* applied after macro expansion *) 
        | A.Pagealign -> "pagealign"::k
        | A.Skip _ -> assert false in
        fun (_,ps) ->
          List.fold_right dump_rec ps []



      let digest_code code =
        (* Because I have introduced 64bits instructions,
           which we can remap to 32bits ones... *)
        let code = norm_instructions code in
        (* Because labels may change, when generated by macro
           expansion *)
        let code = norm_labels code in
        (* Just pretty_print code in a normalized way *)
        let code = List.map dump_pseudo code in
        let pp =  Misc.string_of_prog code in
        debug "CODE" pp ;
        Digest.string pp


(* Observed locations digest *)
      let digest_observed locs =
        let locs = MiscParser.RLocSet.elements locs in
        let pp =
          String.concat "; "
            (List.map (ConstrGen.dump_rloc HU.dump_location) locs) in
        debug "LOCS" pp ;
        Digest.string pp

      let digest info init code observed =
        Digest.to_hex
          (Digest.string
             (String.concat ""
                [digest_info info; digest_init init;
                 digest_code code; digest_observed observed;]))
    end
