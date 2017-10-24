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

let digest_init debug init =
  let open MiscParser in
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

  let dump_location = function
    | Location_reg (i,r) -> Printf.sprintf "%i:%s" i r
    | Location_sreg s -> s
    | Location_global v -> ParsedConstant.pp_v v
    | Location_deref (v,i) ->
        Printf.sprintf "%s[%i]" (ParsedConstant.pp_v v) i
  in

  let pp =
    (String.concat "; "
       (List.map
          (fun (loc,(t,v)) -> match t with
          | TyDef ->
              sprintf "%s=%s"
                (dump_location loc) (ParsedConstant.pp_v v)
          | TyDefPointer ->
              sprintf "*%s=%s"
                (dump_location loc) (ParsedConstant.pp_v v)
          | Ty t ->
              sprintf "%s %s=%s" t
                (dump_location loc) (ParsedConstant.pp_v v)
          | Atomic t ->
              sprintf "_Atomic %s %s=%s" t
                (dump_location loc) (ParsedConstant.pp_v v)
          | Pointer t ->
              sprintf "%s *%s=%s" t
                (dump_location loc) (ParsedConstant.pp_v v)
          | TyArray (t,sz) ->
              sprintf "%s %s[%i]" t (dump_location loc) sz)
          init)) in
  debug "INIT" pp ;
  Digest.string pp

(**********)
(* Digest *)
(**********)

module Make(A:ArchBase.S)
    = struct

      type init = MiscParser.state
      type prog = (int * A.pseudo list) list
      type locations =  MiscParser.LocSet.t


      open MiscParser

      let verbose = 0

      let debug tag s =
        if verbose > 0 then eprintf "%s:\n%s\n" tag s
        else ()

      let digest_init init = digest_init debug init

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
        | A.Instruction i -> A.dump_instruction i::k
        | A.Label (lbl,p) -> sprintf "%s:" lbl::dump_rec p k
	| A.Symbolic s -> sprintf "codevar:%s" s::k
        | A.Macro _ -> assert false (* applied after macro expansion *) in
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
        let locs = MiscParser.LocSet.elements locs in
        let pp = String.concat "; " (List.map dump_location locs) in
        debug "LOCS" pp ;
        Digest.string pp

        
      let digest init code observed =
        Digest.to_hex
          (Digest.string
             (digest_init init ^ digest_code code ^
              digest_observed observed))
    end
