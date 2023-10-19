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
open Code


module type AltConfig = sig
  include DumpAll.Config
  val upto : bool
  val max_ins : int
  val mix : bool
  val max_relax : int
  val min_relax : int
  val choice : check
  type relax
  val prefix : relax list list
  val variant : Variant_gen.t -> bool
  type fence
  val cumul : fence list Config.cumul
end

module Make(C:Builder.S)
    (O:AltConfig with type relax = C.R.relax and type fence = C.A.fence) :
    sig
      val gen : ?relax:C.R.relax list -> ?safe:C.R.relax list -> ?reject:C.R.relax list -> int -> unit
    end

    =
  struct
    let mixed = Variant_gen.is_mixed O.variant
    let do_kvm = Variant_gen.is_kvm  O.variant
    module D = DumpAll.Make(O) (C)
    open C.E
    open C.R

    let dbg = false

    module RelaxSet = C.R.Set

    let is_int e = match get_ie e with
    | Int -> true
    | Ext -> false

    let is_ext e = not (is_int e)

    let equal_fence f1 f2 = C.A.compare_fence f1 f2 = 0

    let is_cumul =
      let open Config in
      match O.cumul with
      | Empty -> (fun _ -> false)
      | All -> (fun _ -> true)
      | Set fs ->
          (fun f -> List.exists (equal_fence f) fs)

    let choice_sc po_safe e1 e2 =
      let r = match e1.edge,e2.edge with
(*
  Now accept internal with internal composition
  when the do not match safe, explicit po candidates.
  A bit rude, maybe...

  Also notice that we are more tolerant for Rfi.
 *)
(* Assuming Dp is safe *)
    | Rf Int,Dp _ | Dp _,Rf Int -> true
    | Dp (_,sd,_),Ws Int | Dp (_,sd,_),Fr Int ->
        not (po_safe sd (dir_src e1) (dir_tgt e2))
    | Po (sd1,_,_), Dp (_,sd2,_) ->
        not (po_safe sd1 (dir_src e1) (dir_tgt e1)) &&
        not (po_safe (seq_sd sd1 sd2) (dir_src e1) (dir_tgt e2))
    | Dp (_,sd1,_),Po (sd2,_,_) ->
        not (po_safe sd2 (dir_src e2) (dir_tgt e2)) &&
        not (po_safe (seq_sd sd1 sd2) (dir_src e1) (dir_tgt e2))
(* Check Po is safe *)
    | Po (sd1,_,_),Po (sd2,_,_) ->
        not (po_safe (seq_sd sd1 sd2) (dir_src e1) (dir_tgt e2))
    | Rf Int,Po (sd,_,_) ->
        po_safe sd (dir_src e2) (dir_tgt e2) &&
        not (po_safe sd (dir_src e1) (dir_tgt e2))
    | Po (sd,_,_),Rf Int ->
        po_safe sd (dir_src e1) (dir_tgt e1) &&
        not (po_safe sd (dir_src e1) (dir_tgt e2))
(* Allow Rmw *)
    | (Rmw _,_)|(_,Rmw _) -> true
(* Added *)
    | _,_ ->
        match get_ie e1, get_ie e2 with
        | Int,Int -> false
        | Ext,_|_,Ext -> true  in
      if dbg then
        eprintf "Choice: %s %s -> %b\n%!" (C.E.pp_edge e1) (C.E.pp_edge e2) r ;
      r

    let choice_default e1 e2 =
      let r = match e1.edge,e2.edge with
(*
  Now accept some internal with internal composition
 *)
      | (Ws Int|Rf Int|Fr Int),(Dp (_,_,_)|Po (Diff,_,_))
      | (Dp (_,_,_)|Po (Diff,_,_)),(Ws Int|Rf Int|Fr Int)
      | Dp (_,Diff,_),Po (Diff,_,_)
      | Po (Diff,_,_),Dp (_,Diff,_)
      | Rf Int,Po (Same,_,_)
      | Po (Same,_,_),Rf Int
      | (Rmw _,_)|(_,Rmw _) -> true
      | _,_ ->
          (* Reject other internal followed by internal sequences *)
          match get_ie e1, get_ie e2 with
          | Int,Int -> false
          | Ext,_|_,Ext -> true  in
      if dbg then
        eprintf "Choice: %s %s -> %b\n%!" (C.E.pp_edge e1) (C.E.pp_edge e2) r ;
      r

(* Check altenance of com/po *)
    let choice_critical e1 e2 =
      let r =
        match e1.edge,e2.edge with
(* Two cases of allowed com composition *)
        | (Ws _|Leave CWs|Back CWs|Fr _|Leave CFr|Back CFr),
          (Rf _|Leave CRf|Back CRf) -> true
(* Rmw allowed to compose arbitrarily *)
        | (Rmw _,_)|(_,Rmw _) -> true
(* Otherwise require alternance *)
        | _,_ ->  C.E.get_ie e1 <> C.E.get_ie e2 in
(*      eprintf "Choice: %s %s -> %b\n" (C.E.pp_edge e1) (C.E.pp_edge e2) r ; *)
      r
    let choice_mixed e1 e2 =
      let r =
        match e1.edge,e2.edge with
(* Two cases of allowed com composition *)
        | (Ws _|Leave CWs|Back CWs|Fr _|Leave CFr|Back CFr),
          (Rf _|Leave CRf|Back CRf) -> true
(* Rmw allowed to compose arbitrarily *)
        | (Rmw _,_)|(_,Rmw _) -> true
(* Otherwise accept composition *)
        | _,_ ->
            let ie1 = C.E.get_ie e1 and ie2 =  C.E.get_ie e2 in
            match ie1,ie2 with
            | Int,Int ->
                begin match loc_sd e1,loc_sd e2 with
                | (Same,Same) | (Diff,Same) | (Same,Diff)
                  -> true
                | Diff,Diff -> false
                end
            | Ext,Ext -> false
            | (Ext,Int) | (Int,Ext)
                -> true  in
(*      eprintf "Choice: %s %s -> %b\n" (C.E.pp_edge e1) (C.E.pp_edge e2) r ; *)
      r
    let choice_uni e1 e2 =  match e1.edge,e2.edge with
    | (Ws _,Ws _)
    | (Fr _,Ws _)
    | (Rf _,Fr _)
    | (Rf _,Hat)
    | (Hat,Fr _)
      -> C.E.get_ie e1 <> C.E.get_ie e2 (* Allow alternance *)
    | Po _,Po _ -> false
    | _,_ -> true

    let choice_id _ _ = true

    let choice_free e1 e2 = match e1.edge,e2.edge with
    | (Ws _,Ws _)
    | (Fr _,Ws _)
    | (Rf _,Fr _)
      -> false
    | _,_ -> true

    let choice_free_alt e1 e2 = match e1.edge,e2.edge with
    | (Ws _,Ws _)
    | (Fr _,Ws _)
    | (Rf _,Fr _)
      -> C.E.get_ie e1 <> C.E.get_ie e2 (* Allow alternance *)
    | _,_ -> true

    let choice_ppo e1 e2 =
      choice_free e1 e2 &&
      C.E.compare e1 e2 <> 0 &&
      (match e1.edge with
      | Dp (dp,_,Dir R) when C.A.is_ctrlr dp -> is_ext e2
      | _ -> true)

    let choice_transitive safes xs ys e1 e2 =
      choice_free_alt e1 e2 &&
      begin match  C.E.get_ie e1, C.E.get_ie e2 with
      | Int,Int ->
          let cs = C.E.compact_sequence xs ys e1 e2 in
          if O.verbose > 0 then eprintf "COMPACT %s,%s -> [%s] -> "
            (C.E.pp_edge e1) (C.E.pp_edge e2)
            (String.concat ","
               (List.map (fun es -> C.R.pp_relax (C.R.ERS es)) cs)) ;
          let r =
            not
              (List.exists
                 (fun es -> C.R.Set.mem (C.R.ERS es) safes)
                 cs) in
          if O.verbose > 0 then eprintf "%b\n" r ;
          r
      | _,_ -> true
      end



    let iarg f = fun _ _ _ _ -> f

    let choose c = match c with
    | Sc -> fun _safes po_safe _xs _ys -> choice_sc po_safe
    | Default -> iarg choice_default
    | MixedCheck -> iarg choice_mixed
    | Critical -> iarg choice_critical
    | Uni -> iarg choice_uni
    | Thin |Total -> iarg choice_id
    | Free -> iarg choice_free_alt
    | Ppo -> iarg choice_ppo
    | Transitive ->
        (fun safes _po_safe -> choice_transitive safes)


    let compat_id ao d = match ao,d with
    | (None,_)|(_,(Irr|NoDir)) -> true
    | Some a,(Dir d) -> C.A.applies_atom a d

    let pair_ok safes po_safe xs ys e1 e2 = match e1.edge,e2.edge with
(*
  First reject some of hb' ; hb'
 *)
    | Hat,Hat   (* Hat *)
(* Ext Ext Only? *)
    | Ws _,Ws _ (* -> Ws *)
    | Fr _,Ws _ (* -> Fr*)
    | Rf _,Fr _ (* -> Ws *)
(*    Rf _,Fr _ (* -> Ws *) May be interesting, because
      values are observed by outcome itself,
      also useful to add Fre after B-cumulativity *)
      ->  C.E.get_ie e1 <> C.E.get_ie e2 (* Allow alternance *)
    | Id,Id -> false
    | Id,_ -> compat_id e1.a2 (dir_src e2)
    | _,Id -> compat_id e2.a1 (dir_tgt e1)
(* Fence cumulativity *)
    | Rf _,Fenced (f,_,_,_)
    | Fenced (f,_,_,_),Rf _ ->
        is_cumul f && choose O.choice safes po_safe xs ys e1 e2
    | _,_ -> choose O.choice safes po_safe xs ys e1 e2

    let check_mixed =
      if mixed then
        fun e1 e2 -> match  e1.edge,e2.edge with
        | Id,Id -> false
        | (_,Id)|(Id,_) -> true
        | _,_ -> false
      else fun _ _ -> true

    let rec hd_non_insert = function
      | [] -> assert false
      | [x] -> x
      | x::xs ->
          if C.E.is_insert_store x.C.E.edge then hd_non_insert xs
          else x
    let last_non_insert xs = hd_non_insert (List.rev xs)

    let do_compat safes po_safe xs ys =
      let x = Misc.last xs and y = List.hd ys in
      let r =
        C.E.can_precede x y
        && check_mixed x y
        && pair_ok safes po_safe xs ys x y
        &&
          begin
            if do_kvm then
              C.E.can_precede (hd_non_insert xs) (last_non_insert ys)
            else true
          end in
      if O.verbose > 2 then begin
        eprintf "do_compat '%s' '%s' = %b\n"
          (C.E.pp_edges xs)
          (C.E.pp_edges ys) r
      end ;
      r


    let can_precede safes po_safe (_,xs) k = match k with
    | [] -> true
    | (_,ys)::_ ->
        do_compat safes po_safe xs ys &&
        begin match k with
        | (_,[{edge=Id;_}])::(_,y::_)::_ when mixed ->
            let x = Misc.last xs in
            C.E.can_precede x y
        | _ -> true
        end

    let pp_ess ess =
      String.concat " "
        (List.fold_right
           (fun (_,es) ->
             List.fold_right
               (fun e k -> pp_edge e::k)
               es)
           ess [])

    let edges_ofs rs =
      List.map (fun r -> (r, edges_of r)) rs

(* Functional for recursive call of generators *)

    let sz (_,es) =
      if List.for_all (fun e -> is_id e.edge) es then 0 else 1


    let rec c_minprocs_es c = function
      | [] -> c
      | e::es ->
          let c = match e.C.E.edge with
          | Back _|Leave _ -> c
          | _ ->
              match get_ie e with
              | Int -> c
              | Ext -> c+1 in
          c_minprocs_es c es

    let rec c_minprocs_suff c = function
      | [] -> c
      | (_,es)::suff -> c_minprocs_suff (c_minprocs_es c es) suff

    let minprocs suff =
      let r = c_minprocs_suff 0 suff in
      if O.verbose > 3 then eprintf "MIN [%s] => %i\n" (pp_ess suff) r ;
      r


    let rec c_minint_es c = function
      | [] -> false,c
      | {edge=Id; _}::es ->  c_minint_es c es
      | e::es ->
          match get_ie e with
          | Ext -> true,c
          | Int -> c_minint_es (c+1) es

    let rec c_minint c = function
      | [] -> c
      | (_,es)::suff ->
          let stop,c = c_minint_es c es in
          if stop then c
          else c_minint c suff

    let minint suff = c_minint 0 suff

(* Prefix *)
    let prefix_expanded = List.flatten (List.map C.R.expand_relax_seq O.prefix)

    let () =
      if O.verbose > 0 && O.prefix <> [] then begin
        eprintf "Prefixes:\n" ;
        List.iter
          (fun rs ->
            eprintf "  %s\n" (C.R.pp_relax_list rs))
          prefix_expanded
      end

    let prefixes = List.map edges_ofs prefix_expanded

    let rec mk_can_prefix = function
      | [] -> (fun _ _ -> true)
      | [x] -> (fun p -> p x)
      | _::xs -> mk_can_prefix xs

    let can_prefix prefix = mk_can_prefix prefix

    let rec is_prefix l rl =
      match rl,l with
      | hrl::trl, hl::tl -> if hl = hrl then  is_prefix tl trl else false
      | [], _ -> true (* end of rl before or at the end of l *)
      | _, [] -> false (* end of l before end of rl*)


    let check_cycle rsuff rl =
      let rsuff = List.map (fun (_,rr) -> rr) rsuff in
      let rsuff = List.concat rsuff in
      not (List.exists (fun rl ->  is_prefix rsuff rl) rl)


    let call_rec prefix f0 safes po_safe over n r suff f_rec k ?(reject=[])=
      if
        can_precede safes po_safe r suff &&
        minprocs suff <= O.nprocs &&
        minint (r::suff) <= O.max_ins-1 &&
        check_cycle (r::suff) reject
      then
        let suff = r::suff
        and n = n-sz r in
        if O.verbose > 2 then eprintf "CALL: %i %s\n%!" n (pp_ess suff) ;
        let k =
          if
            over &&
            (n = 0 || (n > 0 && O.upto)) &&
            can_prefix prefix (can_precede safes po_safe) suff
          then begin
            let tr =  prefix@suff in
(*
            eprintf "TRY: '%s'\n"
              (C.E.pp_edges (List.flatten (List.map snd tr))) ;
*)
            try f0 po_safe tr k
            with  Misc.Exit -> k
            | Misc.Fatal msg |Misc.UserError msg ->
                eprintf "Marche pas: '%s'\n" msg ;
                k
            | e ->
              eprintf "Exc in F0: '%s'\n" (Printexc.to_string e) ;
              raise e
          end else k in
        if n <= 0 then k
        else f_rec n suff k
      else k

    module SdDir2Set =
      MySet.Make
        (struct
          type t = sd * extr * extr
          let compare = Misc.polymorphic_compare
        end)

    let extract_po rs =
      match O.choice with
      | Sc ->
          let d2 =
            List.fold_right
              (fun (r,_) k -> match r with
              | ERS [{edge=Po (sd,e1,e2); _}] -> SdDir2Set.add (sd,e1,e2) k
              | _ -> k)
              rs SdDir2Set.empty in
          if dbg then
            eprintf
              "PoSafe: {%s}\n"
              (SdDir2Set.pp_str ","
                 (fun (sd,e1,e2) -> pp_sd sd ^ "-" ^ pp_extr e1 ^ "-" ^ pp_extr e2)
                 d2) ;
          fun sd e1 e2 -> SdDir2Set.mem (sd,e1,e2) d2
      | m ->
          fun _ _ _ ->
            eprintf "Function po_safe called in mode %s\n%!"
              (pp_check m) ;
            assert false

    let zyva prefix aset relax safe reject n f =
(*      let safes = C.R.Set.of_list safe in *)
      let relax = edges_ofs relax in
      let safe = edges_ofs safe in
      let po_safe = extract_po safe in

      let rec choose_relax rs k = match rs with
      | [] -> k
      | r0::rs -> (* Build simple cycles for relaxation r0 *)

          let call_rec = call_rec prefix (f [fst r0]) aset po_safe ~reject:reject in

(* Add a safe edge to suffix *)
          let rec add_safe over ss n suf k =
            match ss with
            | [] -> k
            | s::ss ->
                let k = call_rec over n s suf (add_relaxs over) k in
                add_safe over ss n suf k

(* Add some relax edges r0 to suffix, or nothing *)
          and add_relaxs over n suf k =
            let k = call_rec true n r0 suf (add_relaxs true) k in
            add_safe over safe n suf k in

          match prefix with
          | [] -> (* Optimise: start with a relax edge r0 *)
              let k = call_rec true n r0 [] (add_relaxs true) k in
              choose_relax rs k
          | _::_ ->
              let k = add_relaxs false n [] k in
              choose_relax rs k in

(* Alternative: mix relaxation from relax list *)

      let all_relax k =
        let relax_set = RelaxSet.of_list (List.map fst relax) in
        let extract_relaxs suff =
          let suff_set = RelaxSet.of_list (List.map fst suff)  in
          RelaxSet.elements (RelaxSet.inter suff_set relax_set) in

        let call_rec =
          call_rec prefix
            (fun po_safe suff k ->
              let rs = extract_relaxs suff in
              let nrs = List.length rs in
              if nrs > O.max_relax || nrs < O.min_relax then k
              else f rs po_safe suff k)
            aset po_safe ~reject:reject in

(* Add a one edge to suffix *)
        let rec add_one over rs ss n suf k = match rs,ss with
        | [],[] -> k
        | [],s::ss ->
            let k = call_rec over n s suf (add_one over relax safe) k in
            add_one over rs ss n suf k
        | r::rs,_ ->
            let k = call_rec true n r suf (add_one true relax safe) k in
            add_one over rs ss n suf k in


(* Force first edge to be a relaxed one *)
        let rec add_first rs k = match rs with
        | [] -> k
        | r::rs ->
            let k = call_rec true n r [] (add_one true relax safe) k in
            add_first rs k in

        match prefix with
        | [] ->
            add_first relax k
        | _::_ ->
            add_one false relax safe n [] k in

(* New relax that does not enforce the first edge to be a relax *)

(* As a safety check, generate cycles with no relaxation *)
      let call_rec = call_rec prefix (f []) aset po_safe ~reject:reject in
      let rec no_relax ss n suf k = match ss with
      | [] -> k
      | s::ss ->
          let k = call_rec true n s suf (no_relax safe) k in
          no_relax ss n suf k in

      fun k -> match relax with
      | [] ->
          no_relax safe n [] k
      | _  ->
         if O.mix && O.max_relax < 1 then k (* Let us stay logical *)
          else if O.mix && O.max_relax > 1 then
            all_relax k
          else
            choose_relax relax k

    let rec all_int l =
      match l with
      | [] -> true
      | a::s -> (is_int a)&&(all_int s)

    let rec count_e ce = function
      | [] -> ce
      | e::es -> count_e (if is_int e then ce else ce+1) es


    let count_ext es = count_e 0 es

    let change_loc e = match loc_sd e with
    | Same -> false
    | Diff -> true

    let count_p p =
      let rec do_rec c = function
        | [] -> c
        | x::xs -> do_rec (if p x then c+1 else c) xs in
      do_rec 0

    let count_changes = count_p change_loc

    let build_safe r0 es =
      let rs =
        List.fold_right (fun (r,_) -> RelaxSet.add r) es RelaxSet.empty in
      let rs = RelaxSet.diff rs (RelaxSet.of_list r0) in
      RelaxSet.elements rs

    exception Result of bool

(* Is xs a prefix of s@p ? *)

    let prefix_spanp xs (p,s) =
      let rec is_prefix xs ys = match xs,ys with
        | [],_ -> raise (Result true)
        | _::_,[] -> xs (* xs -> what is still to be matched *)
        | x::xs,y::ys ->
           if C.E.compare x y = 0 then is_prefix xs ys
           else raise (Result false) in
      try
        let xs = is_prefix xs s in
        match is_prefix xs p with
        | [] -> true (* xs and s@p are equal! *)
        |  _::_ -> false (* xs larger.. *)
      with Result b -> b

    let substring_spanp rej pss =
      List.exists
        (fun xs ->
          List.exists
            (fun ps -> prefix_spanp xs ps)
            pss)
      rej

    let last_check_call rej aset f rs po_safe res k =
      match res with
      | [] -> k
      | _ ->
          let lst = Misc.last res in
          if can_precede aset po_safe lst res then
            let es = List.map snd res in
            let le = List.flatten es in
            try
              if
                (match O.choice with
                | Default| Sc | Ppo | MixedCheck -> true
                | Thin | Free | Uni | Critical | Transitive |Total -> false) &&
                (count_ext le=1 || all_int le || count_changes le < 2) then k
              else begin
                  let ok = (* Check for rejected sequenes that span over cycle "cut" *)
                  let rej = (* Keep non-trivial edge sequences only *)
                    List.filter
                      (function
                       | []|[_] -> false
                       | _::_::_ -> true)
                      rej  in
                  match rej with
                  | [] -> true
                  | _::_ ->
                     let max_sz =
                       List.fold_left (fun  k xs -> max k (List.length xs)) 0 rej in
                     let pss = Misc.cuts max_sz le in
                     not (substring_spanp rej pss) in
                if ok then
                  let mk_info _es =
                    let ss = build_safe rs res in
                    let info =
                      [
                        "Relax",pp_relax_list rs;
                        "Safe", pp_relax_list ss;
                      ] in
                    info,C.R.Set.of_list rs in
                  f le mk_info D.no_name D.no_scope k
                else k
              end
            with (Normaliser.CannotNormalise _) -> k
          else k

    let rec prefixp xs ys =
      match xs,ys with
      | [],_ -> true
      | _::_,[] -> raise Exit
      | x::xs,y::ys ->
         C.E.compare x y = 0 && prefixp xs ys

    let rec sublistp xs ys = match ys with
      | [] -> false
      | _::rem ->
         prefixp xs ys || sublistp xs rem

    let substringp xs ys =
      try sublistp xs ys
      with Exit ->
            match xs with
            | []|[_] -> false
            | _::_::_ ->
               let pss = Misc.cuts (List.length xs) ys in
               List.exists
                 (fun ps -> prefix_spanp xs ps)
                 pss

    let last_minute rej ess =
      not (List.exists (fun es -> List.length es > O.max_ins) ess)
      && begin
          match rej with
          | _::_ ->
             let es = List.flatten ess  in
             not (List.exists (fun xs -> substringp xs es) rej)
          | [] -> true
        end

    let rec zyva_prefix prefixes aset relax safe reject n f k =
      match prefixes with
      | [] -> k
      | pref::rem ->
         zyva pref aset relax safe reject n f
            (zyva_prefix rem aset relax safe reject n f k )

    let do_gen relax safe rej n =
      let sset = C.R.Set.of_list safe in
      let rset = C.R.Set.of_list relax in
      let aset = C.R.Set.union sset rset in
      let rej = List.map (fun a -> edges_of a) rej in
      D.all
        ~check:(last_minute rej)
        (fun f ->
          zyva_prefix prefixes aset relax safe rej n
            (last_check_call rej aset f))

    let debug_rs chan rs =
      List.iter (fun r -> fprintf chan "%s\n" (pp_relax r)) rs

    let secret_gen relax safe reject n =
      let r_nempty = Misc.consp relax in
      let relax = expand_relaxs C.ppo relax
      and safe = expand_relaxs C.ppo safe
      and reject = expand_relaxs C.ppo reject in
      if Misc.nilp relax then if r_nempty then begin
        Warn.fatal "relaxations provided in relaxlist could not be used to generate cycles"
      end ;
      if O.verbose > 0 then begin
        eprintf "** Relax0 **\n" ;
        debug_rs stderr relax ;
        eprintf "** Safe0 **\n" ;
        debug_rs stderr safe
      end ;
      let relax_set = C.R.Set.of_list relax
      and safe_set = C.R.Set.of_list safe in
      let relax = C.R.Set.elements relax_set
      and safe = C.R.Set.elements (C.R.Set.diff safe_set relax_set)
(*      and reject = C.R.Set.elements reject_set *)in
      if O.verbose > 0 then begin
        eprintf "** Relax **\n" ;
        debug_rs stderr relax ;
        eprintf "** Safe **\n" ;
        debug_rs stderr safe
      end ;
      do_gen relax safe reject n

(**********************)
(* Default edge lists *)
(**********************)

    let fold_ie f k = f (Int) (f (Ext) k)
    let fold_dir f k = f Irr k (* expand later ! *)
    let fold_dir2 f = fold_dir (fun i1 k -> fold_dir (f i1) k)
    let fold_sd f k = f (Same) (f Diff k)
    let fold_sd_dir2 f =
      fold_sd
        (fun sd -> fold_dir2 (fun d1 d2 -> f sd d1 d2))
    let fold_all_fences f =
      fold_sd_dir2 (fun sd d1 d2 -> C.A.fold_all_fences (fun fe -> f fe sd d1 d2))
    let fold_cumul_fences f =
      fold_sd_dir2 (fun sd d1 d2 -> C.A.fold_cumul_fences (fun fe -> f fe sd d1 d2))
    let fold_cum f =  fold_cumul_fences f

    let er e = ERS [plain_edge e]
    let safe =
      let k = [] in
      let k = fold_ie (fun ie k -> er (Ws ie)::er (Fr ie)::k) k in k

    let relax =
      let k = [] in
      let k = fold_dir2 (fun d1 d2 k -> er (Po (Diff, d1, d2))::k) k in
      let k = er (Po (Same, Dir R, Dir R))::k in
      let k = fold_all_fences (fun fe sd d1 d2 k -> er (Fenced (fe,sd,d1,d2))::k) k in
      let k =
        C.A.fold_dpr
          (fun dp k ->
            fold_sd (fun sd k -> er (Dp(dp,sd,Dir R))::k) k) k in
      let k =
        C.A.fold_dpw
          (fun dp k ->
            fold_sd (fun sd k -> er (Dp(dp,sd,Dir W))::k) k) k in
      let k = fold_ie (fun ie k -> er (Rf ie)::k) k in
      let k = fold_cum (fun fe sd d1 d2 k -> ac_fence fe sd d1 d2::k) k in
      let k = fold_cum (fun fe sd d1 d2 k -> bc_fence fe sd d1 d2::k) k in
      let k = er (Hat)::k in
      k

    let gen ?(relax=relax) ?(safe=safe) ?(reject=[]) n =
      try secret_gen relax safe reject n
      with e ->
        eprintf "Exc: '%s'\n" (Printexc.to_string e) ;
        raise e
  end
