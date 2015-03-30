(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Printf
open Code


module type AltConfig = sig 
  include DumpAll.Config
  val upto : bool
  val max_ins : int
  val choice : check
  type relax
  val prefix : relax list

  type fence
  val cumul : fence list Config.cumul
end

module Make(C:Builder.S)
    (O:AltConfig with type relax = C.R.relax and type fence = C.A.fence) :
    sig
      val gen : ?relax:C.R.relax list -> ?safe:C.R.relax list -> int -> unit
    end

    =
  struct

    module D = DumpAll.Make(O) (C)
    open C.E
    open C.R

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

    let choice_sc (po_safe,fence_safe) e1 e2 =
      let r = match e1.edge,e2.edge with
(*
  Now reject all po;po, except for Rfi.
  A bit rude, maybe...
 *)
(* Assuming Dp is safe *)
    | Rf Int,Dp _| Dp _,Rf Int -> true
    | Dp _,Ws Int | Dp _,Fr Int ->        
        not (po_safe (dir_src e1) (dir_tgt e2))
    | Po _, Dp _ ->
        not (po_safe (dir_src e1) (dir_tgt e1)) &&
        not (po_safe (dir_src e1) (dir_tgt e2))
    | Dp _,Po _ ->
        not (po_safe (dir_src e2) (dir_tgt e2)) &&
        not (po_safe (dir_src e1) (dir_tgt e2))
(* Check Po is safe *)
    | Po _,Po _ ->
        not (po_safe (dir_src e1) (dir_tgt e2))
    | Rf Int,Po _ ->
        po_safe (dir_src e2) (dir_tgt e2) &&
        not (po_safe (dir_src e1) (dir_tgt e2))
    | Po _,Rf Int ->
        po_safe (dir_src e1) (dir_tgt e1) &&
        not (po_safe (dir_src e1) (dir_tgt e2))
(* fenced s W R + Dp *)
    | Fenced(_,Same,_,Dir R), Dp (_,_,Dir R) ->
        not (fence_safe (dir_src e1) (dir_tgt e2))
(* Dp + fenced d R _ *)
    | Dp (_,_,Dir R),Fenced (_,Same,Dir R,_) ->
        not (fence_safe (dir_src e1) (dir_tgt e2))
(* Allow Rmw *)
    | (Rmw,_)|(_,Rmw) -> true
(* Added *) 
    | _,_ ->
        match get_ie e1, get_ie e2 with
        | Int,Int -> false
        | Ext,_|_,Ext -> true  in
(*      eprintf "Choice: %s %s -> %b\n" (C.E.pp_edge e1) (C.E.pp_edge e2) r ; *)
      r

(* Check altenance of com/po *)              
    let choice_critical e1 e2 =
      let r = 
        match e1.edge,e2.edge with
(* Two cases of allowed com composition *)
        | (Ws _|Leave CWs|Back CWs|Fr _|Leave CFr|Back CFr),
          (Rf _|Leave CRf|Back CRf) -> true
(* Rmw allowed to compose arbitrarily *)
        | (Rmw,_)|(_,Rmw) -> true
(* Otherwise require alternance *)
        | _,_ ->  C.E.get_ie e1 <> C.E.get_ie e2 in
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
    | Sc -> (fun _safes po_safe _xs _ys -> choice_sc po_safe)
    | Critical -> iarg choice_critical
    | Uni -> iarg choice_uni
    | Thin |Total -> iarg choice_id
    | Free -> iarg choice_free_alt
    | Ppo -> iarg choice_ppo
    | Transitive ->
        (fun safes _po_safe -> choice_transitive safes)


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
(* Fence cumulativity *)
    | Rf _,Fenced (f,_,_,_)
    | Fenced (f,_,_,_),Rf _ ->
        is_cumul f && choose O.choice safes po_safe xs ys e1 e2
    | _,_ -> choose O.choice safes po_safe xs ys e1 e2

    let do_compat safes po_safe xs ys =
      let x = Misc.last xs and y = List.hd ys in
      C.E.can_precede x y && pair_ok safes po_safe xs ys x y

    let can_precede safes po_safe (_,xs) k = match k with
    | [] -> true
    | (_,ys)::_ -> do_compat safes po_safe xs ys

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
    let sz _r = 1

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
    let prefix_expanded = C.R.expand_relax_seq O.prefix
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

    let call_rec prefix f0 safes po_safe over n r suff f_rec k =
      if
        can_precede safes po_safe r suff &&
        minprocs suff <= O.nprocs &&
        minint (r::suff) <= O.max_ins-1
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
            try f0 po_safe (prefix@suff) k
            with e ->
              eprintf "Exc in F0: '%s'\n" (Printexc.to_string e) ;
              raise e
          end else k in
        if n <= 0 then k
        else f_rec n suff k
      else k

    module Dir2Set =
      MySet.Make
        (struct
          type t = extr * extr
          let compare = Pervasives.compare
        end)

    let extract_po rs =
      let d2 =
        List.fold_right
          (fun (r,_) k -> match r with
          | ERS [{edge=Po (_,e1,e2);}] -> Dir2Set.add (e1,e2) k
          | _ -> k)
          rs Dir2Set.empty in
      fun e1 e2 -> Dir2Set.mem (e1,e2) d2

    let extract_fence rs =
      let d2 =
        List.fold_right
          (fun (r,_) k -> match r with
          | ERS [{edge=Fenced (_,_,e1,e2)}] -> Dir2Set.add (e1,e2) k
          | _ -> k)
          rs Dir2Set.empty in
      fun e1 e2 -> Dir2Set.mem (e1,e2) d2


    let zyva prefix aset relax safe n f =
(*      let safes = C.R.Set.of_list safe in *)
      let relax = edges_ofs relax in
      let safe = edges_ofs safe in
      let po_safe = extract_po safe in
      let fence_safe = extract_fence safe in
      let po_safe = po_safe,fence_safe in

      let rec choose_relax rs k = match rs with
      | [] -> k
      | r0::rs -> (* Build simple cycles for relaxation r0 *)

          let call_rec = call_rec prefix (f [fst r0]) aset po_safe  in

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
              if List.length rs > !Config.max_relax then k
              else f rs po_safe suff k)
            aset po_safe in
        
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
      let call_rec = call_rec prefix (f []) aset po_safe in
      let rec no_relax ss n suf k = match ss with
      | [] -> k
      | s::ss ->
          let k = call_rec true n s suf (no_relax safe) k in
          no_relax ss n suf k in

      fun k -> match relax with
      | [] ->
          no_relax safe n [] k
      | _  ->
          if !Config.mix && !Config.max_relax > 1 then
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


    let last_check_call _rset _sset aset f rs po_safe res k =
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
                | Sc | Ppo -> true
                | Thin | Free | Uni | Critical | Transitive |Total -> false) &&
                (count_ext le=1 || all_int le || count_changes le < 2) then k
              else begin
                let mk_info _es =
                  let ss = build_safe rs res in
                  let info =
                    [
                     "Relax",pp_relax_list rs;
                     "Safe", pp_relax_list ss;
                   ] in
                  info,C.R.Set.of_list rs in
                f le mk_info D.no_name k
              end
            with Normaliser.CannotNormalise -> k
          else k

    let last_minute ess =
      not (List.exists (fun es -> List.length es > O.max_ins) ess)

    let rec zyva_prefix prefixes aset relax safe n f k =
      match prefixes with
      | [] -> k
      | pref::rem ->
         zyva pref  aset relax safe n f
            (zyva_prefix rem aset relax safe n f k)

    let do_gen relax safe n =
      let sset = C.R.Set.of_list safe in
      let rset = C.R.Set.of_list relax in
      let aset = C.R.Set.union sset rset in
      D.all
        ~check:last_minute
        (fun f ->
          zyva_prefix prefixes aset relax safe n
            (last_check_call rset sset aset f))

    let debug_rs chan rs =
      List.iter (fun r -> fprintf chan "%s\n" (pp_relax r)) rs

    let secret_gen l1 l2 n =
      let l1 = expand_relaxs C.ppo l1
      and l2 = expand_relaxs C.ppo l2 in
      let s1 = C.R.Set.of_list l1
      and s2 = C.R.Set.of_list l2 in
      let l1 = C.R.Set.elements (C.R.Set.diff s1 s2) in
      if O.verbose > 0 then begin
        eprintf "** Relax **\n" ;
        debug_rs stderr l1 ;
        eprintf "** Safe **\n" ;
        debug_rs stderr l2
      end ;
      do_gen l1 l2 n

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
        
    let gen ?(relax=relax) ?(safe=safe) n =
      try secret_gen relax safe n
      with e ->
        eprintf "Exc: '%s'\n" (Printexc.to_string e) ;
        raise e
  end
