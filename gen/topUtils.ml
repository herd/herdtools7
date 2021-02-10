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

module type Config = sig
  val optcoherence : bool
  val do_observers : Config.do_observers
  val obs_type : Config.obs_type
  val poll : bool
  val hexa : bool
end

module Make : functor (O:Config) -> functor (C:ArchRun.S) ->
  sig
(* Coherence utilities *)
    type cos0 =  (string * (C.C.node * IntSet.t) list list) list
    type cos = (string * (Code.v * IntSet.t) list list) list
    val pp_coherence : cos0 -> unit
    val last_map : cos0 -> C.C.event StringMap.t
    val compute_cos : cos0 ->  cos
(* prefetch *)
    type pt = { ploc:Code.loc ; pdir:Code.dir; } (* In thread/Out thread *)
    val io_of_thread : C.C.node list -> (pt * pt) option
    val io_of_detour : C.C.node -> (pt * pt) option
    val compile_prefetch_ios : int ->  (pt * pt) option list -> string
(* affinity *)
    val compile_coms : C.C.node list list -> string list
(* Misc *)
    val comp_loc_writes : C.C.node -> StringSet.t
    val comp_atoms : C.C.node -> StringSet.t
    val find_next_pte_write : C.C.node -> C.C.node option
    val check_here : C.C.node -> bool
    val do_poll : C.C.node -> bool
    val fetch_val : C.C.node -> Code.v
  end =
  functor (O:Config) -> functor (C:ArchRun.S) ->
  struct

    type cos0 =  (string * (C.C.node * IntSet.t) list list) list
    type cos = (string * (Code.v * IntSet.t) list list) list

    open Printf
    open Code

    let pp_coherence cos0 =
      eprintf "COHERENCE: " ;
      Misc.pp_list stderr ""
        (fun chan (x,vs) ->
          fprintf chan "<%s:%a>" x
            (fun chan ->
              Misc.pp_list chan "|"
                (fun chan ->
                  Misc.pp_list chan ","
                    (fun chan (n,obs) ->
                      let pp chan =
                        if O.hexa then fprintf chan "0x%x{%s}"
                        else fprintf chan "%i{%s}" in
                      pp chan n.C.C.evt.C.C.cell
                        (IntSet.pp_str "," (sprintf "%i") obs)
                    )))
            vs)
        cos0 ;
      eprintf "\n%!"

(****************************)
(* Last in coherence orders *)
(****************************)
    let rec find_last = function
      | [] -> assert false
      | [xs] -> Misc.last xs
      | _::xss -> find_last xss


    let do_last_map cos =
      let lsts =
        List.map
          (fun (loc,xss) ->
            let r,_ = find_last xss in
            loc,r)
          cos in
      List.fold_left
        (fun m (loc,lst) -> StringMap.add loc lst.C.C.evt m)
        StringMap.empty lsts

    let last_map cos = match O.do_observers with
    | Config.Local when O.optcoherence -> do_last_map cos
    | _ -> StringMap.empty


    let compute_cos =
      List.map
        (fun (loc,ns) ->
          loc,
          List.map
            (List.map (fun (n,obs) -> n.C.C.evt.C.C.cell,obs))
            ns)

(******************)
(* Prefetch hints *)
(******************)


(* In thread/Out thread *)
    type pt = { ploc:Code.loc ; pdir:Code.dir; }

    let io_of_node n =
      {ploc=n.C.C.evt.C.C.loc;
       pdir=Misc.as_some n.C.C.evt.C.C.dir;}

    let io_of_thread n = match n with
    | []|[_] -> None
    | n0::rem ->
        let n0 = C.C.find_non_insert n0
        and n1 = C.C.find_non_insert_prev (Misc.last rem) in
        Some (io_of_node n0,io_of_node n1)

    let io_of_detour _n = None

    let add_data f loc k = if Code.is_data loc then f loc::k else k

    let compile_prefetch_ios =

      let rec do_rec p = function
        | [] -> []
        | None::rem -> do_rec (p+1) rem
        | Some (i,o)::rem ->
            let k = do_rec (p+1) rem in
            if i.ploc = o.ploc then k
            else
              add_data
                (fun loc -> sprintf "%i:%s=F" p (Code.pp_loc loc))
                i.ploc
                (add_data
                   (fun loc ->
                     sprintf "%i:%s=%s" p
                       (Code.pp_loc loc)
                       (match o.pdir with W -> "W" | R -> "T" | J -> "I"))
                   o.ploc k) in

      fun fst ios -> String.concat "," (do_rec fst ios)

(******************)
(* Affinity hints *)
(******************)

(*  Most of placement computation is now by litmus *)

    let write_before m =
      let rec do_rec n =
        if m == n then false
        else
          let e = n.C.C.edge in
          match C.E.loc_sd e with
          | Same ->
              begin match  n.C.C.evt.C.C.dir with
              | Some W -> true
              | None|Some R|Some J -> do_rec n.C.C.prev
              end
          | Diff -> false in
      do_rec m.C.C.prev

    let write_after m =
      let rec do_rec n =
        let e = n.C.C.edge in
(*        eprintf "After %s\n" (C.E.pp_edge e) ; *)
        begin match  n.C.C.evt.C.C.dir with
        | Some W -> true
        | None|Some R|Some J ->
            let nxt = n.C.C.next in
            if nxt == m then false else
            begin match C.E.loc_sd e with
            | Same -> do_rec nxt
            | Diff -> false
            end
        end in
      do_rec m.C.C.next

    let last_edge ns =
      let n = Misc.last ns in
      let open C.E in
      match n.C.C.edge.C.E.edge with
      | Hat ->
          let wb = write_before n
          and wa = write_after n in
          begin match wb,wa with
          | true,true -> Ws Ext
          | true,false -> Rf Ext
          | false,true -> Fr Ext
          | false,false ->
              Warn.fatal "Incorrect Hat: read chains are not allowed"
          end
      | e -> e

    let compile_coms nss =
      List.map
        (fun ns ->
          let open C.E in
          match last_edge ns with
          | Fr _|Leave CFr|Back CFr -> "Fr"
          | Rf _|Leave CRf|Back CRf -> "Rf"
          | Ws _|Leave CWs|Back CWs -> "Co"
          | Irf _ -> "Irf" | Ifr _ -> "Ifr"
          | _ -> assert false)
        nss


(********)
(* Misc *)
(********)

(* Local writes *)

    let comp_loc_writes n0 =
      let rec do_rec n =
        let k =
          if n.C.C.next == n0 then StringSet.empty
          else do_rec n.C.C.next in
        let k =
          let e =  n.C.C.evt in
          match e.C.C.dir,e.C.C.loc with
          | Some W,Data loc -> StringSet.add loc k
          | ((Some R|None|Some J),_)|(Some W,Code _) -> k in
         k in
      do_rec n0

(* Atomic accesses *)
    let comp_atoms n0 =
      let rec do_rec n =
        let k =
          if n.C.C.next == n0 then StringSet.empty
          else do_rec n.C.C.next in
        let k =
          let e =  n.C.C.evt in
          match e.C.C.atom,e.C.C.loc with
          | (None,_)|(_,Code _) -> k
          | Some a,Data loc ->
              if C.A.worth_final a then
                StringSet.add loc k
              else k in
        k in
      do_rec n0

(* Worth inserting local check *)
    let find_next_pte_write n =
      let loc = n.C.C.evt.C.C.loc  in
      try
        let r =
          C.C.find_node
            (fun m ->
              let e = m.C.C.evt in
              if Code.loc_eq loc e.C.C.loc then match e.C.C.dir,e.C.C.bank with
              | Some W,Pte -> true
              | _,_ -> false
              else raise Not_found)
            n.C.C.next in
        Some r
      with Not_found -> None

    let is_load_init e = e.C.C.dir = Some R && e.C.C.v = 0

    let check_edge = function
      | C.E.Ws Ext
      | C.E.Fr Ext
      | C.E.Leave (CFr|CWs)
      | C.E.Back(CFr|CWs)  -> true
      | _-> false

    let check_here n = match n.C.C.evt.C.C.bank with
    | Pte ->
        Misc.is_some (find_next_pte_write n)
    | Ord|Tag|CapaTag|CapaSeal ->
        check_edge n.C.C.edge.C.E.edge && not (is_load_init n.C.C.evt)

(* Poll for value is possible *)
    let do_poll n =
      match O.poll,n.C.C.prev.C.C.edge.C.E.edge,n.C.C.evt.C.C.v with
      | true,
        (C.E.Rf Ext|C.E.Leave CRf|C.E.Back CRf),1 -> true
      | _,_,_ -> false

    let fetch_val n =
      match n.C.C.prev.C.C.edge.C.E.edge, n.C.C.edge.C.E.edge with
      | C.E.Irf _,_ -> 2
      | _,C.E.Ifr _ -> 1
      | _,_ -> 0
  end
