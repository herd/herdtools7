(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2011-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
open Code
open Printf


exception CannotNormalise of string

module type Config = sig
  val lowercase : bool
end

module Make : functor (C:Config) -> functor (E:Edge.S) ->
  sig
(* Normalise, return normalised cycle *)
    val normalise : E.edge list ->  E.edge list
(* Return family name, without normalising *)
    val family : E.edge list -> string
(* Return number of procs in test *)
    val get_nprocs : E.edge list -> int
(* All at once *)
    val normalise_family : E.edge list ->  string * E.edge list * int
  end
    = functor (C:Config) -> functor (E : Edge.S) ->
  struct

    let debug = false

(* Cycles of edges *)
    module CE = struct
      type t =
          { edge : E.edge ; dir : dir option ;
            mutable next : t ; mutable prev : t;
            mutable matches : t ; }

      exception NotFound

      let find_node p n =
        let rec do_rec m =
          if p m then m
          else
            let m = m.next in
            if m == n then raise NotFound
            else do_rec m in
        do_rec n

      let find_node_rev p n =
        let rec do_rec m =
          if p m then m
          else
            let m = m.prev in
            if m == n then raise NotFound
            else do_rec m in
        do_rec n

      let e0 = E.parse_edge "Rfi"

      let rec nil =
        { edge = e0; dir=None ;  next = nil ; prev = nil ; matches = nil ; }


      let map f n =
        let rec do_rec m =
          let y = f m in
          let m = m.next in
          if m == n then [y]
          else y::do_rec m in
        do_rec n

      let _edges n = map (fun n -> n.edge) n

      let  _pp n =
        let xs = map (fun n -> E.pp_edge n.edge) n in
        String.concat " " xs

      let proc_list (e,o) =
        let rec do_rec m =
          if m == o then [o]
          else m::do_rec m.next in
        do_rec e


      let _pp_proc c =
        let ns = proc_list c in
        let xs = List.map (fun n -> E.pp_edge n.edge) ns in
        String.concat " " xs

      let dir_src e = match E.dir_src e with
      | Dir d -> Some d
      | NoDir -> None
      | Irr -> Warn.fatal "Unresolved direction"

      let find_back n =

        let rec find_rec k m = match m.edge.E.edge with
        | E.Back _ ->
            if k = 0 then m
            else find_next (k-1) m
        | E.Leave _ ->
            find_next (k+1) m
        | _ -> find_next k m

        and find_next k m =
          (* Cycle is not well formed anyway *)
          if m.next == n then raise (CannotNormalise "find_back")
          else find_rec k m.next in
        find_rec 0 n

      let set_matches n =
        let rec do_rec m = match m.edge.E.edge with
        | E.Leave _ ->
(*            eprintf "LEAVE [%s]\n" (_pp m); *)
            let matches = find_back m.next in
(*            eprintf "BACK [%s]\n" (_pp matches); *)
            m.matches <- matches ;
            matches.matches <- m ;
            do_next m
        | _ -> do_next m
        and do_next m = if m.next != n then do_rec m.next in
        do_rec n

      let mk_cycle es =
        let ms =
          List.map
            (fun e ->
              { edge=e; dir=dir_src e;
                next=nil; prev=nil; matches=nil; })
            es in

        let patch = function
          | [] -> assert false
          | x::xs ->
              let rec do_rec prev = function
                | [] ->
                    prev.next <- x ; x.prev <- prev
                | y::ys ->
                    prev.next <- y ; y.prev <- prev ;
                    do_rec y ys in
              do_rec x xs ; x in

        let r = patch ms in
        set_matches r ;
        if debug then eprintf "CE.cycle returned [%s]\n%!" (_pp r) ;
        r


(* Notice, do not halt on stores... *)
      let ext_com e = match e.E.edge with
      | E.Rf Ext|E.Fr Ext|E.Ws Ext|E.Hat|E.Irf Ext|E.Ifr Ext -> true
      | _ -> false

(* Find skipping Leave/Back *)
      let find_node_out p n =
        let rec do_rec m =
          if debug then eprintf "FIND NODE OUT [%s]\n" (_pp m) ;
          if p m then m
          else do_next m

        and do_next m =
          let m = match m.edge.E.edge with
          | E.Leave _ -> m.matches.next
          | _ -> m.next in
          if m == n then raise NotFound
          else do_rec m in

        do_rec n

      let _find_edge p = find_node (fun n -> p n.edge)

      let find_edge_out p = find_node_out (fun n -> p n.edge)

(*  EXP *)
(*
      let find_back =
        find_edge
          (fun e -> match e.E.edge with E.Back _ -> true | _ -> false)

      let find_leave =
        find_edge
          (fun e -> match e.E.edge with E.Leave _ -> true | _ -> false)
*)
(* Find node at outermost level *)
      let find_out n =
        let rec do_rec (n0,_ as c0) d m =
          if debug then
            eprintf "OUTER %i [%s]\n%!" n0 (_pp m) ;
          match m.edge.E.edge with
          | E.Leave _ -> do_next c0 (d+1) m
          | E.Back _ ->
              let c0 =
                let d = d-1 in
                let d0,n0 = c0 in
                if d < d0 then begin
                  if debug then
                    eprintf "CHANGE: %s -> %s\n%!"
                      (E.pp_edge n0.edge) (E.pp_edge m.next.edge) ;
                  (d,m.next)
                end else c0 in
              do_next c0 (d-1) m
          | _ -> do_next c0 d m
        and do_next c d m =
          if m.next == n then snd c
          else do_rec c d m.next in
        do_rec (0,n) 0 n

(*
 * Warning:
 * split_proc below has to be the same as in cycle.ml
 * for diy to name tests properly when no familly is given.
 *)

      let find_non_pseudo_prev n = find_node_rev (fun n -> E.is_non_pseudo n.edge.E.edge) n

      let find_start_proc n =
        if debug then eprintf "Start proc [%s]\n%!" (_pp n) ;
        let n = find_out n in
        if debug then eprintf "Found out [%s]\n%!" (_pp n) ;
        let p = find_non_pseudo_prev n.prev in
        if
          ext_com p.edge
        then p.next
        else
          try
            let n = find_edge_out ext_com n in n.next
          with NotFound ->
            (* "No external communication in cycle" *)
               raise (CannotNormalise "find_start")

      let split_procs n =
        try
          let n = find_start_proc n in   (* n is the entry of a proc *)
          assert (ext_com n.prev.edge) ;
          let rec do_rec m =
            if debug then eprintf "REC: '%s'\n" (_pp m) ;
            let e = m in
            let o = find_edge_out ext_com m in
            if o.next == n then [(e,o)]
            else (e,o)::do_rec o.next in
          let ns = do_rec n in
          if debug then begin
            if debug then eprintf "Split -> %!" ;
            eprintf "[%i]\n" (List.length ns) ;
            List.iter (fun n -> eprintf "  %s\n" (_pp_proc n)) ns
          end ;
          ns
        with e ->
          if debug then
            eprintf "Exc in split_procs: '%s'\n" (Printexc.to_string e) ;
          raise e

      let compare_edges e1 e2 =
        let open E in
        match e1.edge,e2.edge with
        | (Po _|Rf _),(Fenced _|Dp _)
        | Dp _,Fenced _
          -> 1
        | (Fenced _|Dp _),(Po _|Rf _)
        | Fenced _,Dp _ -> -1
        | _,_ -> Misc.polymorphic_compare e1 e2

      let ninternals n =
        let rec do_rec r m =
          match E.get_ie m.edge with
          | Ext -> r
          | Int ->
              if m.next == n then r
              else do_rec (r+1) m.next in
        do_rec 0 n

      let compare_edges_cycle n1 n2 =
        let rec do_rec m1 m2 =
          match compare_edges m1.edge m2.edge with
          | 0 ->
              let m1 = m1.next and m2 = m2.next in
              if m1 == n1 && m2 == n2 then 0
              else begin
                assert (m1 != n1 && m2 != n2) ;
                do_rec m1 m2
              end
          | r -> r in
        let i1 = ninternals n1
        and i2 = ninternals n2 in
        match compare i1 i2 with
        | 0 ->  do_rec n1 n2
        | r -> r
    end
(* In/Out *)
    module Dir = struct
      type t = R | W | F | J
      let pp = function
        | R -> "R"
        | W -> "W"
        | F -> "F"
        | J -> "J"

      let tr e =
        let d = Misc.as_some e.CE.dir in
        let r =
          match d with
          | Code.W -> W
          | Code.R ->
              begin match e.CE.edge.E.edge with
              | E.Ifr _ -> F
              | _ ->
                  begin match e.CE.prev.CE.edge.E.edge with
                  | E.Irf _ -> F
                  | _ -> R
                  end
              end
          | Code.J -> J in
        if debug then
          eprintf "%s[%s] -> %s\n"
            (E.pp_edge e.CE.edge) (Code.pp_dir d)
            (pp r) ;
        r

    end

    type points = One of Dir.t | Two of Dir.t * Dir.t

    let order =
      let open Dir in
      [|
        One W;
        Two (W,W);
        Two (R,R);
        Two (R,F);
        Two (F,R);
        Two (F,F);
        Two (R,W);
        Two (F,W);
        Two (W,R);
        Two (W,F);
        One R;
        One F;
      |]

    let pp_points = function
      | One d -> Dir.pp d
      | Two (d1,d2) -> Dir.pp d1 ^ Dir.pp d2

    let t_id = Hashtbl.create 17

    let () =
      Array.iteri
        (fun k p -> Hashtbl.add t_id p k)
        order

    let id p =
      try Hashtbl.find t_id p
      with Not_found -> assert false

    let compare_points p1 p2 =
      let i1 = id p1 and i2 = id p2 in
      Misc.int_compare i1 i2

    module CP = struct
      type t =
          { points : points ; cycle : CE.t ;
            mutable next : t ; }

      let rec nil =
        { points = One Dir.R; cycle = CE.nil; next = nil; }

      let has_dir e = match e.CE.dir with
      | Some _ -> true
      | None -> false

      let is_actual_edge e = has_dir e && not (E.is_node e.CE.edge.E.edge)

      let mk_cycle cy =
        let es = CE.mk_cycle cy in
        let eos = CE.split_procs es in
        let ms =
          List.map
            (fun (e,o) ->
              let e = CE.find_node is_actual_edge e
              and o = CE.find_node_rev is_actual_edge o in
              let p =
                if e == o then One (Dir.tr e)
                else Two (Dir.tr e,Dir.tr o) in
              { points=p; cycle=e;  next=nil; })
            eos in
        let patch = function
          | [] -> assert false
          | x::xs ->
              let rec do_rec prev = function
                | [] ->
                    prev.next <- x ;
                | y::ys ->
                    prev.next <- y ;
                    do_rec y ys in
              do_rec x xs ; x in
        patch ms


      let map f n =
        let rec do_rec m =
          let y = f m in
          let m = m.next in
          if m == n then [y]
          else y::do_rec m in
        do_rec n

      let compare_points_cycle n1 n2 =
        let rec do_rec m1 m2 =
          match compare_points m1.points m2.points with
          | 0 ->
              let m1 = m1.next and m2 = m2.next in
              if m1 == n1 && m2 == n2 then 0
              else begin
                assert (m1 != n1 && m2 != n2) ;
                do_rec m1 m2
              end
          | r -> r in
        do_rec n1 n2

      let compare n1 n2 = match compare_points_cycle n1 n2 with
      | 0 -> CE.compare_edges_cycle n1.cycle n2.cycle
      | r -> r

      let norm n =
        let rec do_rec r m =
          let cmp = compare r m in
          let r = if cmp < 0 then r else m in
          let m = m.next in
          if m == n then r
          else do_rec r m in
        do_rec n n.next

      let pp n =
        let xs = map (fun n -> pp_points n.points) n in
        String.concat "+" xs

      let fold f y0 n  =
        let rec do_rec m y =
          let y = f m y in
          let m = m.next in
          if m == n then y
          else do_rec m y in
        do_rec n y0

      let size n = fold (fun _ n -> n+1) 0 n

    end

    let pp_key allsame key =
      let pp = match key with
      | "WW" when allsame -> "CoWW"
      | "RW" when allsame -> "CoRW1"
      | "W+RW" when allsame -> "CoRW2"
      | "W+WR" when allsame -> "CoWR"
      | "W+RR" when allsame -> "CoRR"
      | "WR" when allsame -> "CoWR0"
      | "WW+RR" -> "MP"
      | "WW+FF" -> "MP.FF"
      | "WW+RF" -> "MP.RF"
      | "WW+FR" -> "MP.FR"
      | "WR+WR" -> "SB"
      | "WF+WR" -> "SB.FR"
      | "WR+WF" -> "SB.RF"
      | "WF+WF" -> "SB.FF"
      | "WR+WR+WR" -> "3.SB"
      | "WF+WF+WF" -> "3.SB.FFF"
      | "WR+WR+WR+WR" -> "4.SB"
      | "W+RW+RR" -> "WRC"
      | "W+FW+RR" -> "WRC+F+RR"
      | "W+FW+FF" -> "WRC+F+FF"
      | "W+FW+FR" -> "WRC+F+FR"
      | "W+FW+RF" -> "WRC+F+RF"
      | "W+RW+FF" -> "WRC+R+FF"
      | "W+RW+FR" -> "WRC+R+FR"
      | "W+RW+RF" -> "WRC+R+RF"
      | "W+RR+WR" -> "RWC"
      | "W+FF+WF" -> "RWC.FF.F"
      | "W+RF+WF" -> "RWC.RF.F"
      | "RW+RW" -> "LB"
      | "FW+RW" -> "LB.FR"
      | "RW+FW" -> "LB.RF"
      | "FW+FW" -> "LB.FF"
      | "RW+RW+RW" -> "3.LB"
      | "FW+FW+FW" -> "3.LB.FFF"
      | "RW+RW+RW+RW" -> "4.LB"
      | "WW+WR" -> "R"
      | "W+RW+WR" -> "WRW+WR"
      | "W+RR+WW" -> "WRR+2W"
      | "WW+RW" -> "S"
      | "W+RW+RW" -> "WWC"
      | "WW+WW" -> "2+2W"
      | "WW+WW+WW" -> "3.2W"
      | "WW+WW+WW+WW" -> "4.2W"
      | "W+RW+WW" -> "WRW+2W"
      | "WW+RR+WR" -> "W+RWC"
      | "WW+FF+WF" -> "W+RWC.F.FF"
      | "WW+RW+RR" -> "ISA2"
      | "WW+FW+FF" -> "ISA2.F.FF"
      | "W+RR+W+RR" -> "IRIW"
      | "W+RR+W+RW"|"W+RW+W+RR" -> "IRRWIW"
      | "W+RW+W+RW" -> "IRWIW"
      | "WW+RW+WR" -> "Z6.0"
      | "WW+WW+RW" -> "Z6.1"
      | "WW+RW+RW" -> "Z6.2"
      | "WW+WW+RR" -> "Z6.3"
      | "WW+WR+WR" -> "Z6.4"
      | "WW+WW+WR" -> "Z6.5"
      | "WW+FW" -> "S.F"
      | "WW+WF" -> "R.F"
      | "W+RF+WR " -> "RWC.RF.R"
      | "WW+FW+FR" -> "ISA2.F.FR"
      | "WW+FW+RF" -> "ISA2.F.RF"
      | "WW+FW+RR" -> "ISA2.F.RR"
      | "WW+RW+FF" -> "ISA2.R.FF"
      | "WW+RW+FR" -> "ISA2.R.FR"
      | "WW+RW+RF" -> "ISA2.R.RF"
      | "RW+FW+FW" -> "3.LB.R.F.F"
      | "RW+RW+FW" -> "3.LB.R.R.F"
      | "WR+WF+WF" -> "3.SB.R.F.F"
      | "WR+WR+WF" -> "3.SB.R.R.F"
      | "WW+FW+WF" -> "Z6.0.F.F"
      | "WW+FW+WR" -> "Z6.0.F.R"
      | "WW+RW+WF" -> "Z6.0.R.F"
      | "WW+WW+FW" -> "Z6.1.F"
      | "WW+FW+FW" -> "Z6.2.F.F"
      | "WW+FW+RW" -> "Z6.2.F.R"
      | "WW+RW+FW" -> "Z6.2.R.F"
      | "WW+WW+FF" -> "Z6.3.FF"
      | "WW+WW+FR" -> "Z6.3.FR"
      | "WW+WW+RF" -> "Z6.3.RF"
      | "WW+WF+WF" -> "Z6.4.F.F"
      | "WW+WF+WR" -> "Z6.4.F.R"
      | "WW+WR+WF" -> "Z6.4.R.F"
      | "WW+WW+WF" -> "Z6.5.F"
      | "W+RW+FW " -> "WWC.R.F"
      | "W+RF+WW " -> "WRR+2W.RF"
      | "WW+FF+WR" -> "W+RWC.FF.R"
      | "WW+FR+WF" -> "W+RWC.FR.F"
      | "WW+FR+WR" -> "W+RWC.FR.R"
      | "WW+RF+WF" -> "W+RWC.RF.F"
      | "WW+RF+WR" -> "W+RWC.RF.R"
      | "WW+RR+WF" -> "W+RWC.RR.F"
      | k -> k in
      if C.lowercase then Misc.lowercase pp else pp

    let normalise cy =
      try
        let ps = CP.mk_cycle cy in
        let ps = CP.norm ps in
        let cy = CE.map (fun e -> e.CE.edge) ps.CP.cycle in
        cy
      with CE.NotFound ->
        raise (CannotNormalise "normalise")

    let allsame cy =
      List.for_all
        (fun e -> match E.loc_sd e with
        | Same -> true
        | Diff -> false)
        cy

    let family cy =
      let ps = CP.mk_cycle cy in
      let key = CP.pp ps  in
      pp_key (allsame cy) key

    let get_nprocs cy =
      let ps = CP.mk_cycle cy in
      CP.size ps

    let normalise_family cy =
      try
        let ps = CP.mk_cycle cy in
        let ps = CP.norm ps in
        let key = CP.pp ps  in
        let cy = CE.map (fun e -> e.CE.edge) ps.CP.cycle in
        pp_key (allsame cy) key,cy,CP.size ps
      with CE.NotFound ->
        raise (CannotNormalise "normalise_family")
  end
