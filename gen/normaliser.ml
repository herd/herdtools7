(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)
open Code
open Printf


exception CannotNormalise

module type Config = sig
  val lowercase : bool
end

module Make : functor (C:Config) -> functor (E:Edge.S) ->
  sig
(* Normalise, return normalised cycle *)
    val normalise : E.edge list ->  E.edge list
(* Return family name, without normalising *)
    val family : E.edge list -> string
(* Both *)
    val normalise_family : E.edge list ->  string * E.edge list
  end
    =  functor (C:Config) -> functor (E : Edge.S) ->
  struct

    let debug = false

(* Cycles of edges *)
    module CE = struct
      type t =
          { edge : E.edge ; mutable dir : dir ;
            mutable next : t ; mutable prev : t;
            mutable matches : t ; }

      let e0 = E.parse_edge "Rfi"

      let rec nil =
        { edge = e0; dir=R ;  next = nil ; prev = nil ; matches = nil ; }


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
      | Dir d -> d
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
          if m.next == n then raise CannotNormalise
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

        let find_next_dir n =
          let rec f_rec m =
            if not (E.is_store m.edge) then m.dir
            else if m.next == n then assert false
            else f_rec m.next in
          f_rec n in

        let remove_store_dir n =
          let rec do_rec m =
            if E.is_store m.edge then m.dir <- find_next_dir m.next ;
            if m.next != n then do_rec m.next in
          do_rec n in

        let r = patch ms in
        remove_store_dir r ;
        set_matches r ;
        if debug then eprintf "CE.cycle returned [%s]\n%!" (_pp r) ;
        r


(* Notice, do not halt on stores... *)

      let _same_proc e = try E.get_ie e = Int with E.IsStore _ -> assert false
      let _diff_proc e = try E.get_ie e = Ext with E.IsStore _ -> assert false

      let ext_com e = match e.E.edge with
      | E.Rf Ext|E.Fr Ext|E.Ws Ext|E.Hat -> true
      | _ -> false

      exception NotFound

      let find_node p n =
        let rec do_rec m =
          if p m then m
          else
            let m = m.next in
            if m == n then raise NotFound
            else do_rec m in
        do_rec n

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

      let find_start_proc n =
        if debug then eprintf "Start proc [%s]\n%!" (_pp n) ;
        let n = find_out n in
        if debug then eprintf "Found out [%s]\n%!" (_pp n) ;
        if
          ext_com n.prev.edge
        then n
        else
          try
            let n = find_edge_out ext_com n in n.next
          with NotFound ->
            (* "No external communication in cycle" *)
               raise CannotNormalise

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
        | _,_ -> Pervasives.compare e1 e2

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
        match Pervasives.compare i1 i2 with
        | 0 ->  do_rec n1 n2
        | r -> r
    end
(* In/Out *)
    type points = One of dir | Two of dir * dir

    let order =
      [|
        One W;
        Two (W,W);
        Two (R,R);
        Two (R,W);
        Two (W,R);
        One R;
      |]

    let pp_points = function
      | One d -> pp_dir d
      | Two (d1,d2) -> pp_dir d1 ^ pp_dir d2

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
            mutable next : t ; mutable prev : t ; }

      let rec nil =
        { points = One R; cycle = CE.nil; prev = nil; next = nil; }

      let mk_cycle cy =
        let es = CE.mk_cycle cy in
        let eos = CE.split_procs es in
        let ms =
          List.map
            (fun (e,o) ->
              let p =
                if e == o then One e.CE.dir
                else Two (e.CE.dir,o.CE.dir) in
              { points=p; cycle=e; prev=nil; next=nil; })
            eos in
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

    end

    let pp_key key =
      let pp = match key with
      | "WW+RR" -> "MP"          
      | "WR+WR" -> "SB"
      | "WR+WR+WR" -> "3.SB"
      | "WR+WR+WR+WR" -> "4.SB"
      | "W+RW+RR" -> "WRC"
      | "W+RR+WR" -> "RWC"
      | "RW+RW" -> "LB"
      | "RW+RW+RW" -> "3.LB"
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
      | "WW+RW+RR" -> "ISA2"
      | "W+RR+W+RR" -> "IRIW"
      | "W+RR+W+RW"|"W+RW+W+RR" -> "IRRWIW"
      | "W+RW+W+RW" -> "IRWIW"
      | "WW+RW+WR" -> "Z6.0"
      | "WW+WW+RW" -> "Z6.1"
      | "WW+RW+RW" -> "Z6.2"
      | "WW+WW+RR" -> "Z6.3"
      | "WW+WR+WR" -> "Z6.4"
      | "WW+WW+WR" -> "Z6.5"
      | k -> k in
      if C.lowercase then String.lowercase pp else pp

    let normalise cy =
      try
        let ps = CP.mk_cycle cy in
        let ps = CP.norm ps in
        let cy = CE.map (fun e -> e.CE.edge) ps.CP.cycle in
        cy
      with CE.NotFound ->
        raise CannotNormalise

    let family cy =
      let ps = CP.mk_cycle cy in
      let key = CP.pp ps  in
      pp_key key
        
    let normalise_family cy =
      try
        let ps = CP.mk_cycle cy in
        let ps = CP.norm ps in
        let key = CP.pp ps  in
        let cy = CE.map (fun e -> e.CE.edge) ps.CP.cycle in
        pp_key key,cy
      with CE.NotFound ->
        raise CannotNormalise
  end
