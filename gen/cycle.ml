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

module type S = sig
  type edge
  type atom

  type event =
      { loc : loc ;
        v   : v ;
        dir : dir ;
        proc : Code.proc ;
        atom : atom option ;
        rmw : bool ;        
        idx : int ; }

  val evt_null : event
  val make_wsi : int -> Code.loc -> event

  val debug_evt : event -> string

  module OrderedEvent : Set.OrderedType with type t = event

  module EventMap : MyMap.S with type key = event
  val union_map : 'a EventMap.t -> 'a EventMap.t -> 'a EventMap.t

  type node = {
      mutable evt : event ;
      mutable edge : edge ;
      mutable next : node ;
      mutable prev : node ;
      mutable detour : node ;
      mutable store : node  ;
    } 
  val nil : node

(* Re-extract edges out of cycle *)
  val extract_edges : node -> edge list

(* Resolve edge direction and build cycle *)
  val resolve_edges : edge list -> edge list * node

(* Finish edge cycle, adding complete events *)
  val finish : node -> unit

(* Composition of the two more basic steps above *)
  val make : edge list -> edge list * node

(* split cycle amoungst processors *)
  val split_procs : node -> node list list


(* Return coherence orders *)
  val coherence : node -> (loc * (node * IntSet.t) list list) list

(* Get all detour events *)
  val get_detours : node -> node list
  val get_detours_from_list : node list -> node list

end

module type Config = sig
  val coherence_decreasing : bool
  val same_loc : bool
  val verbose : int
(* allow threads s.t start -> end is against com+ *)
  val allow_back : bool
end

module Make (O:Config) (E:Edge.S) :
    S with type edge = E.edge and type atom = E.atom
= struct

  type edge = E.edge
  type atom = E.atom

  type event =
      { loc : loc ;
        v   : v ;
        dir : dir ;
        proc : Code.proc ;
        atom : atom option ;
        rmw : bool ;
        idx : int ; }

  let evt_null = 
    { loc="*" ; v=(-1) ; dir=R; proc=(-1); atom=None; rmw=false; idx=(-1); }

  let make_wsi idx loc =
    { evt_null with dir=W ; loc=loc; idx=idx; v=0;}
  module OrderedEvent = struct
    type t = event
    let compare e1 e2 = Misc.int_compare e1.idx e2.idx
  end

  module EventMap = MyMap.Make(OrderedEvent)

  let union_map m1 m2 =
    EventMap.union
      (fun _n1 _n2 -> assert false)
      m1 m2

  type node = {
      mutable evt : event ;
      mutable edge : edge ; (* NB evt is the source of edge *)
      mutable next : node ;
      mutable prev : node ;
      mutable detour : node ;
      mutable store : node  ;
    }

  let debug_dir d = match d with W -> "W" | R -> "R"

  let debug_atom a =
    match a with None -> "" | Some a -> E.pp_atom a

  let debug_evt e =
    sprintf "%s%s %s %i" (debug_dir e.dir) (debug_atom e.atom) e.loc e.v

let debug_edge = E.pp_edge


let rec nil =
  {
   evt = evt_null ;
   edge = E.plain_edge (E.Po (Diff,Irr,Irr)) ;
   next = nil ;
   prev = nil ;
   detour = nil ;
   store = nil ;
  }

let debug_node chan n =
  if n.store != nil then
    fprintf chan "[Store: %s]" (debug_evt n.store.evt) ;
  fprintf chan "%s -%s->"
    (debug_evt n.evt) (debug_edge n.edge)

  let debug_nodes chan ns =
    let rec iter chan = function
      | [] -> ()
      | [n] -> debug_node chan n
      | n::ns -> fprintf chan "%a,%a" debug_node n iter ns in
    iter chan ns

 let debug_cycle chan n =
  let rec do_rec m =
    debug_node chan m ;
    output_char chan '\n' ;
    if m.next != n then do_rec m.next in
  do_rec n ;
  flush chan



let do_alloc_node idx e = 
  {
   evt = { evt_null with idx= idx ;} ;
   edge = e ;
   next = nil ; 
   prev = nil ;
   detour = nil ;
   store = nil ;
  }

let alloc_node idx e =
  let n = do_alloc_node idx e in
  if E.is_detour e then begin
    let m = do_alloc_node (idx+1) e in
    n.detour <- m ;
    n,idx+2
  end else
    n,idx+1

(* Add a node to non-empty cycle *)

let cons_cycle n c =
  n.next <- c ;
  n.prev <- c.prev ;
  c.prev.next <- n ;
  c.prev <- n ;
  n

let set_detours n =
  let rec do_rec m =
    if E.is_detour m.edge then begin
        let d = m.detour in
        d.prev <- m ;
        d.next <- m.next
    end ;
    if m.next != n then do_rec m.next in
  do_rec n

let do_remove_store n =
  let rec do_rec m =
    if E.is_store m.edge then begin
      let nxt = m.next and prev = m.prev in
      nxt.prev <- prev ;
      prev.next <- nxt ;
      nxt.store <- m ;
      m.evt <- { m.evt with dir=W;}
    end ;
    if m.next != n then do_rec m.next in
  do_rec n

let remove_store n =
  let rec find_not_store m =
    if E.is_store m.edge then
      if m.next == n then Warn.fatal "All 'Store' cycle"
      else find_not_store m.next
    else m in
  let n = find_not_store n in
  do_remove_store n ;
  n

let check_balance = 
  let rec do_rec r = function
    | [] -> r = 0
    | e::es ->
        do_rec (match e.E.edge with E.Back _ -> r-1 | E.Leave _ -> r+1 | _ -> r) es in
  do_rec 0


let build_cycle =

  let rec do_rec idx es = match es with
  | [] -> assert false (* Empty cycle is absurd *)
  | [e] ->
      let n,_ = alloc_node idx e in
      n.next <- n ; n.prev <- n ;
      n
  | e::es ->
      let n,idx = alloc_node idx e in
      cons_cycle n (do_rec idx es) in

  fun es ->
    if not (check_balance es) then Warn.fatal "Leave/Back are not balanced" ;
    let c = do_rec 0 es in
    set_detours c ;
    let c = remove_store c in
    c


let find_node p n =
  let rec do_rec m =
    if p m then m
    else
      let m = m.next in
      if m == n then raise Exit
      else do_rec m in
  do_rec n


let find_edge p = find_node (fun n -> p n.edge)

let find_node_prev p n =
  let rec do_rec m =
    if p m then m
    else
      let m = m.prev in
      if m == n then raise Exit
      else do_rec m in
  do_rec n

let find_edge_prev p = find_node_prev (fun n -> p n.edge)

(* Add events in nodes *)

module Env = Map.Make(String)

let locs =
  let t = Array.make 26 "" in
  t.(0) <- "x" ;
  t.(1) <- "y" ;
  t.(2) <- "z" ;
  for k=0 to (26-3)-1 do
    t.(k+3) <- String.make 1 (Char.chr (Char.code 'a' + k))
  done ;
  t

let locs_len = Array.length locs
    
let make_loc n =
  if n < locs_len then locs.(n)
  else Printf.sprintf "x%02i" (n-locs_len)
  
let next_loc (loc0,vs) = make_loc loc0,(loc0+1,vs)

let same_loc e = match E.loc_sd e with
    | Same -> true
    | Diff -> false
let diff_loc e = not (same_loc e)

let same_proc e = E.get_ie e = Int
let diff_proc e = E.get_ie e = Ext

let find_prec n = n.prev

(* Coherence definition *)
let rec count_ws = function
  | [] -> 0
  | n::ns -> match n.evt.dir with
    | W -> 1+count_ws ns
    | R -> count_ws ns
      
let start_co =
  if O.coherence_decreasing then count_ws
  else (fun _ -> 1)

let next_co = 
  if O.coherence_decreasing then (fun v -> v-1)
  else (fun v -> v+1)


        
        
(****************************)
(* Add events in edge cycle *)
(****************************)

(* Put directions into edge component of nodes, for
   easier access *)
let patch_edges n =
  let rec do_rec m = 
    let e = E.set_src m.evt.dir (E.set_tgt m.next.evt.dir m.edge) in
    m.edge <- e ;
    if m.next != n then do_rec m.next in
  do_rec n

(* Set directions of events *)

let is_rmw_edge e = match e.E.edge with
| E.Rmw ->
    if E.compare_atomo e.E.a1 e.E.a2 = 0 then true
    else Warn.fatal "Non equal atomicities for RMW"
| _ -> false

let is_rmw d e = match d with
| R -> is_rmw_edge e.edge
| W -> is_rmw_edge e.prev.edge

let set_dir n0 =
  let rec do_rec p m =
    let d = match E.dir_tgt p.edge,E.dir_src m.edge with
    | Irr,Irr ->
        Warn.fatal "Ambiguous direction %s %s"
          (E.pp_edge p.edge) (E.pp_edge m.edge)
    | (Dir d,Irr)|(Irr,Dir d) -> d
    | Dir d1,Dir d2 ->
        if d1=d2 then d1
        else
          Warn.fatal "Impossible direction %s %s"
            (E.pp_edge p.edge) (E.pp_edge m.edge) in
    let a =
      let a2 = p.edge.E.a2 and a1 = m.edge.E.a1 in
      if E.compare_atomo a1 a2 = 0 then a1
      else
        if a1 = None && E.is_ext p.edge then a2
        else if a2 = None &&  E.is_ext m.edge then a1
        else
          Warn.fatal "Impossible atomicity %s %s"
            (E.pp_edge p.edge) (E.pp_edge m.edge) in
    let rmw = is_rmw d m in
    m.evt <- { m.evt with dir=d; atom=a; rmw=rmw} ;
    if m.next != n0 then do_rec p.next m.next in
  do_rec (find_prec n0) n0 ;
  patch_edges n0 ;
  if O.verbose > 1 then begin
    eprintf "DIRECTIONS\n" ;
    debug_cycle stderr n0
  end


(***************************)
(* Set locations of events *)
(***************************)

(* Loc is changing *)
let set_diff_loc st n0 =
  let rec do_rec st p m =
    let loc,st =
      if same_loc p.edge then p.evt.loc,st
      else next_loc st in    
    m.evt <- { m.evt with loc=loc; } ;
    if m.store != nil then begin
      m.store.evt <- { m.store.evt with loc=loc; }
    end ;
(*    eprintf "LOC SET: %a\n%!" debug_node m ; *)
    if m.next != n0 then do_rec st p.next m.next
    else begin
      if m.evt.loc = n0.evt.loc then
        Warn.fatal "Cannot get changing loc accros %s\n"
          (E.pp_edge m.edge) ;
      st
    end in
  let p = find_prec n0 in
  assert (not (same_loc p.edge)) ;
  do_rec st p n0

(* Loc is not changing *)
let set_same_loc st n0 =
  let loc,st = next_loc st in
  let rec do_rec m =
    m.evt <- { m.evt with loc=loc; } ;
    if m.store != nil then begin
      m.store.evt <- { m.store.evt with loc=loc; }
    end ;
    if m.next != n0 then do_rec m.next in
  do_rec n0 ;
  st



(* Set the values of write events *)

  let split_by_loc n =
    let rec do_rec m =
      let r =
        if m.next == n then begin
          assert (m.evt.loc <> m.next.evt.loc) ;
          [[]]
        end else do_rec m.next in
      if m.evt.loc =  m.next.evt.loc then match r with
      | ms::rem -> (m::ms)::rem
      | [] -> assert false
      else [m]::r in
    do_rec n

  let split_one_loc n =
    let rec do_rec m =
      m::
      if m.next == n then []
      else do_rec m.next in
    [do_rec n]


  let rec do_set_write_val next = function
    | [] -> ()
    | n::ns ->
        let next = 
          let st = n.store in
          if st == nil then next
          else begin
            st.evt <- { st.evt with v=next; } ;
            next_co next
          end in
        if E.is_detour n.edge then begin
          let m = n.detour in
          let v =  match n.evt.dir with
          | R -> next
          | W -> next_co next in
          m.evt <- { m.evt with dir=W; loc=n.evt.loc; v=v ;};              
          begin match n.evt.dir with
          | W ->
              n.evt <- { n.evt with v = next; } ;
              do_set_write_val (next_co (next_co next)) ns
          | R ->  do_set_write_val (next_co next) ns
          end
        end else begin match n.evt.dir with
        | W ->
            n.evt <- { n.evt with v = next; } ;
            do_set_write_val (next_co next) ns
        | R ->  do_set_write_val next ns
        end

  let set_all_write_val nss =
    List.iter
      (fun ns ->
        do_set_write_val (start_co ns) ns)
      nss
  
  let set_write_v n =
    let nss =
      try 
        let m =
          find_node
            (fun m ->
              m.prev.evt.loc <> m.evt.loc &&
              m.next.evt.loc = m.evt.loc) n in
        split_by_loc m
      with Exit -> try
        let m = 
          find_node
            (fun m -> match m.prev.edge.E.edge with
            | E.Fr _|E.Rf _|E.RfStar _|E.Ws _|E.Leave _|E.Back _
            | E.Hat|E.Rmw|E.Detour _|E.DetourWs _ -> true
            | E.Po _|E.Dp _|E.Fenced _ -> false
            | E.Store -> assert false) n in
        split_one_loc m
      with Exit -> Warn.fatal "Cannot set write values" in
    set_all_write_val nss ;
    nss
  

(* TODO: this is wrong for Store CR's: consider Rfi Store PosRR *)
let do_set_read_v =

  let next n = match n.edge.E.edge with
  | E.Detour _ -> next_co n.evt.v
  | _ -> n.evt.v in

  let rec do_rec v = function
    | [] -> ()
    | n::ns ->
        let v = 
          if n.store == nil then v
          else n.store.evt.v in
        match n.evt.dir with
        | R ->
            n.evt <- { n.evt with v=v; } ;
            do_rec (next n) ns
        | W ->
            do_rec (next n) ns in
  do_rec 0

let set_read_v nss = List.iter do_set_read_v nss
(* zyva... *)      

let finish n =
  let st = 0,Env.empty in
(* Set locations *)
  let sd,n =
    let no =
      try Some (find_edge_prev diff_loc (find_edge_prev diff_proc n))
      with Exit -> None in
    match no with
    | Some n ->
        Diff,
        begin try find_edge same_loc n
        with Exit -> Warn.fatal "This cycle changes location at every step" end
    | None -> Same,n in

  let _nv,_st = 
    match sd with
    | Diff -> set_diff_loc st n
    | Same -> set_same_loc st n in

  if O.verbose > 1 then begin
    eprintf "LOCATIONS\n" ;
    debug_cycle stderr n
  end ;    
(* Set write values *)
  let by_loc = set_write_v n in
  if O.verbose > 1 then begin
    eprintf "WRITE VALUES\n" ;
    debug_cycle stderr n
  end ;    
(* Set load values *)
  set_read_v by_loc ;
  if O.verbose > 1 then begin
    eprintf "READ VALUES\n" ;
    debug_cycle stderr n
  end ;
  ()


(* Re-extract edges, with irelevant directions solved *)
let extract_edges n =
  let rec do_rec m =
    let k =
      if m.next == n then []
      else do_rec m.next in
    let k = m.edge::k in
    let k = if m.store != nil then  m.store.edge::k else k in
    k in
  do_rec n

let resolve_edges = function
  | [] -> Warn.fatal "No edges at all!"
  | es ->
      let c = build_cycle es in
      set_dir c ;
      extract_edges c,c

let make es =
  let es,c = resolve_edges es in
  finish c ;
  es,c

(*************************)
(* Gather events by proc *)
(*************************)

let find_start_proc n =
  if
    diff_proc n.prev.edge
  then n
  else    
    let n = find_edge (fun n -> diff_proc n) n in
    try find_edge same_proc n
    with Exit -> n


let cons_not_nil k1 k2 = match k1 with
| [] -> k2
| _::_ -> k1::k2


let find_proc t  n = 
  let rec array_rec j =
    assert (j < Array.length t) ;
    list_rec j t.(j)

  and list_rec j = function
    | [] -> array_rec (j+1)
    | m::ms -> if n == m then j else list_rec j ms in
  array_rec 0

let find_back n =

  let rec find_rec k m = match m.edge.E.edge with
  | E.Back _ ->
      if k = 0 then m
      else find_next (k-1) m
  | E.Leave _ ->
      find_next (k+1) m
  | _ -> find_next k m

  and find_next k m =
    if m.next == n then Warn.fatal "Non-matching Leave/Back"
    else find_rec k m.next in
  find_rec 0 n

      
let merge_changes n nss =
  let t = Array.of_list nss in
  let rec do_rec m =
    match m.edge.E.edge with
    | E.Leave _ ->
        let i = find_proc t m in
        let back = find_back m.next in
        let j = find_proc t back.next in
        if i=j then Warn.fatal "Useless Leave/Back" ;
        t.(i) <- t.(i) @ t.(j) ;
        t.(j) <- [] ;
        do_next m
    | _ -> do_next m

  and do_next m = if m.next != n then do_rec m.next in

  do_rec n ;
  List.filter Misc.consp (Array.to_list t)

  let value_before v1 v2 =
    if O.coherence_decreasing then v1 > v2 else v1 < v2


  let proc_back ns = match ns with
  | []|[_] -> false
  | fst::rem ->
      let lst = Misc.last rem in
      let e1 = fst.evt and e2 = lst.evt in
      e1.loc = e2.loc && value_before e2 e1

  let split_procs n =
    let n =
      try find_start_proc n
      with Exit -> Warn.fatal "Cannot split in procs" in
    let rec do_rec m =
      let k1,k2 =
        if m.next == n then begin
          if same_proc m.edge then
            Warn.fatal "%s at proc end" (debug_edge m.edge)
          else
            [],[]
        end else do_rec m.next in
      if same_proc m.edge then
        m::k1,k2
      else
        [m],cons_not_nil k1 k2 in
    let k1,k2 = do_rec n in
    let nss = cons_not_nil k1 k2 in
    let nss = merge_changes n nss in
    let rec num_rec k = function
      | [] -> ()
      | ns::nss ->
          List.iter
            (fun n -> n.evt <- { n.evt with proc = k; })
            ns ;
          num_rec (k+1) nss in
    num_rec 0 nss ;
    if
      not O.allow_back &&
      List.exists proc_back nss
    then Warn.fatal "Forbidden po vs. com" ;
    nss


(****************************)
(* Compute coherence orders *)
(****************************)

let rec group_rec x ns = function
  | [] -> [x,List.rev ns]
  | (y,n)::rem ->
      if String.compare x y = 0 then group_rec x (n::ns) rem
      else (x,List.rev ns)::group_rec  y [n] rem

  let group = function
    | [] -> []
    | (x,n)::rem -> group_rec x [n] rem

  let by_loc xvs =
    let r = group xvs in
    let r =  List.stable_sort (fun (x,_) (y,_) -> String.compare x y) r in
    let r =
      List.map
        (fun (x,ns) -> match ns with
        |  [] -> assert false
        | _::_ -> (x,ns))
        r in
    group r
        
      
      
(* find changing location *)
  let find_change n =
    let rec do_rec m =
      if m.evt.loc <> m.next.evt.loc then Some m.next
      else if m.next == n then 
        None
      else do_rec m.next in
    do_rec n
      

  let get_writes n =
    let rec do_rec m =
      let k =
        if m.next == n then []
        else do_rec m.next in
      let e = m.evt in
      let k =
        if E.is_detour m.edge then
          (e.loc,m.detour)::k
        else k in
      let k =  match e.dir with
      | W -> (e.loc,m)::k
      | R -> k in
      let st = m.store in
      if st != nil then (st.evt.loc,st)::k
      else k in
      
    do_rec n

  let get_observers n =
    let e = n.evt in
    assert (e.dir = W) ;
    let k = IntSet.empty in
    let k = if e.proc >= 0 then IntSet.add e.proc k else k in
    let k = match n.edge.E.edge with
    | E.Rf _ -> IntSet.add n.next.evt.proc k
    | E.Detour (Dir _) when n.detour == nil ->   IntSet.add n.next.evt.proc k
    | _ -> k in
    k

  let coherence n =
    let r = match find_change n with
    | Some n ->
        let ws = get_writes n in
(*
        List.iter
          (fun (loc,n) ->
            eprintf "LOC=%s, node=%a\n" loc debug_node n)
          ws ;
*)
        let r = by_loc ws in
        List.fold_right
          (fun (loc,ws) k -> match ws with
          | [] -> k
          | [_] -> (loc,ws)::k
          | _ ->
              List.iter
                (fun ns -> eprintf "[%a]\n" debug_nodes ns)
                ws ;
              assert false)
          r []
    | None ->
        if O.same_loc then
          match get_writes n with
          | [] -> []
          | (loc,_)::_ as xs ->
              [loc,[List.map snd xs]]
        else
          Warn.fatal "Unique location" in
    List.map
      (fun (loc,ns) ->
        loc,
        List.map
          (List.map (fun n -> n,get_observers n))             
          ns)
      r

(* Get all detour events *)
  let get_detours m =
    let rec do_rec k n =
      let k =
        if E.is_detour n.edge then n.detour::k
        else k in
      if n.next == m then k
      else do_rec k n.next in
    do_rec [] m

  let get_detours_from_list  ns =
    List.fold_right
      (fun n k ->
        if E.is_detour n.edge then n.detour::k
        else k)
      ns []
end
