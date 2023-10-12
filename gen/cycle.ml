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

module type S = sig
  type fence
  type edge
  module SIMD : Atom.SIMD
  type atom
  module PteVal : PteVal_gen.S with type pte_atom = atom

  type event =
      { loc : loc ; ord : int; tag : int ;
        ctag : int; cseal : int; dep : int ;
        v   : v ; (* Value read or written *)
        vecreg: v list list ; (* Alternative for SIMD *)
        ins : int ;
        dir : dir option ;
        proc : Code.proc ;
        atom : atom option ;
        rmw : bool ;
        cell : v array ; (* Content of memory, after event *)
        tcell : v array ; (* value of tag memory after event *)
        bank : SIMD.atom Code.bank ;
        idx : int ;
        pte : PteVal.t ; }

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
      mutable store : node ;
    }
  val nil : node
  val str_node : node -> string
  val debug_cycle : out_channel ->  node -> unit

(* Find, may raise Not_found *)
  val find_node : (node -> bool) -> node -> node
  val find_node_prev : (node -> bool) -> node -> node
(* First node a strict po-predecessor of second node. *)
  val po_pred : node -> node -> bool
  val find_prev_code_write : node -> string

  val find_edge : (edge -> bool) -> node -> node
  val find_edge_prev : (edge -> bool) -> node -> node

  val find_non_insert_store : node -> node
  val find_non_insert_store_prev : node -> node

  val find_non_pseudo : node -> node
  val find_non_pseudo_prev : node -> node

(* Generic fold *)
  val fold : (node -> 'a -> 'a) -> node -> 'a -> 'a

(* Extract wide accesses from cycle. Size as number of integers *)
  val get_wide : node -> int StringMap.t
(* Extract pair accesses from cycle. *)
  val get_pair : node -> StringSet.t

(* Re-extract edges out of cycle *)
  val extract_edges : node -> edge list

(* Resolve edge direction and build cycle *)
 val resolve_edges : edge list -> edge list * node

(* Finish edge cycle, adding complete events, returns initial environment *)
  val finish : node -> (string * Code.v) list

(* Composition of the two more basic steps above *)
  val make : edge list -> edge list * node * Code.env

(* split cycle amoungst processors *)
  val split_procs : node -> node list list

(* Return coherence orders *)
  val coherence : node -> (string * (node * IntSet.t) list list) list

(* Return last pteval in pte accesses coherence *)
  val last_ptes : node -> (string * PteVal.t) list

(* All locations *)
  val get_globals : ?init:Code.env -> node -> string list

(* All (modified) code labels *)
  val get_labels : node -> string list

end

module type Config = sig
  val same_loc : bool
  val verbose : int
(* allow threads s.t. start -> end is against com+ *)
  val allow_back : bool
  val naturalsize : MachSize.sz
  val hexa : bool
  val variant : Variant_gen.t -> bool
end

module Make (O:Config) (E:Edge.S) :
    S with type fence = E.fence
       and type edge = E.edge
       and module SIMD = E.SIMD
       and type atom = E.atom
       and module PteVal = E.PteVal
  = struct
  let dbg = false
  let do_memtag = O.variant Variant_gen.MemTag
  let do_morello = O.variant Variant_gen.Morello
  let do_kvm = Variant_gen.is_kvm O.variant
  let do_neon = O.variant Variant_gen.Neon

  type fence = E.fence
  type edge = E.edge
  module SIMD = E.SIMD
  type atom = E.atom
  module PteVal = E.PteVal

  type event =
      { loc : loc ; ord : int; tag : int;
        ctag : int; cseal : int; dep : int;
        v   : v ;
        vecreg: v list list ;
        ins : int ;
        dir : dir option ;
        proc : Code.proc ;
        atom : atom option ;
        rmw : bool ;
        cell : v array ; (* value of cell at node exit *)
        tcell : v array ; (* value of tag cell at node exit *)
        bank : SIMD.atom Code.bank ;
        idx : int ;
        pte : PteVal.t }

  let pte_default = PteVal.default "*"

  let evt_null =
    { loc=Code.loc_none ; ord=0; tag=0;
      ctag=0; cseal=0; dep=0;
      vecreg= [];
      v=(-1) ; ins=0;dir=None; proc=(-1); atom=None; rmw=false;
      cell=[||]; tcell=[||];
      bank=Code.Ord; idx=(-1);
      pte=pte_default; }

  let make_wsi idx loc = { evt_null with dir=Some W ; loc=loc; idx=idx; v=0;}

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
      mutable store : node ;
    }

  let debug_dir d = match d with
         Some W -> "W" | Some R -> "R" | Some J -> "J" | None -> "_"

  let debug_atom a =
    match a with None -> "" | Some a -> E.pp_atom a

  let debug_tag =
    if do_memtag then fun e -> sprintf " (tag=%i)" e.tag
    else fun _ -> ""

  let debug_morello =
    if do_morello then fun e ->
      sprintf " (ord=%i) (ctag=%i) (cseal=%i) (dep=%i)" e.ord e.ctag e.cseal e.dep
    else fun _ -> ""

  let debug_neon =
    if do_neon then
      let pp_one = Code.add_vector O.hexa in
      fun e ->
      sprintf " (vecreg={%s})"
        (String.concat "," (List.map pp_one e.vecreg))
    else fun _ -> ""

  let debug_val = Code.pp_v ~hexa:O.hexa

  let debug_vec v =
    String.concat ", " (List.map debug_val (Array.to_list v))

  let debug_evt e =
    let pp_v =
      match e.bank with
      | Pte -> PteVal.pp e.pte
      | (Ord|Pair|Tag|CapaTag|CapaSeal|VecReg _|Instr) -> debug_val e.v in
    sprintf "%s%s %s %s%s%s%s%s"
      (debug_dir e.dir)
      (debug_atom e.atom)
      (Code.pp_loc e.loc)
      (match debug_vec e.cell with
       | "" -> "" | s -> "cell=[" ^ s ^"] ")
      pp_v (debug_tag e) (debug_morello e) (debug_neon e)

  let debug_edge = E.pp_edge


  let rec nil =
    {
     evt = evt_null ;
     edge = E.plain_edge (E.Po (Diff,Irr,Irr)) ;
     next = nil ;
     prev = nil ;
     store = nil ;
    }

  let  debug_node chan n =
    if n.store != nil then begin
      let n = n.store in
      fprintf chan "[%s %s]"
        (debug_edge n.edge) (debug_evt n.evt)
    end ;
    fprintf chan "%s -%s->"
      (debug_evt n.evt) (debug_edge n.edge)

  let str_node n = sprintf "%s -%s->" (debug_evt n.evt) (debug_edge n.edge)

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
   evt = { evt_null with idx = idx ;} ;
   edge = e ;
   next = nil ;
   prev = nil ;
   store = nil ;
  }

let alloc_node idx e =
  let n = do_alloc_node idx e in
  n,idx+1

(* Add a node to non-empty cycle *)

let cons_cycle n c =
  n.next <- c ;
  n.prev <- c.prev ;
  c.prev.next <- n ;
  c.prev <- n ;
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
    c


let find_node p n =
  let rec do_rec m =
    if p m then m
    else
      let m = m.next in
      if m == n then raise Not_found
      else do_rec m in
  do_rec n


let find_node_prev p n =
  let rec do_rec m =
    if p m then m
    else
      let m = m.prev in
      if m == n then raise Not_found
      else do_rec m in
  do_rec n

(*  n and m are on the same thread, n being strictly before m *)
  let po_pred n m =
    if dbg then
      eprintf "po_pred: n=[%a], m=[%a]\n%!" debug_node n debug_node m ;
  let rec do_rec p =
    if dbg then eprintf "  pred_rec, node %a\n%!" debug_node p ;
    if p == m then true
    else if E.is_ext p.edge || p.next == n then false
    else do_rec p.next in
  do_rec n.next

let find_prev_code_write n =
  let rec do_rec m =
    if dbg then
      eprintf "find_prev_code_write, n=%a m=%a\n%!"
        debug_node n debug_node m ;
    let e = m.evt in
    match e.loc,E.safe_dir m.edge with
    | Code c,Some W ->
        (* Avoid the case where the cachesync is po-before the code write... *)
        begin if po_pred n m then raise Not_found end ;
        c
    | _,_ ->
        let m = m.prev in
        if m == n then raise Not_found
        else do_rec m in
  do_rec n


let find_edge p = find_node (fun n -> p n.edge)
let find_edge_prev p = find_node_prev (fun n -> p n.edge)

let non_insert_store e = not (E.is_insert_store e.E.edge)
let find_non_insert_store m = find_edge non_insert_store m
let find_non_insert_store_prev m = find_edge_prev non_insert_store m

let non_pseudo e = E.is_non_pseudo e.E.edge
let find_non_pseudo m = find_edge non_pseudo m
let find_non_pseudo_prev m = find_edge_prev non_pseudo m

let is_real_edge e =  non_pseudo e && non_insert_store e

let find_real_edge_prev = find_edge_prev is_real_edge

(* generic scan *)
  let fold f m k =
    let rec fold_rec n k =
      let k = f n k
      and nxt = n.next in
      if nxt == m then k
      else fold_rec nxt k in
    fold_rec m k

(* Get size (as integers) from annotations *)

  let as_integers n = match n.evt.loc with
      | Data loc ->
         begin
           match E.as_integers n.edge with
           | Some sz -> Some (loc,sz)
           | None -> None
         end
      | Code _ -> None

  let get_wide_list ns =
    List.fold_left
      (fun k n ->
        match as_integers n with
        | Some (_,n) -> max n k
        | None -> k)
    0 ns

  let get_wide m =
    fold
      (fun n k ->
        match as_integers n with
        | Some (loc,sz) ->
           let sz0 = StringMap.safe_find 0 loc k in
           StringMap.add loc (max sz0 sz) k
        | None-> k)
    m StringMap.empty

  let is_pair n = match n.evt.loc with
      | Data loc ->
         if E.is_pair n.edge then Some loc
         else None
      | Code _ -> None

  let get_pair m =
    fold
      (fun n k ->
        match is_pair n with
        | Some loc -> StringSet.add loc k
        | None -> k)
      m StringSet.empty

(* Add events in nodes *)

module Env = Map.Make(String)

let locs,next_x =
  let t = Array.make 26 "" in
  t.(0) <- "x" ;
  t.(1) <- "y" ;
  t.(2) <- "z" ;
  for k=0 to (26-3)-1 do
    t.(k+3) <- String.make 1 (Char.chr (Char.code 'a' + k))
  done ;
  t,t.(1)

let locs_len = Array.length locs

let make_loc n =
  if n < locs_len then locs.(n)
  else Printf.sprintf "x%02i" (n-locs_len)

let next_loc e ((loc0,lab0),vs) = match E.is_fetch e with
| true -> Code (sprintf "Lself%02i" lab0),((loc0,lab0+1),vs)
| _ -> 
  Code.Data (make_loc loc0),((loc0+1,lab0),vs)

let same_loc e = match E.loc_sd e with
    | Same -> true
    | Diff -> false

let diff_loc e = not (same_loc e)

let same_proc e = E.get_ie e = Int
let diff_proc e = E.get_ie e = Ext


(* Coherence definition *)

module CoSt = struct

  module M =
    MyMap.Make
      (struct type t = E.SIMD.atom Code.bank let compare = compare end)

  type t = { map : int M.t; co_cell : int array; }

  let (<<) f g = fun x -> f (g x)
  and (<!) f x = f x

  let create ?(init=0) sz =
    let map  =
      M.add Tag init <<  M.add CapaTag init <<
      M.add CapaSeal init << M.add Ord init << M.add Instr init <! M.empty
    and co_cell = Array.make (if sz <= 0 then 1 else sz) init in
    { map; co_cell;  }

  let find_no_fail key map =
    try M.find key map with Not_found -> assert false

  let get_co st bank = find_no_fail bank st.map

  let set_co st bank v =
    let b = match bank with VecReg _ -> Ord | _ -> bank in
    { st with map=M.add b v st.map; }

  let get_cell st = st.co_cell

  let set_cell st n =
    let e = n.evt in match e.bank with
    | Ord|Pair -> begin
       let old = st.co_cell.(0) in
       let co_cell = Array.copy st.co_cell in
       let cell2 =
         match n.prev.edge.E.edge with
         | E.Rmw rmw ->
           let old = E.extract_value old n.prev.evt.atom in
           E.compute_rmw rmw old e.v
         | _ -> e.v in
       begin
         match e.bank with
         | Ord ->
            co_cell.(0) <- E.overwrite_value old e.atom cell2
         | Pair -> (* No Rmw for pairs *)
            co_cell.(0) <- E.overwrite_value old e.atom (e.v-1);
            let old = st.co_cell.(0) in
            co_cell.(1) <- E.overwrite_value old e.atom e.v
         | _ -> assert false
       end ;
       {e with cell=co_cell;},{ st with co_cell; }
    end
    | _ -> e,st

  let set_tcell st e = match e.bank with
    | Tag ->
       {e with tcell=[| e.v; |];},st
    | _ -> e,st

  let next_co st bank =
   match bank with
   | VecReg n ->
      let v = find_no_fail Ord st.map in
      { st with map=M.add Ord (v+E.SIMD.nregs n) st.map; }
   | _ ->
      let v = find_no_fail bank st.map in
      { st with map=M.add bank (v+1) st.map; }

  let step_simd st n =
    let fst = find_no_fail Ord st.map in
    let lst = fst+E.SIMD.nregs n in
    { co_cell=E.SIMD.step n fst st.co_cell;
      map=M.add Ord lst st.map;}

end

let pte_val_init loc = match loc with
| Code.Data loc when do_kvm -> PteVal.default loc
| _ -> pte_default

(****************************)
(* Add events in edge cycle *)
(****************************)

(* Put directions into edge component of nodes, for easier access *)

let rec next_dir m = match m.next.evt.dir with
| None -> next_dir m.next
| Some d -> d

let patch_edges n =
  let rec do_rec m =
    let e = match  m.evt.dir with
    | None -> m.edge
    | Some d ->
        E.set_src d (E.set_tgt (next_dir m) m.edge) in
    m.edge <- e ;
    if m.next != n then do_rec m.next in
  do_rec n

(*  Merge annotations *)
  exception FailMerge

  let merge2 a1 a2 = match a1,a2 with
  | (None,a)|(a,None) -> a
  | Some a1,Some a2 ->
      match E.merge_atoms a1 a2 with
      | None -> raise FailMerge
      | Some _ as r -> r


  let merge_annotations m =
      let rec do_rec n =
        let e = n.edge in
        if non_insert_store e then begin
          let p = find_non_insert_store_prev n.prev in
          if O.verbose > 0 then Printf.eprintf "Merge p=%a, n=%a\n"
            debug_node p debug_node n ;
          let pe = p.edge in
          let a2 = pe.E.a2 and a1 = e.E.a1 in
          try
            let a = merge2 a2 a1 in
            p.edge <- { pe with E.a2=a ; } ;
            n.edge <- { e  with E.a1=a ; } ;
            if O.verbose > 1 then Printf.eprintf "    => p=%a, n=%a\n"
              debug_node p debug_node n
          with FailMerge ->
            Warn.fatal "Impossible annotations: %s %s"
              (E.pp_edge pe) (E.pp_edge e)
        end ;
        if n.next != m then do_rec  n.next in
    do_rec m

(* Set directions of events *)

let is_rmw_edge e = match e.E.edge with
| E.Rmw _ ->true
| _ -> false

let is_rmw d e = match d with
| R -> is_rmw_edge e.edge
| W -> is_rmw_edge e.prev.edge
| J -> is_rmw_edge e.edge



let remove_store n0 =
  let n0 =
    try find_non_insert_store n0
    with Not_found -> Warn.user_error "I cannot believe it" in
  let rec do_rec m =
    begin
      match m.edge.E.edge with
      | E.Store ->
         let prev = find_non_insert_store_prev m
         and next = find_non_insert_store m in
         prev.next <- next ;
         next.prev <- prev ;
         m.evt <- { m.evt with dir = Some W; } ;
         next.store <- m
      | E.Node W -> (* Also remove isolated W nodes, before computing values *)
         let prev = m.prev
         and next = m.next in
         prev.next <- next ;
         next.prev <- prev
      | _ -> ()
    end ;
    if m.next != n0 then do_rec m.next in
  do_rec n0 ;
  n0

 let set_dir n0 =
  let rec do_rec m =
    if non_insert_store m.edge then begin
      let my_d =  E.dir_src m.edge in
      let p = find_non_insert_store_prev m.prev in
      if E.is_node m.edge.E.edge then begin (* perform sanity checks specific to Node pseudo-edge *)
        if E.is_node p.edge.E.edge then begin
          Warn.fatal "Double 'Node' pseudo edge %s %s"
          (E.pp_edge p.edge) (E.pp_edge m.edge)
        end ;
        let n = find_non_insert_store m.next in
        if not (E.is_ext p.edge && E.is_ext n.edge) then
           Warn.fatal "Node pseudo edge %s appears in-between  %s..%s (one neighbour at least must be an external edge)"
           (E.pp_edge m.edge)  (E.pp_edge p.edge)  (E.pp_edge n.edge)
      end ;
(*    eprintf "p=%a, m=%a\n" debug_node p debug_node m  ; *)
      let prev_d = E.dir_tgt p.edge in
      let d = match prev_d,my_d with
      | Irr,Irr ->
          Warn.fatal "Ambiguous direction %s %s"
            (E.pp_edge p.edge) (E.pp_edge m.edge)
      | (Dir d,Irr)|(Irr,Dir d) -> d
(*      | Dir W,Dir R when is_rmw W m -> R  *)
      | Dir d1,Dir d2 ->
          if d1=d2 then d1
          else
            Warn.fatal "Impossible direction %s[%s] %s[%s]"
              (str_node p) (pp_extr prev_d) (str_node m) (pp_extr my_d)
      | (NoDir,_)|(_,NoDir) -> assert false in
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
      m.evt <- { m.evt with dir=Some d; atom=a; rmw=rmw}
    end else
    begin
      let p = find_non_pseudo_prev m.prev
      and n = find_non_pseudo m.next in
(*      eprintf "[%a] in [%a]..[%a]\n" debug_node m debug_node p debug_node n ; *)
      if not (E.is_ext p.edge || E.is_po_or_fenced_joker p.edge || E.is_ext n.edge || E.is_po_or_fenced_joker n.edge) then begin
        Warn.fatal "Insert pseudo edge %s appears in-between  %s..%s (at least one neighbour must be an external edge)"
          (E.pp_edge m.edge)  (E.pp_edge p.edge)  (E.pp_edge n.edge)
      end
    end ;
    if m.next != n0 then do_rec m.next in
  do_rec n0 ;
  patch_edges n0 ;
  if O.verbose > 1 then begin
    eprintf "DIRECTIONS\n" ;
    debug_cycle stderr n0
  end


(***************************)
(* Set locations of events *)
(***************************)

  let is_non_fetch_and_same e =
    is_real_edge e && same_loc e && not (E.is_fetch e)
  let is_read_same_nonfetch n m =
    n.evt.loc = m.evt.loc && n.evt.dir = Some R && not (E.is_ifetch n.edge.E.a1)

  let check_fetch n0 sd =
    let rec do_rec m =
      let p = find_real_edge_prev m.prev in
      (* ensure Instr read is followed or preceded by plain read to same location*)
      if E.is_ifetch m.edge.E.a1 && m.evt.dir = Some R && not
        (is_read_same_nonfetch m.next m ||
        try (find_node_prev (fun n -> is_read_same_nonfetch n m) m) != m with Not_found -> false)
      then
        Warn.user_error "Instruction read followed by ifetch to different location [%s] => [%s]"
          (str_node p) (str_node m);
      if
        (E.is_fetch p.edge && is_non_fetch_and_same m.edge ||
        E.is_fetch m.edge && is_non_fetch_and_same p.edge) && sd = Diff
      then begin
        Warn.user_error "Ambiguous Data/Code location es [%s] => [%s]"
          (str_node p) (str_node m)
      end ;
      if m.next != n0 then do_rec m.next in
  do_rec n0

(* Loc is changing *)
let set_diff_loc st n0 =
  let rec do_rec st p m =
    let loc,st =
      if same_loc p.edge then begin
        p.evt.loc,st
      end else next_loc m.edge st in
    m.evt <- { m.evt with loc=loc ; bank=E.atom_to_bank m.evt.atom; } ;
(*    eprintf "LOC SET: %a [p=%a]\n%!" debug_node m debug_node p; *)
    if m.store != nil then begin
      m.store.evt <-
        { m.store.evt with loc=loc ; bank=Ord; }
    end ;
    if m.next != n0 then do_rec st p.next m.next
    else begin
      if m.evt.loc = n0.evt.loc then
        Warn.fatal "Cannot get changing loc accros %s\n"
          (E.pp_edge m.edge) ;
      st
    end in
  let p = n0.prev in
  assert (not (same_loc p.edge)) ;
  do_rec st p n0

(* Loc is not changing *)
let set_same_loc st n0 =
  let n1 =
    try find_node (fun n -> E.is_com n.edge) n0
    with Not_found -> n0 in
  let loc,st = next_loc n1.edge st in
  let rec do_rec m =
    m.evt <- { m.evt with loc=loc; bank=E.atom_to_bank m.evt.atom; } ;
    if m.store != nil then begin
      m.store.evt <-
        { m.store.evt with loc=loc; bank=Ord; }
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

  let tr_value e v = E.tr_value e.atom v

  let set_write_val_ord st n =
    let st = CoSt.next_co st Ord in
    let v = CoSt.get_co st Ord in
    n.evt <- { n.evt with v = tr_value n.evt v; } ;
    (* Writing Ord resets morello tag *)
    let st = CoSt.set_co st CapaTag evt_null.ctag in
    let e,st = CoSt.set_cell st n in
    n.evt <- e ;
    st

(* do_set_write_val returns true when variable next_x has been used
   and should thus be initialised *)
  let rec do_set_write_val next_x_ok st pte_val = function
    | [] -> next_x_ok
    | n::ns ->
       let st =
         if n.store == nil then st
         else set_write_val_ord st n.store in
       begin if Code.is_data n.evt.loc then
          begin if do_memtag then
            let tag = CoSt.get_co st Tag in
            n.evt <- { n.evt with tag=tag; }
          else if do_morello then
            let ord = CoSt.get_co st Ord in
            let ctag = CoSt.get_co st CapaTag in
            let cseal = CoSt.get_co st CapaSeal in
            n.evt <- { n.evt with ord=ord; ctag=ctag; cseal=cseal; }
          end
        else begin
          let instr = CoSt.get_co st Instr in
          n.evt <- { n.evt with ins=instr}
        end
(*
          else if do_neon then (* set both fields, it cannot harm *)
            let ord = get_co st Ord in
            let v = get_co st VecReg in
            let vecreg = [|v;v;v;v;|] in
            n.evt <- { n.evt with ord=ord; vecreg=vecreg; }
*)
        end ;
        begin match n.evt.dir with
        | Some W ->
            begin match n.evt.loc with
            | Data _ ->
                let bank = n.evt.bank in
                begin match bank with
                | Ord | Instr ->
                   let st = set_write_val_ord st n in
                   do_set_write_val next_x_ok st pte_val ns
                | Pair ->
                   (* Same code as for Ord, however notice that
                      CoSet.set_cell has a case for pairs.
                      However increment of current value is by 2 *)
                   let cell = CoSt.get_cell st in
                   assert (Array.length cell=2) ;
                   let st = CoSt.next_co st Ord in (* Pre-increment *)
                   let st = set_write_val_ord st n in
                   do_set_write_val next_x_ok st pte_val ns
                | Tag|CapaTag|CapaSeal ->
                   let st = CoSt.next_co st bank in
                   let v = CoSt.get_co st bank in
                   n.evt <- { n.evt with v = v; } ;
                   let e,st = CoSt.set_tcell st n.evt in
                   n.evt <- e ;
                   do_set_write_val next_x_ok st pte_val ns
                | VecReg a ->
                   let st = CoSt.step_simd st a in
                   let cell = CoSt.get_cell st in
                   let vecreg  = E.SIMD.read a cell in
                   let v =
                     match vecreg with
                       | (v::_)::_ -> v
                       | _ -> assert false in
                   n.evt <- { n.evt with vecreg; cell;v;} ;
                   do_set_write_val next_x_ok st pte_val ns
                | Pte ->
                   let next_x_pred = ref false in
                   let pte_val =
                     if do_kvm then begin
                         let next_loc () =
                           match n.evt.loc with
                           | Code.Data x ->
                              begin try
                                  let m =
                                    find_node
                                      (fun m ->
                                        match m.evt.loc with
                                        | Code.Data y ->
                                           not (Misc.string_eq x y)
                                        | _-> false) n in
                                  Code.as_data m.evt.loc
                                with Not_found ->
                                  next_x_pred := true ; next_x end
                           | Code.Code _ -> assert false in
                         E.set_pteval n.evt.atom pte_val next_loc
                       end else pte_val in
                   n.evt <- { n.evt with pte = pte_val; } ;
                   do_set_write_val (!next_x_pred || next_x_ok) st pte_val ns
                end
            | Code _ ->
              let bank = n.evt.bank in
                begin match bank with
              | Instr ->
                  let st = CoSt.next_co st bank in
                  let v = CoSt.get_co st bank in
                  n.evt <- { n.evt with ins = v;} ;
                  do_set_write_val next_x_ok st pte_val ns
               | _ -> do_set_write_val next_x_ok st pte_val ns
            end
            end
        | Some (R|J) |None -> do_set_write_val next_x_ok st pte_val ns
        end

  let set_all_write_val nss =
    let _,initvals =
      List.fold_right
        (fun ns (k,env as r) ->
          match ns with
          | [] -> r
          | n::_ ->
              let loc = n.evt.loc in
              let sz = get_wide_list ns in
              let i = if do_kvm then k else 0 in
              let next_x_ok =
                do_set_write_val
                  false
                  (CoSt.create ~init:i sz)
                  (pte_val_init loc) ns in
              let env = if do_kvm then (Code.as_data loc,k)::env else env in
              if next_x_ok then
                k+8,(next_x,k+4)::env
              else
                k+4,env)
        nss (0,[]) in
    initvals

  let set_write_v n =
    let nss =
      try
        let m =
          find_node
            (fun m ->
              m.prev.evt.loc <> m.evt.loc &&
              m.next.evt.loc = m.evt.loc) n in
        split_by_loc m
      with Not_found -> try
        let m =
          find_node
            (fun m -> match m.prev.edge.E.edge with
            | E.Fr _|E.Rf _|E.Ws _|E.Leave _|E.Back _
            | E.Hat|E.Rmw _|E.Irf _|E.Ifr _ -> true
            | E.Po _|E.Dp _|E.Fenced _|E.Insert _|E.Store|E.Node _ -> false
            | E.Id -> assert false) n in
        split_one_loc m
      with Exit -> Warn.fatal "Cannot set write values" in
    let initvals = set_all_write_val nss in
    nss,initvals

(* Loop over every node and set the expected value from the previous node *)
let set_dep_v nss =
  let v = List.fold_left
    (fun k ns ->
      List.fold_left
        (fun v n ->
          n.evt <- { n.evt with dep=v; } ;
          n.evt.v)
        k ns)
    0 nss in
  (if List.length nss > 0 then
    if List.length (List.hd nss) > 0 then
      let n = (List.hd (List.hd nss)) in
      n.evt <- { n.evt with dep=v; }) ;
  ()

(* TODO: this is wrong for Store CR's: consider Rfi Store PosRR *)
let set_read_v n cell =
  let e = n.evt in
  let v = E.extract_value cell.(0) e.atom in
(* eprintf "SET READ: cell=0x%x, v=0x%x\n" cell v ; *)
  let e = { e with v=v; } in
  n.evt <- e
(*  eprintf "AFTER %a\n" debug_node n *)

let set_read_pair_v n cell =
  let e = n.evt in
  let v0 = E.extract_value cell.(0) e.atom
  and v1 =  E.extract_value cell.(1) e.atom in
  let v = v0 + v1 in
  let e = { e with v=v; } in
  n.evt <- e

let do_set_read_v =
  (* st keeps track of tags, cell and pte_cell are the current
     state of memory *)
  let rec do_rec st cell pte_cell = function
    | [] -> cell.(0),pte_cell
    | n::ns ->
        let cell =
          if n.store == nil then cell
          else n.store.evt.cell in
        let bank = n.evt.bank in
        begin match n.evt.dir with
        | Some R ->
            begin match bank with
            | Ord ->
               set_read_v n cell
            | Pair ->
               set_read_pair_v n cell
            | VecReg a ->
               let v = E.SIMD.read a cell in
               n.evt <- { n.evt with vecreg=v; }
            | Tag|CapaTag|CapaSeal ->
                n.evt <- { n.evt with v = CoSt.get_co st bank; }
            | Pte ->
                n.evt <- { n.evt with pte = pte_cell; }
            | Instr ->
                n.evt <- { n.evt with ins = CoSt.get_co st bank; }
            end ;
            do_rec st cell pte_cell ns
        | Some W ->
            let st =
              match bank with
              | Tag|CapaTag|CapaSeal ->
                 CoSt.set_co st bank n.evt.v
              |Instr -> CoSt.set_co st bank n.evt.ins
              | Pte|Ord|Pair|VecReg _ ->
                 st in
            do_rec st
              (match bank with
               | Ord|Pair|VecReg _ ->
                  if Code.is_data n.evt.loc then n.evt.cell
                  else cell
               | Tag|CapaTag|CapaSeal|Pte|Instr -> cell)
              (match bank with
               | Ord|Pair|Tag|CapaTag|CapaSeal|VecReg _|Instr -> pte_cell
               | Pte -> n.evt.pte)
              ns
        | None | Some J ->
            do_rec st cell pte_cell ns
        end in
  fun ns -> match ns with
  | []   -> assert false
  | n::_ ->
     let sz = get_wide_list ns in
     let st = CoSt.create sz in
     let cell = CoSt.get_cell st in
     do_rec st cell
        (pte_val_init n.evt.loc)
        ns

let set_read_v nss =
  List.fold_right
    (fun ns k -> match ns with
    | [] -> k
    | n::_  ->
        let vf = do_set_read_v ns in
        (n.evt.loc,vf)::k)
    nss []

(* zyva... *)

let finish n =
  let st = (0,0),Env.empty in
(* Set locations *)
  let sd,n =
    let no =
      try Some (find_edge_prev diff_loc (find_edge_prev diff_proc n))
      with Not_found -> None in
    match no with
    | Some n ->
        Diff,
        begin try find_edge same_loc n
        with Not_found -> Warn.fatal "This cycle changes location at every step" end
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
  let by_loc,initvals = set_write_v n in
  if O.verbose > 1 then begin
    eprintf "INITIAL VALUES: %s\n"
      (String.concat "; "
         (List.map
            (fun (loc,k) -> sprintf "%s->%d" loc k)
            initvals)) ;
    eprintf "WRITE VALUES\n" ;
    debug_cycle stderr n
  end ;
(* Set load values *)
  let vs = set_read_v by_loc in
(* Set dependency values *)
  (if do_morello then set_dep_v by_loc) ;
  if O.verbose > 1 then begin
    eprintf "READ VALUES\n" ;
    debug_cycle stderr n ;
    eprintf "FINAL VALUES [%s]\n"
      (String.concat ","
         (List.map
            (fun (loc,(v,_pte)) -> sprintf "%s -> 0x%x"
                (Code.pp_loc loc) v) vs))
  end ;
  if O.variant Variant_gen.Self then check_fetch n sd;
  initvals


(* Re-extract edges, with irelevant directions solved *)
let extract_edges n =
  let rec do_rec m =
    let k =
      if m.next == n then []
      else do_rec m.next in
    let k = m.edge::k in
    let k =
      if m.store == nil then k
      else E.plain_edge m.store.edge.E.edge::k in
    k in
  do_rec n

let resolve_edges = function
  | [] -> Warn.fatal "No edges at all!"
  | es ->
      let c = build_cycle es in
      merge_annotations c ;
      let c = remove_store c in
      set_dir c ;
      extract_edges c,c

let make es =
  let es,c = resolve_edges es in
  let initvals = finish c in
  es,c,initvals

(*************************)
(* Gather events by proc *)
(*************************)

let find_start_proc n =
  let p = find_non_pseudo_prev n.prev in
  if
    diff_proc p.edge
  then p.next
  else
    let n = find_edge (fun n -> diff_proc n) n in
    try find_edge same_proc n
    with Not_found -> n


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

  let value_before v1 v2 = v1 < v2


  let proc_back ns = match ns with
  | []|[_] -> false
  | fst::rem ->
      let lst = Misc.last rem in
      let e1 = fst.evt and e2 = lst.evt in
      e1.loc = e2.loc && value_before e2 e1

  let debug_proc ns =
    String.concat " " (List.map (fun n -> sprintf "<%s>" (str_node n)) ns)

  let debug_procs nss =  List.iter (fun ns -> eprintf "%s\n" (debug_proc ns)) nss

  let split_procs n =
    let n =
      try find_start_proc n
      with Not_found -> Warn.fatal "Cannot split in procs" in
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
            (fun n ->
              if n.store != nil then begin
                n.store.evt <-  { n.store.evt with proc = k; }
              end ;
              n.evt <- { n.evt with proc = k; })
            ns ;
          num_rec (k+1) nss in
    num_rec 0 nss ;
    if
      not O.allow_back &&
      List.exists proc_back nss
    then Warn.fatal "Forbidden po vs. com" ;
    if O.verbose > 1 then begin
      eprintf "SPLITTED:\n" ; debug_procs nss
    end ;
    nss

(****************************)
(* Compute coherence orders *)
(****************************)

let rec group_rec x ns = function
  | [] -> [x,List.rev ns]
  | (y,n)::rem ->
      if Code.loc_compare x y = 0 then group_rec x (n::ns) rem
      else (x,List.rev ns)::group_rec  y [n] rem

  let group = function
    | [] -> []
    | (x,n)::rem -> group_rec x [n] rem

  let by_loc xvs =
    let r = group xvs in
    let r =  List.stable_sort (fun (x,_) (y,_) -> Code.loc_compare x y) r in
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


  let do_get_writes pbank n =
    let rec do_rec m =
      let k =
        if m.next == n then []
        else do_rec m.next in
      let e = m.evt in
      let k =  match e.dir with
      | Some W ->
          if
            E.is_node m.edge.E.edge || not (pbank m.evt.bank)
          then k else (e.loc,m)::k
      | None| Some R | Some J -> k in
      if m.store == nil then k
      else begin
        let e = m.store.evt in
        if pbank e.bank then
          (e.loc,m.store)::k
        else k
      end in
    do_rec n

  let get_ord_writes =
    let open Code in
    do_get_writes (* Not so sure about capacity here... *)
      (function Ord|Tag|VecReg _|Pair|Instr -> true | CapaTag|CapaSeal|Pte -> false)

  let get_pte_writes =
    do_get_writes (function Code.Pte -> true | _ -> false)

  let to_tagloc = function
    | Data s -> Data (Misc.add_atag s)
    | Code s -> Code (Misc.add_atag s)

  let get_tag_locs (loc,n) =
    (to_tagloc loc,n)

  let get_observers n =
    let e = n.evt in
    assert (e.dir = Some W) ;
    let k = IntSet.empty in
    let k = if e.proc >= 0 then IntSet.add e.proc k else k in
    let k = match n.edge.E.edge with
    | E.Rf _ -> IntSet.add n.next.evt.proc k
    | _ -> k in
    k

  let coherence n =
    let r = match find_change n with
    | Some n ->
        let ord_ws = get_ord_writes n in
        (* MTE locations shadow normal locations, so we need
         * to track them separately. As we may be interested
         * in the same graph nodes, lets just duplicate and
         * label accordingly. *)
        let tag_ws = if do_memtag then
          List.map get_tag_locs (get_ord_writes n) else [] in
        let ws = ord_ws@tag_ws in
        if O.verbose > 1 then
          List.iter
            (fun (loc,n) ->
              eprintf "LOC=%s, node=%a\n" (Code.pp_loc loc) debug_node n)
            ws ;
        let r = by_loc ws in
        List.fold_right
          (fun (loc,ws) k -> match ws with
          | [] -> k
          | [ns] ->
             if O.verbose > 1 then
               Printf.eprintf "Standard write sequence on %s: %s\n"
                 (Code.pp_loc loc)
                 (String.concat " "
                    (List.map str_node ns)) ;
             (loc,ws)::k
          | _ ->
              List.iter
                (fun ns -> eprintf "[%a]\n" debug_nodes ns)
                ws ;
              assert false)
          r []
    | None ->
        if O.same_loc then
          match get_ord_writes n with
          | [] -> []
          | (loc,_)::_ as xs ->
              [loc,[List.map snd xs]]
        else
          Warn.fatal "Unique location" in
    List.fold_right
      (fun (loc,ns) k ->
        match loc with
        | Data loc ->
            (loc,
            List.map
              (List.map (fun n -> n,get_observers n))
              ns)::k
        | Code _ ->  k)
      r []

  let last_ptes n =
    match find_change n with
    | Some n ->
        let ws = get_pte_writes n in
        let r = by_loc ws in
        List.fold_right
          (fun (loc,ns) k -> match List.flatten ns with
          | []|[_]|_::_::_::_ -> k
          | [_;n;] ->
              let p = n.evt.pte in
              (Misc.add_pte (Code.as_data loc),p)::k)
          r []
    | None ->  []

(* Get all shared locations/labels *)

  let get_rec get m =
    let rec do_rec k n =
      if n.next == m then k
      else
        let k = get n.evt.loc k in
        do_rec k n.next in
    let locs = do_rec [] m in
    StringSet.elements (StringSet.of_list locs)


  let get_globals ?(init=[]) m =
    let init = List.map (fun (loc,_) -> loc) init in
    let code =
      get_rec
        (fun loc k -> match loc with Data loc -> loc::k | Code _ -> k)
        m in
    StringSet.elements (StringSet.of_list (init@code))

  let get_labels m =
    get_rec
      (fun loc k -> match loc with Code loc -> loc::k | Data _ -> k)
      m

end
