(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
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

  type atom
  module Value : Value_gen.S with type atom = atom
  module SIMD : Atom.SIMD
  module RMW : Atom.RMW with type atom = atom

  (* TODO can be parametric by dir *)
  type event =
      { loc : loc ; ord : int;
        (* TODO morello related value, fold into value *)
        tag : int ;
        (* morello *)
        ctag : int; cseal : int; dep : int ;
        v   : Value.v ; (* Value read or written *)
        (* TODO fold into value *)
        vecreg: Value.v list list ; (* Alternative for SIMD *)
        dir : dir option ;
        proc : Code.proc ;
        atom : atom option ;
        (* TODO why not put into dir ? *)
        rmw : bool ;
        (* TODO, cell and tcell fold together *)
        cell : Value.v array ; (* Content of memory, after event *)
        tcell : Value.v array ; (* value of tag memory after event *)
        bank : SIMD.atom Code.bank ;
        idx : int ;
        (* If need to check this operation can fault.
           Label the instruction with `Label.t`. *)
        check_fault : (Label.t * bool) option;
        (* If the effect of this event should be check in the postcondition.
           E.g., if the value changes since last time,
           therefore, any read event should lead to a postcondition
           to check the observed value *)
        check_value : bool option }

  val evt_null : event
  val make_wsi : int -> Code.loc -> event

  (* TODO change v event *)
  val debug_evt : event -> string

  (* TODO change v event *)
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
  val finish : node -> node * Value.env

(* Composition of the two more basic steps above *)
  val make : edge list -> edge list * node * Value.env

(* split cycle amoungst processors *)
  val split_procs : node -> node list list

(* Return coherence orders *)
  val coherence : node -> (string * (node * IntSet.t) list list) list

(* Return last pteval in pte accesses coherence *)
  val last_ptes : node -> (string * Value.pte) list

(* All locations *)
  val get_globals : ?init:Value.env -> node -> string list

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
       and module Value = E.Value
       and module RMW = E.RMW
  = struct
  let dbg = false
  let do_memtag = O.variant Variant_gen.MemTag
  let do_morello = O.variant Variant_gen.Morello
  let do_kvm = Variant_gen.is_kvm O.variant
  let do_neon = O.variant Variant_gen.Neon
  let do_sve = O.variant Variant_gen.SVE
  let do_sme = O.variant Variant_gen.SME
  let do_no_fault = O.variant Variant_gen.NoFault

  type fence = E.fence
  type edge = E.edge
  module SIMD = E.SIMD
  type atom = E.atom
  module Value = E.Value
  module RMW = E.RMW

  type event =
      { loc : loc ; ord : int; tag : int;
        ctag : int; cseal : int; dep : int;
        v   : Value.v ;
        vecreg: Value.v list list ;
        dir : dir option ;
        proc : Code.proc ;
        atom : atom option ;
        rmw : bool ;
        cell : Value.v array ; (* value of cell at node exit *)
        tcell : Value.v array ; (* value of tag cell at node exit *)
        bank : SIMD.atom Code.bank ;
        idx : int ;
        check_fault : (Label.t * bool) option;
        check_value : bool option }

  let pte_default = Value.default_pte "*"

  let is_pte_default loc pte =
    let rhs = match loc with
    | Data loc -> Value.default_pte loc
    | Code _ -> pte_default in
    Value.pte_compare pte rhs = 0

  let evt_null =
    { loc=Code.loc_none ; ord=0; tag=0;
      ctag=0; cseal=0; dep=0;
      vecreg= [];
      v=Value.no_value; dir=None; proc=(-1); atom=None; rmw=false;
      cell=[||]; tcell=[||];
      bank=Code.Ord; idx=(-1);
      check_fault=None;
      check_value=None; }

  let make_wsi idx loc = { evt_null with dir=Some W ; loc=loc; idx=idx; v=Value.from_int 0;}

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
         Some W -> "W" | Some R -> "R" | None -> "_"

  let debug_atom a =
    match a with None -> "" | Some a -> E.pp_atom a

  let debug_tag =
    if do_memtag then fun e -> sprintf " (tag=%i)" e.tag
    else fun _ -> ""

  let debug_morello =
    if do_morello then fun e ->
      sprintf " (ord=%i) (ctag=%i) (cseal=%i) (dep=%i)" e.ord e.ctag e.cseal e.dep
    else fun _ -> ""

  let debug_vector =
    if do_neon || do_sve || do_sme then
      let pp_one value = Code.add_vector O.hexa
        (List.map Value.to_int value) in
      fun e ->
      sprintf " (vecreg={%s})"
        (String.concat "," (List.map pp_one e.vecreg))
    else fun _ -> ""

  let debug_val = Value.pp_v ~hexa:O.hexa

  let debug_vec v =
    String.concat ", " @@ List.map debug_val @@ Array.to_list v

  let debug_evt e =
    sprintf "#[%d] %s%s %s %s %s%s%s%s%s fault_check:%s value_check:%s"
      e.idx
      (debug_dir e.dir)
      (debug_atom e.atom)
      (Code.pp_loc e.loc)
      ( if e.rmw then "rmw" else "" )
      ( match debug_vec e.cell with | "" -> "" | s -> "cell=[" ^ s ^"] ")
      (debug_val e.v) (debug_tag e) (debug_morello e) (debug_vector e)
      ( match e.check_fault with | Some (_,b) -> sprintf "%b" b | None -> "none" )
      ( match e.check_value with | Some b -> sprintf "%b" b | None -> "none" )

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
      fprintf chan "%a\n" debug_node m ;
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

let check_balance es =
  let count = es
    |> List.map (fun e -> match e.E.edge with E.Back _ -> -1 | E.Leave _ -> 1 | _ -> 0)
    |> List.fold_left ( + ) 0 in
  count = 0

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

let same_loc e = Code.is_same_loc @@ E.loc_sd e

let diff_loc e = Code.is_diff_loc @@ E.loc_sd e

let same_proc e = E.get_ie e = Int
let diff_proc e = E.get_ie e = Ext
let int_com e = match e.E.edge with
  | E.Rf Int|E.Fr Int|E.Ws Int -> true
  | _ -> false


(* Coherence definition *)

module CoSt = struct

  module M =
    MyMap.Make
      (struct type t = E.SIMD.atom Code.bank let compare = compare end)

  type t = { map : int M.t;
             co_cell : Value.v array;
             pte_value : Value.pte;
             (* - Irr, checks both
                - Dir R, only checks read
                - Dir W, only checks write
                - NoDir, no need to check *)
             check_fault : Code.extr;
             check_value : bool;
             machine_feature: StringSet.t }

  let create init_value sz pte_value check_value check_fault machine_feature =
    let map = List.fold_left ( fun acc bank -> M.add bank init_value acc ) M.empty
                  [Tag; CapaTag; CapaSeal; Ord; ]
    and co_cell = Array.make (if sz <= 0 then 1 else sz) (Value.from_int init_value) in
    { map; co_cell; pte_value; check_fault; check_value ; machine_feature }

  let find_no_fail key map =
    try M.find key map with Not_found -> assert false

  let get_co st bank = Value.from_int (find_no_fail bank st.map)

  let set_co st bank v =
    let b = match bank with VecReg _ -> Ord | _ -> bank in
    { st with map=M.add b v st.map; }

  let get_cell st = st.co_cell
  let set_cell st co_cell = {st with co_cell; }

  (* Assume node `n` is a memory store event,
     assign a written value to `n`*)
  let update_cell_on_write st n =
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
            let width = Value.from_int ((Value.to_int e.v) - 1) in
            co_cell.(0) <- E.overwrite_value old e.atom width;
            let old = st.co_cell.(0) in
            co_cell.(1) <- E.overwrite_value old e.atom e.v
         | _ -> assert false
       end ;
       {e with cell=co_cell;},{ st with co_cell; }
    end
    | _ -> e,st


 (* Record a new `pte_value`, and set `check_fault` *)
  let set_pte_value st check_fault pte_value = { st with pte_value; check_fault }

  let get_pte_value st = st.pte_value

  let get_check_value st = st.check_value

  let set_check_fault st = {st with check_fault = Irr }

  (* Check if `pte_val` might fault *)
  let label_pte_fault dir pte_val =
    Some ( (Label.next_label "L"), (Value.can_fault dir pte_val) )

  (* Helper function returns a fresh label and a boolean for if it should fault,
     if a fault check is need. Otherwise return `None`. *)
  let fault_update st dir =
    let unset_check_fault st = {st with check_fault = NoDir } in
    let pte_val = get_pte_value st in
    match () with
    | _ when (st.check_fault = NoDir || do_no_fault) -> None,unset_check_fault st
      (* Need to check fault *)
    | _ when do_kvm ->
      let fault,check_fault = match dir,st.check_fault with
      | _,NoDir -> None,NoDir
      | (R|W),Irr | W,Dir W | R,Dir R -> label_pte_fault dir pte_val,NoDir
      | W,Dir R -> None,Dir R
      | R,Dir W -> None,Dir W in
      fault,{st with check_fault}
      (* In variants `memtag` and `morello`, the cycles are constructed such that
         no fault occurs *)
    | _ when do_memtag || do_morello ->
      Some ((Label.next_label "L"), false),unset_check_fault st
    |_ -> None,unset_check_fault st

  let implicit_pte_update st dir =
    match Value.implicitly_set_pteval dir st.machine_feature st.pte_value with
    | Some (check_fault,pte_value) -> set_pte_value st check_fault pte_value
    | None -> st

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
    let new_co_cell = st.co_cell |> Array.map Value.to_int
                      |> E.SIMD.step n fst
                      |> Array.map Value.from_int in
    { st with co_cell=new_co_cell; map=M.add Ord lst st.map; }
end

(* Decide the initial pte value. *)
let pte_val_init ns loc =
  match loc with
    | Code.Data loc when do_kvm ->
      let atom_list = List.filter_map
        ( fun node -> node.evt.atom ) ns in
      Value.init_pte loc atom_list
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
  | (None,Some a)
  | (Some a,None) when E.is_ifetch (Some a) -> raise FailMerge
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
(*    eprintf "p=%a, m=%a\n" debug_node p debug_node m; *)
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
      if not (E.is_ext p.edge || E.is_ext n.edge) then begin
        Warn.fatal "Insert pseudo edge %s appears in-between  %s..%s (at least one neighbour must be an external edge)"
          (E.pp_edge m.edge)  (E.pp_edge p.edge)  (E.pp_edge n.edge)
      end;
      match p.edge.E.edge with
      | (E.Rf Ext | E.Fr Ext) ->
        Warn.fatal "Insert pseudo edge %s appears after external communication edge %s"
        (E.pp_edge m.edge) (E.pp_edge p.edge)
      | _ -> ()
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

let is_read_same_fetch m =
    let check n = (n != m && (loc_compare n.evt.loc  m.evt.loc) = 0 && n.evt.dir = Some R &&
                  (E.is_ifetch n.edge.E.a1)) in
    try ignore (find_node_prev (fun n -> check n) m); true
    with Not_found -> false

  let check_fetch n0 =
    let rec do_rec m =
      (* ensure Instr read is followed or preceded by plain read to same location*)
      begin match m.evt.loc, m.evt.dir with
        | Code.Code _, Some R when (E.is_ifetch m.edge.E.a1) ->
            if is_read_same_fetch m then begin
              Warn.user_error "Multiple ifetch reads to same code location [%s]" (str_node m)
              end;
        | Code.Code _, Some R when not (E.is_ifetch m.edge.E.a1) ->
            if not (is_read_same_fetch m) then begin
             Warn.user_error "Reading from label that doesn't exist [%s]" (str_node m)
            end;
        | Code.Code _, Some W when (E.is_ifetch m.edge.E.a1) ->
          Warn.user_error "Writing non-instruction value to code location: [%s]" (str_node m)
        | _ -> ();
        end;
        if m.next != n0 then do_rec m.next in
  do_rec n0

(* Loc is changing *)
let set_diff_loc st n0 =
  let rec do_rec st p m =
    let loc,st =
      if same_loc p.edge then begin
        p.evt.loc,st
      end
    else
      let n1 = try
        find_node
          (fun n -> (if not (same_loc n.prev.edge) then raise Not_found); E.is_ifetch n.edge.E.a1 ) m.next
        with Not_found -> try
          find_node_prev
            (fun n -> (if not (same_loc n.edge) then raise Not_found); E.is_ifetch n.edge.E.a2 ) m.prev
        with Not_found ->  m in
      next_loc n1.edge st in
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

  (* Split the cycle into segments, each contains
     events for the same location *)
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

  let exist_plain_value_write ns =
      List.exists
      ( fun n -> match n.evt.bank,n.evt.dir with
      |(Ord|Tag|CapaTag|CapaSeal|VecReg _|Pair|Instr),Some W -> true
      |_ -> false ) ns

  let exist_fault_related_write ns =
    List.fold_left ( fun acc n ->
      let next = match n.evt.bank,n.evt.dir with
        | Tag,Some W -> Irr
        | Pte,Some W -> Value.need_check_fault n.evt.atom
        | _ -> NoDir in
      match acc,next with
      | _,Irr | Irr, _
      | Dir W,Dir R | Dir R,Dir W -> Irr
      | NoDir,n | n,NoDir -> n
      | Dir W,Dir W -> Dir W
      | Dir R,Dir R -> Dir R
    ) NoDir ns

  let tr_value e v = E.tr_value e.atom v

  let set_write_val_ord st n =
    let st = CoSt.next_co st Ord in
    let v = CoSt.get_co st Ord in
    if v = n.evt.v then
      Warn.fatal "Updated value remains the same. An issue should be reported.";
    let st = CoSt.implicit_pte_update st W in
    n.evt <- { n.evt with v = tr_value n.evt v; } ;
    (* Writing Ord resets morello tag *)
    let st = CoSt.set_co st CapaTag evt_null.ctag in
    let e,st = CoSt.update_cell_on_write st n in
    n.evt <- e ;
    st

  (* `do_set_write_val` returns true when variable next_x has been used
     and should thus be initialised *)
  let do_set_write_val next_x_ok st nss =
    List.fold_left ( fun (next_x_ok, st) n ->
    (* Update the `cell` in `st` if there is a `.store *)
      let st = if n.store == nil then st else set_write_val_ord st n.store in
      (* Update tag and instruction value in `st` no matter `W`, `R` etc. *)
      (* TODO: potentially rework the if-elseif-else here as it is confused *)
      begin if Code.is_data n.evt.loc then
        begin if do_memtag then
          let tag = Value.to_int (CoSt.get_co st Tag) in
          n.evt <- { n.evt with tag; }
        else if do_morello then
          let ord = Value.to_int (CoSt.get_co st Ord) in
          let ctag = Value.to_int (CoSt.get_co st CapaTag) in
          let cseal = Value.to_int (CoSt.get_co st CapaSeal) in
          n.evt <- { n.evt with ord; ctag; cseal; }
        end
 (*
          else if do_neon then (* set both fields, it cannot harm *)
            let ord = get_co st Ord in
            let v = get_co st VecReg in
            let vecreg = [|v;v;v;v;|] in
            n.evt <- { n.evt with ord=ord; vecreg=vecreg; }
 *)
      end ;
      (* END of `if Code.is_data n.evt.loc` *)
      match n.evt.dir with
      | Some W ->
          begin
          let check_value = Some (CoSt.get_check_value st) in
          (* No need to add fault check in read modify write situation,
             as the label will be assigned in read *)
          let fault_update_without_rmw st =
            if n.evt.rmw then None,st else CoSt.fault_update st W in
          match n.evt.loc with
          | Data _ ->
            let bank = n.evt.bank in
            begin match bank with
            | Instr -> Warn.fatal "instruction annotation to data bank not possible?"
            | Ord ->
              let st = set_write_val_ord st n in
              let check_fault, st =
                if do_morello then None, st
                else fault_update_without_rmw st in
              n.evt <- { n.evt with check_fault; check_value; };
              (next_x_ok, st)
            | Pair ->
              (* Same code as for Ord, however notice that
                 CoSet.set_cell has a case for pairs.
                 However increment of current value is by 2 *)
              let cell = CoSt.get_cell st in
              assert (Array.length cell>=2) ;
              let st = CoSt.next_co st Ord in (* Pre-increment *)
              let st = set_write_val_ord st n in
              let check_fault, st = fault_update_without_rmw st in
              n.evt <- { n.evt with check_fault; check_value; };
              (next_x_ok, st)
            | Tag ->
              let st = CoSt.next_co st bank |> CoSt.set_check_fault in
              let v = CoSt.get_co st bank in
              n.evt <- { n.evt with v = v; check_value; } ;
              let e,st = CoSt.set_tcell st n.evt in
              n.evt <- e ;
              (next_x_ok, st)
            | CapaTag|CapaSeal ->
              (* in Morello, check fault on CapaTag or CapaSeal access
                 if it is followed by a depend address edge *)
              let check_fault =
                if E.is_dp_addr n.prev.edge.E.edge then
                  Some (Label.next_label "L", false)
                else None in
              let st = CoSt.next_co st bank in
              let v = CoSt.get_co st bank in
              n.evt <- { n.evt with v = v; check_value; check_fault} ;
              let e,st = CoSt.set_tcell st n.evt in
              n.evt <- e ;
              (next_x_ok, st)
            | VecReg a ->
              let st = CoSt.step_simd st a in
              let cell = CoSt.get_cell st
                           |> Array.map Value.to_int in
              let vecreg  = E.SIMD.read a cell
                       |> List.map (List.map Value.from_int) in
              let cell = Array.map Value.from_int cell in
              let v =
                match vecreg with
                  | (v::_)::_ -> v
                  | _ -> assert false in
              n.evt <- { n.evt with vecreg; cell; v; check_value; } ;
              (next_x_ok, st)
            | Pte ->
            (* TODO Rework here, esp the function `next_loc` and ref value `next_x_pred`.
              They are all difficult to understand. *)
              let next_x_pred = ref false in
              (* get the previous `pte_value` *)
              let pte_val = CoSt.get_pte_value st in
              (* update the pte value in kvm variant *)
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
                      | Code.Code _ -> Warn.fatal "Code location has no pte value." in
                    E.set_pteval n.evt.atom pte_val next_loc
                  end else pte_val in
              let check_fault = Value.need_check_fault n.evt.atom in
              let st = CoSt.set_pte_value st check_fault pte_val in
              let v = Value.from_pte pte_val in
              n.evt <- { n.evt with v; check_value } ;
              ((!next_x_pred || next_x_ok), st)
            end (* END of match bank *)
          | Code _ ->
            n.evt <- { n.evt with check_value; } ;
            let bank = n.evt.bank in
            match bank with
            | Instr -> Warn.fatal "not letting instr write happen"
            | _ -> (next_x_ok, st)
          end (* END of `Some W` *)
      | Some R |None -> (next_x_ok, st)
    ) (* END of the function applying to `fold_left` *) (next_x_ok, st) nss
    (* END of do_set_write_val *)

  let set_all_write_val nss =
    (* `initptes` contains the initial pte values, if they are non-default *)
    let _,initvals =
      List.fold_left
        (fun (k,env as r) ns ->
          match ns with
          | [] -> r
          | n::_ ->
              (* Assume all node in `ns` is the same location as `n`,
                 process the nodes in list `ns` for the location `loc` *)
              let loc = n.evt.loc in
              let sz = get_wide_list ns in
              let init_val = if do_kvm then k else 0 in
              let pte_val = pte_val_init ns loc in
              (* Since it is a cycle, the initial value of `check_value`
                 and `check_fault` depend on if there are write to
                 the variable and pte respectively. *)
              let check_value = exist_plain_value_write ns in
              let check_fault = exist_fault_related_write ns in
              let machine_feature =
                List.fold_left
                  ( fun acc n ->
                    StringSet.union acc (E.get_machine_feature n.edge)
                  ) StringSet.empty ns in
              let init_st = CoSt.create init_val sz pte_val check_value check_fault machine_feature in
              let next_x_ok,_st = do_set_write_val false init_st ns in
              let env = if init_val = 0 then env
                        else (Code.as_data loc,Value.from_int init_val)::env in
              (* Add pte initial values when kvm and the value is not default *)
              let env = if (not do_kvm) || is_pte_default loc pte_val then env
                        else ((Misc.add_pte @@ Code.as_data loc),Value.from_pte pte_val)::env in
              if next_x_ok then
                k+8,(next_x,Value.from_int (k+4))::env
              else
                k+4,env )
        (* When in kvm mode, using initial value `1, 5, 9, ...,`
           avoiding value `0`, which collides with
           the default value of register. *)
        (1,[]) nss in
    initvals

  (* TODO carry back the pte init value *)
  let set_write_v n =
    let start_node,nss =
      try
        let start_node =
          find_node
            (fun m ->
              m.prev.evt.loc <> m.evt.loc &&
              m.next.evt.loc = m.evt.loc) n in
        start_node,split_by_loc start_node
      with
      | Not_found ->
        fold (fun n0 _ -> if E.is_id n0.edge.E.edge then assert false) n ();
        let is_com_rmw n0 = E.is_com n0.edge || is_rmw_edge n0.edge in
        let to_com_rmw n0 = not (is_com_rmw n0.prev) && is_com_rmw n0 in
        let to_com n0 = not (E.is_com n0.prev.edge) && E.is_com n0.edge in
        (* In the case of one location, the order on searching start node is:
          - a read node, NOT preceded by a com/rmw node, is itself a com/rmw node
          - a node, preceded by a non com/rmw node, is itself a com/rmw node
          - a read node, NOT preceded by a com node, is itself a com node
          - a node, preceded by a com node, is itself a com node.
          This order ensures some consistency on the result litmus tests,
          especially when a cycle contain rmw edge.
          However in the situation with two rmw edges, the input cycle order
          still affects the result litmus, e.g. "Amo.StEor Rfe Amo.StAdd Rfe". *)
        let start_node = try find_node (fun m -> to_com_rmw m && m.evt.dir = Some R) n
        with Not_found -> try find_node (fun m -> to_com_rmw m) n
        with Not_found -> try find_node (fun m -> to_com m && m.evt.dir = Some R) n
        with Not_found -> try find_node (fun m -> to_com m) n
        with Not_found -> Warn.fatal "cannot set write values" in
        start_node,split_one_loc start_node
      | Exit -> Warn.fatal "cannot set write values" in
    let initvals = set_all_write_val nss in
    start_node,nss,initvals

(* Loop over every node and set the expected value from the previous node *)
let set_dep_v nss =
  let v = List.fold_left
    (fun k ns ->
      List.fold_left
        (fun v n ->
          n.evt <- { n.evt with dep=Value.to_int v; } ;
          n.evt.v)
        k ns)
    (Value.from_int 0) nss in
  (if List.length nss > 0 then
    if List.length (List.hd nss) > 0 then
      let n = (List.hd (List.hd nss)) in
      n.evt <- { n.evt with dep=Value.to_int v; }) ;
  ()

(* TODO: this is wrong for Store CR's: consider Rfi Store PosRR *)
let set_read_individual_v n cell check_value =
  let e = n.evt in
  let v = E.extract_value cell.(0) e.atom in
(* eprintf "SET READ: cell=0x%x, v=0x%x\n" cell v ; *)
  let e = { e with v=v; check_value } in
  n.evt <- e
(* eprintf "AFTER %a\n" debug_node n *)

let set_read_pair_v n cell check_value =
  let e = n.evt in
  let v0 = E.extract_value cell.(0) e.atom |> Value.to_int
  and v1 =  E.extract_value cell.(1) e.atom |> Value.to_int in
  let v = v0 + v1 |> Value.from_int in
  let e = { e with v=v; check_value } in
  n.evt <- e

(* Assume all the events are for the same location,
   convert the node list, i.e., the first unnamed parameter,
   to the final value `cell` and PTE value `pte_cell` *)
let do_set_read_v init =
  let do_rec st ns =
    (* `st` keeps track of tags and current state of memory,
       - plain value => CoSt.get_cell, CoSt.set_cell,
       - pte value => CoSt.get_pte_value, CoSt.set_pte_value *)
    List.fold_left ( fun st n ->
      let st = if n.store == nil then st else CoSt.set_cell st n.store.evt.cell in
      let cell = CoSt.get_cell st in
      let bank = n.evt.bank in
      begin match n.evt.dir with
      (* Assign the read value according to `cell` and `pte_cell` *)
      | Some R ->
        (* If the result of this read need to be checked,
           i.e. generating postcondition *)
        let check_value = Some (CoSt.get_check_value st) in
        begin match bank with
        | Ord | Instr->
          let st = CoSt.implicit_pte_update st R in
          set_read_individual_v n cell check_value;
          let check_fault, st =
            if do_morello then None, st
            (* because `rmw` is treated as both read and write,
               we should assign label to this read event.
               Here we assume write is stronger than read. *)
            else if n.evt.rmw then CoSt.fault_update st W
            else CoSt.fault_update st R in
          n.evt <- { n.evt with check_fault };
          st
        | Pair ->
          let st = CoSt.implicit_pte_update st R in
          set_read_pair_v n cell check_value;
          let check_fault, st = CoSt.fault_update st R in
          n.evt <- { n.evt with check_fault };
          st
        | VecReg a ->
          let st = CoSt.implicit_pte_update st R in
          let cell = Array.map Value.to_int cell in
          let v = E.SIMD.read a cell
                   |> E.SIMD.reduce
                   |> Value.from_int in
          let check_fault, st = CoSt.fault_update st R in
          n.evt <- { n.evt with v=v ; vecreg=[]; bank=Ord; check_value; check_fault ; };
          st
        | Tag ->
          n.evt <- { n.evt with v = CoSt.get_co st bank; check_value; };
          st
        | CapaTag|CapaSeal ->
          (* in Morello, check fault on CapaTag or CapaSeal access
             if it is followed by a depend address edge *)
          let check_fault =
            if E.is_dp_addr n.prev.edge.E.edge then
              Some (Label.next_label "L", false)
            else None in
          n.evt <- { n.evt with v = CoSt.get_co st bank; check_value; check_fault };
          st
        | Pte ->
          let pte_val = CoSt.get_pte_value st in
          let v = Value.from_pte pte_val in
          n.evt <- { n.evt with v; };
          st
        end
      (* Update `st`, `cell` and `pte_cell` for future read events *)
      | Some W ->
        let st =
          match bank with
          | Tag ->
            CoSt.set_co st bank (Value.to_int n.evt.v) |> CoSt.set_check_fault
          |CapaTag|CapaSeal ->
            CoSt.set_co st bank (Value.to_int n.evt.v)
          |Ord|Pair|VecReg _ ->
              (* Record the cell value in `st` in
               memory access to a non-instruction value *)
            let st = CoSt.implicit_pte_update st W in
            if Code.is_data n.evt.loc then CoSt.set_cell st n.evt.cell
            else st
          | Instr -> st
          |Pte ->
            (* Record the pte value in `st` in
              memory access to a non-instruction pte value *)
            if Code.is_data n.evt.loc then
              let check_fault = Value.need_check_fault n.evt.atom in
              CoSt.set_pte_value st check_fault @@ Value.to_pte n.evt.v
            else st in
        st
      | None ->
        st
    end ) st ns in
  fun ns -> match ns with
  | []   -> assert false
  | n::_ ->
    let sz = get_wide_list ns in
    let pte_val = pte_val_init ns n.evt.loc in
    let check_value = exist_plain_value_write ns in
    let check_fault = exist_fault_related_write ns in
    let machine_feature =
      List.fold_left
        ( fun acc n ->
          StringSet.union acc (E.get_machine_feature n.edge)
        ) StringSet.empty ns in
    let init_st = CoSt.create init sz pte_val check_value check_fault machine_feature in
    let final_st = do_rec init_st ns in
    (CoSt.get_cell final_st).(0),CoSt.get_pte_value final_st

  let set_read_v nss initvals =
  List.filter_map
    (fun ns -> match ns with
      | [] -> None
      | n::_  ->
        let init = if not (Code.is_data n.evt.loc) then 0
                  else List.assoc_opt (Code.as_data n.evt.loc) initvals
                  |> Option.map Value.to_int
                  |> Option.value ~default:0 in
        let vf = do_set_read_v init ns in
        Some (n.evt.loc,vf))
  nss

  (* find the next node with communication but
     there are all ordinary write nodes in between. *)
  let rec find_fault_com = function
    | [] -> None
    | hd::tail ->
      if hd.evt.bank != Ord then None
      else if E.is_com hd.edge then
        match tail with
        | [] -> assert false
        | next::_ ->
          if next.evt.bank = Ord then Some next
          else None
      else find_fault_com tail

  let propagate_fault by_loc =
    let rec iter_with_tail f l =
      match l with
      | [] -> ()
      | h::t -> f h t; iter_with_tail f t in
    (* Collect all the node that might faults *)
    List.iter
    ( fun node_list ->
      (* Propagate fault cross thread.
         If there is a fault check in
        `node`, it will find the next
        ordinary write cross thread in the `tail`,
        under the condition there is no other
        write in between. If such next ordinary
        write exists, we propagate the fault
        check to it.*)
      iter_with_tail ( fun node tail ->
        match node.evt.check_fault with
          | None -> ()
          | Some (_label, fault_bool) ->
            (* circulate `tail` back to `node_list` *)
            match find_fault_com (node :: tail @ node_list) with
            | Some n ->
              if n.evt.check_fault = None then
                let check_fault = Some ((Label.next_label "L", fault_bool)) in
                n.evt <- { n.evt with check_fault }
            | None -> ()
        ) node_list
    ) by_loc

(* zyva... *)

let finish n =
  let st = (0,0),Env.empty in
(* Set locations *)
  let sd,n =
    let no =
      try begin
        (* Find the first external communication edge,
           hence satisfying `diff_proc`. If no such edge,
           find the first internal communication edge as the
           start point, `start_n` to process the cycle.
           This allows cycle containing only internal
           communications edges but different locations *)
        let start_n =
          try find_edge_prev diff_proc n
          with Not_found -> find_edge_prev int_com n in
        Some (find_edge_prev diff_loc start_n)
      end with Not_found -> None in
    match no with
    | Some n ->
        Diff,
        begin try find_edge same_loc n
        with Not_found -> Warn.fatal "This cycle changes location at every step" end
    | None -> Same,n in

  let _nv,_st =
    match sd with
    | Diff -> set_diff_loc st n
    | Same -> set_same_loc st n
    | UnspecLoc -> assert false in

  if O.verbose > 1 then begin
    eprintf "LOCATIONS\n" ;
    debug_cycle stderr n
  end ;
(* Set write values *)
  let start_node,by_loc,initvals = set_write_v n in
  if O.verbose > 1 then begin
    eprintf "INITIAL VALUES: %s\n"
      (String.concat "; "
         (List.map
            (fun (loc,k) -> sprintf "%s->%s" loc (Value.pp_v k))
            initvals)) ;
    eprintf "WRITE VALUES\n" ;
    debug_cycle stderr start_node
  end ;
(* Set load values *)
  let vs = set_read_v by_loc initvals in
  propagate_fault by_loc;
(* Set dependency values *)
  (if do_morello then set_dep_v by_loc) ;
  if O.verbose > 1 then begin
    eprintf "READ VALUES\n" ;
    debug_cycle stderr start_node ;
    eprintf "FINAL VALUES [%s]\n"
      (vs |> List.map
        ( fun (loc,(v,_pte)) -> sprintf "%s -> %s"
          (Code.pp_loc loc) (Value.pp_v v) )
        |> String.concat "," )
  end ;
  if O.variant Variant_gen.Self then check_fetch start_node;
  start_node,initvals
(* END of finish *)


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

let find_start_proc n =
  let ext_count =
    fold ( fun n acc -> if diff_proc n.edge then acc + 1 else acc ) n 0 in
  (* Reject cycle with precisely one external edge *)
  if ext_count = 1 then Warn.fatal "only one external edge";
  let p = find_non_pseudo_prev n.prev in
  if
    diff_proc p.edge
  then p.next
  else
    try begin
      let ext_n = find_edge diff_proc n in
      try find_edge same_proc ext_n
      with Not_found -> ext_n
    end with Not_found ->
      (* in the case of no external internal edges,
         find the first internal edge *)
      let int_n = find_edge int_com n in
      (* Shift to the next node, as the returning start_proc.
         This maintain the same behaviour when there is an
         external edge as the process above, especially,
         . `find_edge same_proc ext_n`, which find the first
         internal edge after the external edge.*)
      int_n.next


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
  let c,initvals = finish c in
  es,c,initvals

(*************************)
(* Gather events by proc *)
(*************************)


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
            [],[]
        end else do_rec m.next in
      if same_proc m.edge then
        m::k1,k2
      else
        [m],cons_not_nil k1 k2 in
    let k1,k2 = do_rec n in
    let nss = cons_not_nil k1 k2 in
    let nss = merge_changes n nss in
    let num_rec k =
      List.iteri (fun index ns ->
        List.iter (fun n ->
          if n.store != nil then
            n.store.evt <-  { n.store.evt with proc = index + k; };
          n.evt <- { n.evt with proc = index + k; }
        ) ns
      ) in
    num_rec 0 nss ;
    if
      not O.allow_back &&
      List.exists proc_back nss
    then Warn.fatal "Forbidden po vs. com";
    if O.verbose > 1 then begin
      eprintf "SPLITTED:\n" ; debug_procs nss
    end ;
    nss

(****************************)
(* Compute coherence orders *)
(****************************)

(* For each continuous elements to location `x` in the input,
  group those elements, accumulating through `ns`. *)
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
      | None| Some R -> k in
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
              (* Assume there is no consecutive writes to the same location *)
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
              let p = Value.to_pte n.evt.v in
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
