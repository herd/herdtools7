(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Utilities for Sela's models *)

open Printf

module type S = sig
  type event
  type event_set
  type event_rel
  type concrete

(* Events relevant to Sela's models *)
  val evt_relevant : event -> bool
  val globally_visible : event -> bool

  type nature = Exe | Com | Prop of int

  type splitted =
      { nature:nature ; event:event; }

  val is_satisfy : splitted -> bool
  val is_init : splitted -> bool
  val is_commit : splitted -> bool

  val relevant_to_proc : splitted -> int -> bool

  val pp_splitted : splitted -> string

  type e2pred = event * event -> bool

  type model =
      { fbefore:e2pred ; fafter:e2pred;
        flocal: splitted -> splitted -> splitted -> bool; }

(* Set and maps on splitted events *)
  module SplittedSet : MySet.S
  with type elt = splitted


  module SplittedRel :  InnerRel.S
  with type elt0 = splitted
  and module Elts = SplittedSet
  val pp_splitted_rel : SplittedRel.t -> string
  val vb_pp_splitted : SplittedRel.t -> (string * event_rel) list

(* Local order *)

  type loc_ord

  val pp_loc_ord : loc_ord -> (string * event_rel) list

  val proj_loc_ord :  SplittedRel.t -> loc_ord

  val splitted_loc_ord : 
      model ->
        concrete ->
          event_set ->
            event_rel ->
              event_rel -> SplittedRel.t

  val mk_loc_ord :
      model ->
        concrete ->
          event_set ->
            event_rel ->
              event_rel ->
                loc_ord

  val mem_loc_ord : (splitted * splitted) -> loc_ord -> bool


end

module type Conf = sig
  type event
  val visible_fence : event -> bool
end

module Make(S:Sem.Semantics) (Conf:Conf with type event = S.event) : S with
type event = S.event
and type event_set = S.event_set
and type event_rel = S.event_rel
and type concrete = S.concrete
=
  struct
    type event = S.event
    type event_set = S.event_set
    type event_rel = S.event_rel
    type concrete = S.concrete

    module E = S.E

(* Events relevant to Sela's model *)
    let evt_relevant x = E.is_mem x || E.is_commit x || E.is_barrier x
 
    let globally_visible x = E.is_mem_store x || Conf.visible_fence x

    type nature = Exe | Com | Prop of int

    let pp_nature = function
      | Exe -> "e"
      | Com -> "c"            
      | Prop i -> sprintf "t%i" i

    let nature_compare n1 n2 = Pervasives.compare n1 n2

    type splitted =
      { nature:nature ; event:event; }

    let pp_splitted e =
      sprintf "%s-%s" (S.E.pp_eiid e.event) (pp_nature e.nature)

    let splitted_compare e1 e2 = match S.E.event_compare e1.event e2.event with
    | 0 -> nature_compare e1.nature e2.nature
    | r -> r

    let is_satisfy e =
      E.is_load e.event &&
      (match e.nature with | Exe -> true | _ -> false)

    let is_init e =
      E.is_store e.event &&
      (match e.nature with | Exe -> true | _ -> false)

    let is_commit e = 
      match e.nature with | Com -> true | _ -> false

    let locally_relevant xe =
      if globally_visible xe.event then
        match xe.nature with
        | Com -> true
        | Exe|Prop _ -> false
      else
        match xe.nature with
        | Exe -> true
        | Com|Prop _ -> false

    let proc_eq = Misc.int_eq

    let relevant_to_proc xe i =
      let x = xe.event in
      let px = match E.proc_of x with
      | Some px -> px | None -> assert false  in
      if proc_eq px i then locally_relevant xe
      else
        match xe.nature with
        | Prop j -> proc_eq i j
        | Exe|Com -> false

    type e2pred = event * event -> bool

    type model =
        { fbefore:e2pred ; fafter:e2pred;
          flocal: splitted -> splitted -> splitted -> bool; }

    module OrderedSplitted = struct
      type t = splitted
      let compare = splitted_compare
    end

    module SplittedSet = MySet.Make(OrderedSplitted)
    module SplittedRel = InnerRel.Make(OrderedSplitted)

    let pp_splitted_rel r =
      SplittedRel.pp_str ""
        (fun (e1,e2) ->
          sprintf "<%s,%s>\n" (pp_splitted e1) (pp_splitted e2))
        r

    module M =
      Map.Make
        (struct
          type t = nature * nature
          let compare = Pervasives.compare
        end)

    let add_pair k (x,y as v) m =
      if E.event_compare x y <> 0 then
        let vs = try M.find k m with Not_found -> [] in
        M.add k (v::vs) m
      else m


    let rt = E.EventRel.remove_transitive_edges

    let spiltted2r r =
      let m =
        SplittedRel.fold
          (fun (xe,ye) -> add_pair (xe.nature,ye.nature) (xe.event,ye.event))
          r M.empty in
      let vb_pp =
        M.fold
          (fun (n1,n2) pairs k ->            
            let tag =  sprintf "%s -> %s" (pp_nature n1) (pp_nature n2)
            and r = E.EventRel.of_list pairs in
            (tag,r)::k)
          m [] in
      List.map
        (fun (tag,r) -> tag, rt r)
        vb_pp
            
    let vb_pp_splitted r = spiltted2r r 
      

(***************)
(* Local order *)
(***************)

(* Projection of loc_ord onto four event relations *)
  type loc_ord =
      {eord:E.EventRel.t;
       cord:E.EventRel.t;
       e2c:E.EventRel.t;
       c2e:E.EventRel.t; }

    let rt = E.EventRel.remove_transitive_edges

    let pp_loc_ord loc_ord =
      ("eord",rt loc_ord.eord)::
      ("cord",rt loc_ord.cord)::
      ("e2c",rt loc_ord.e2c)::
      ("c2e",rt loc_ord.c2e)::[]


    let loc_ord0 =
      {eord=E.EventRel.empty ;
       cord=E.EventRel.empty ;
       e2c=E.EventRel.empty ;
       c2e=E.EventRel.empty ; }
  
  let proj_loc_ord r =
    SplittedRel.fold
      (fun (xe,ye) k ->
        let x = xe.event and y = ye.event in
        match xe.nature,ye.nature with
        | Exe,Exe -> { k with eord = E.EventRel.add (x,y) k.eord; }
        | Com,Com -> { k with cord = E.EventRel.add (x,y) k.cord; }
        | Exe,Com -> { k with e2c = E.EventRel.add (x,y) k.e2c; }
        | Com,Exe -> { k with c2e = E.EventRel.add (x,y) k.c2e; }
        | Prop _,_
        | _,Prop _ -> assert false)
      r loc_ord0



(* Big loc_ord (on events) *)
      
  let fold_se f e k =
    f { nature=Exe; event=e;}
      (f  { nature=Com; event=e;} k)
      
  let fold_se3 f x y z =
    fold_se
      (fun xe ->
        fold_se
          (fun ye ->
            fold_se
              (fun ze -> f xe ye ze) z)
          y)
      x


  let flocal m xe ye ze k =
    if m.flocal xe ye ze then
      (xe,ye)::k
    else
      k

  let splitted_loc_ord m conc evts rf po =
    let pos = E.proj_proc_view conc.S.str  po in
    let r1 =
      let rs =
        List.map
          (fun po ->
            let pairs =
              E.EventRel.fold
                (fun (x,y) k ->
                  let zs =
                    E.EventSet.inter
                      (E.EventRel.reachable x po)
                      (E.EventRel.up y po) in
                  E.EventSet.fold
                    (fun z k ->  fold_se3 (flocal m) x y z k)
                    zs k)
                po [] in
            SplittedRel.of_list pairs)
          pos in
      rs
    and r2 =
      let pairs =
        E.EventRel.fold
          (fun (w,r) k ->
            if E.same_proc w r then
              ({nature = Exe ; event=w;},
               {nature = Exe ; event=r;})::k
            else k)
          rf [] in
      SplittedRel.of_list pairs
    and r3 =
      let pairs =
        E.EventSet.fold
          (fun e k ->
            ({nature = Exe ; event=e;},
             {nature = Com ; event=e;})::k)
          evts [] in
      SplittedRel.of_list pairs in
    SplittedRel.transitive_closure
      (SplittedRel.unions  (r3::r2::r1))

    let mk_loc_ord m conc evts rf po =
      let r = splitted_loc_ord m conc evts rf po in
      proj_loc_ord r

    let mem_loc_ord (e1,e2) loc_ord =
      let r =
        match e1.nature,e2.nature with
        | Exe,Exe -> loc_ord.eord
        | Exe,Com -> loc_ord.e2c
        | Com,Exe -> loc_ord.c2e
        | Com,Com -> loc_ord.cord
        | Prop _,_ | _,Prop _ -> assert false in
      E.EventRel.mem (e1.event,e2.event) r
        
  end
