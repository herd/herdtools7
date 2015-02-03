(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Internal keys for mcompare *)

open LogState

(* A key is a reference for a row, it features the name of
   the test, plus information left abstract *)

type 'a t = { name : string ; info : 'a }

module type Config = sig
  val verbose : int
  val kinds : LogState.kind TblRename.t
  val conds : LogConstr.constr TblRename.t
end

module Make(Opt:Config) = struct
  module W = Warn.Make(Opt)
  module LS = LogState.Make(Opt)

  module None = struct
    type info = unit

    let add_names ts = Array.map (fun n -> { name = n; info = (); }) ts

    let add_log t =
      Array.map (fun n -> { name = n.tname; info = (); }) t.tests
  end


  module Kind = struct
    type info = { kind : LogState.kind ; loop : bool }

    let add_kind_tests nks ts =
      let sz_nks = Array.length nks in
      let sz_ts = Array.length ts in
      let tout = ExtArray.create () in
      let out x = ExtArray.add tout x in
      let rec do_rec idx_nks idx_ts =
        if idx_nks >= sz_nks || idx_ts >= sz_ts then
          ExtArray.blit tout nks idx_nks (sz_nks-idx_nks)
        else
          let (n,k,loop as nk) = nks.(idx_nks)
          and t = ts.(idx_ts) in
          let c = String.compare n t.tname in
          if c < 0 then begin
            out nk ; do_rec (idx_nks+1) idx_ts
          end else if c > 0 then
            do_rec idx_nks (idx_ts+1)
          else (* c=0 *) match k with
          | None -> 
              out (n,Some t.LogState.kind,t.LogState.loop) ;
              do_rec (idx_nks+1) (idx_ts+1)
          | Some k1 ->
              let k2 = t.LogState.kind in
              if k1=k2 then
                out (n,Some k1,loop || t.LogState.loop)
              else begin
                W.warn "Kind variation for test %s" n ;
                out (n,Some NoKind,loop || t.LogState.loop)
              end ;
              do_rec (idx_nks+1) (idx_ts+1) in
      do_rec 0 0 ;
      ExtArray.to_array tout

    let add names ts =
      let nks_init = Array.map (fun x -> x,None,false) names in
      let nks =
        List.fold_left
          (fun nks t -> add_kind_tests nks t.tests)
          nks_init ts in
      Array.map
        (fun (n,k,loop) -> match k with
        | None ->
            { name = n ;
              info = { kind = NoKind ; loop = false}}
        | Some k ->
            { name = n ;
              info = { kind = k ; loop = loop}})
        nks

    let pps =
      Array.map
        (fun key ->
          let k = key.info.kind
          and loop = key.info.loop in
          let k = LS.pp_kind k in
          if loop then [k;"Loop"] else [k])

  end

(* complete information: ie test result for first column *)
  module Full = struct
    type info = LogState.test

    let add log =
      Array.map
        (fun t -> { name = t.tname ; info = t; })
        log.tests
  end

(* Condition *)

  module Cond = struct

    type info = 
     { cond : LogConstr.constr option ; unsure : bool ; kind : LogState.kind;}


    let sure k c = { cond = c ; unsure = false; kind=k; }
    let unsure k c = { cond = c ; unsure = true; kind=k; }

    let change_condition k c =  match c with
    | None -> sure k None
    | Some c ->
        let p = ConstrGen.prop_of c in
        let sure c = sure k (Some c) in
        match k with
        | Forbid -> sure (ConstrGen.NotExistsState p)
        | Allow -> sure (ConstrGen.ExistsState p)
        | Require -> sure (ConstrGen.ForallStates p)
        | Undefined -> sure  (ConstrGen.ExistsState p)
        | NoKind -> unsure k (Some c)
        | ErrorKind -> assert false

    let add_col t1 =
      Array.map
        (fun t ->          
          let _k,c =
            let c,from_log =
              try
                Some (TblRename.find_value Opt.conds t.tname),false
              with Not_found ->  t.condition,true in 
            try
              let k = TblRename.find_value Opt.kinds t.tname in
              k,change_condition k c
            with Not_found ->
              let k = match c with
              | None -> NoKind
              | Some c ->
                  let open ConstrGen in
                  begin match c with
                  | NotExistsState _ -> LogState.Forbid
                  | ExistsState _ -> LogState.Allow
                  | ForallStates _ -> LogState.Require
                  end in
              k,if from_log then unsure k c else sure k c in
          { name = t.tname ; info = c; })
        t1.tests

    let add = add_col

    let merge_cond x y = match x.cond,y.cond with
    | Some _,_ -> x
    | _,Some _ -> y
    | _,_ -> x

    let merge_info xs ys =
      let sz_xs = Array.length xs
      and sz_ys = Array.length ys in
      let tout = ExtArray.create () in
      let out x = ExtArray.add tout x in
      let rec loop i_xs i_ys =
        if i_xs >= sz_xs then
          ExtArray.blit tout ys i_ys (sz_ys-i_ys)
        else if i_ys >= sz_ys then
          ExtArray.blit tout xs i_xs (sz_xs-i_xs)
        else
          let x = xs.(i_xs) and y = ys.(i_ys) in
          let c = String.compare x.name y.name in
          if c < 0 then begin
            out x ; loop (i_xs+1) i_ys
          end else if c > 0 then begin
            out y ; loop i_xs (i_ys+1)
          end else begin
            out { x with info = merge_cond x.info y.info; } ;
             loop (i_xs+1) (i_ys+1)
          end in
      loop 0 0 ;
      ExtArray.to_array tout

    let rec merge_infos xs = function
      | [] -> xs
      | ys::rem -> merge_infos (merge_info xs ys) rem

    let adds tss =
      let ess = List.map add_col tss in
      match ess with
      | [] -> [| |]
      | xs::rem -> merge_infos xs rem
  end
end
