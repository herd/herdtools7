(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Hashconsed switch trees *)

open Printf

exception Cannot

module Make (O:Indent.S) (I:CompCondUtils.I) :
    sig
      type switch
      val compile :  I.C.prop -> switch
      val dump : Indent.t -> switch -> (I.Loc.t -> string) -> unit
    end =
  struct

(*******************************)
(* Hashconsing  infrastructure *)
(*******************************)
    module Scalar = I.C.V.Scalar
    open Hashcons

    type switch = switch_node hash_consed
    and switch_node =
      | Switch of I.Loc.t * (Scalar.t * int) list * int * switch list
      | Return of bool

    module S = struct
      type t = switch_node

      let rec equal_cases cs1 cs2 = match cs1,cs2 with
      | ([],_::_)|(_::_,[]) -> false
      | [],[] -> true
      | (i1,j1)::cs1,(i2,j2)::cs2 ->
          Scalar.compare i1 i2 = 0 &&
          Misc.int_eq j1 j2 &&
          equal_cases cs1 cs2

      let rec equal_rhs rhs1 rhs2 = match rhs1,rhs2 with
      | ([],_::_)|(_::_,[]) -> false
      | [],[] -> true
      | r1::rhs1,r2::rhs2 -> r1 == r2 && equal_rhs rhs1 rhs2

      let equal s1 s2 = match s1,s2 with
      | (Return _,Switch _)|(Switch _,Return _) -> false
      | Return b1,Return b2 -> b1 = b2
      | Switch (loc1,cs1,d1,rhs1),Switch (loc2,cs2,d2,rhs2) ->
          I.Loc.compare loc1 loc2 = 0 &&
          equal_cases cs1 cs2 &&
          Misc.int_eq d1 d2 &&
          equal_rhs rhs1 rhs2

      let rec hash_list hash_elt acc = function
        | [] -> acc
        | e::es -> hash_list hash_elt (19 * hash_elt e + acc) es

      let hash = function
        | Return false -> 0
        | Return true -> 1
        | Switch (loc,cs,d,rhs) ->
            let h = Hashtbl.hash loc in
            let h = hash_list (fun (i,j) -> 17 * (Hashtbl.hash i) + j) h cs in
            let h = 19 * d + h in
            let h = hash_list (fun s -> s.hkey) h rhs in
            h
    end

(* Hashconsing hash table *)
    module H = Hashcons.Make(S)
(* Hash table on hashconsed switch trees *)
    module HH =
      Hashtbl.Make
        (struct
          type t = switch
          let equal s1 s2 = s1 == s2
          let hash s = s.hkey
        end)


    let ht = H.create 101

    let return b = H.hashcons ht (Return b)

    let number cs d =
      let ht = HH.create 17 in
      let n = ref 0 in
      let do_number s =
        try  HH.find ht s
        with Not_found ->
          let k = !n in
          incr n ; HH.add ht s k ;
          k in
      let cs = List.map (fun (i,s) -> i,do_number s) cs in
      let d = do_number d in
      let xs = HH.fold (fun s k r -> (k,s)::r) ht [] in
      let xs =
        List.sort
          (fun (k1,_) (k2,_) -> Misc.int_compare k1 k2)
          xs in
      let rhs = List.map snd xs in
      cs,d,rhs


    let switch loc cs d =
      let cs =
        List.sort
          (fun (i1,_) (i2,_) -> Scalar.compare i1 i2)
          cs in
      let cs,d,rhs = number cs d in
      H.hashcons ht (Switch (loc,cs,d,rhs))


(***************)
(* Compilation *)
(***************)
    open ConstrGen
    open Constant

    module ScalarSet = MySet.Make(Scalar)
    module M = MyMap.Make(I.Loc)

    let add loc v m = match v with
    | Concrete i ->
        let vs = try M.find loc m with Not_found -> ScalarSet.empty in
        M.add loc (ScalarSet.add i vs) m
    | Symbolic _|Label _|Tag _ -> raise Cannot

    let rec collect m = function
      | Atom (LV (loc,v)) -> add loc v m
      | Atom (LL _|FF _) -> raise Cannot
      | Not p -> collect m p
      | And ps|Or ps -> List.fold_left collect m ps
      | Implies (p1,p2) -> collect (collect m p1) p2


    let choose_loc m =
      let locs =
        M.fold (fun loc vs k -> ((loc,vs),ScalarSet.cardinal vs)::k) m [] in
      match locs with
      | [] -> assert false
      | p0::rem ->
          let r,_ =
            List.fold_left
              (fun (_,cx as px) (_,cy as py) ->
                if cx > cy then px else py)
              p0 rem in
          r
    let do_implies p1 p2 = match p1,p2 with
    | Or [],_ -> And []
    | And [],_ -> p2
    | _,Or [] -> Not p1
    | _,And [] -> p1
    | _,_ -> Implies (p1,p2)

    let do_or p1 p2 = match p1,p2 with
    | Or ps1,Or ps2 -> Or (ps1@ps2)
    | (_,And []) | (And [],_) -> And []
    | Or ps,p -> Or (ps@[p])
    | p,Or ps -> Or (p::ps)
    | _,_ -> Or [p1;p2]

    let do_and p1 p2 = match p1,p2 with
    | And ps1,And ps2 -> And (ps1@ps2)
    | (_,Or []) | (Or [],_) -> Or []
    | And ps,p -> And (ps@[p])
    | p,And ps -> And (p::ps)
    | _,_ -> And [p1;p2]

    let do_not = function
      | Or [] -> And []
      | And [] -> Or []
      | p -> Not p

    let atom_pos loc v loc0 v0 =
      if  I.Loc.compare loc loc0 = 0 then
        Some (Scalar.compare v v0 = 0)
      else None

    let atom_neg loc vs loc0 v0 =
      if  I.Loc.compare loc loc0 = 0 then
        Some (not (ScalarSet.mem v0 vs))
      else None


    let eval atom  =
      let rec eval_rec p = match p with
      | Atom (LV (loc0,Concrete v0)) ->
          begin match atom loc0 v0 with
          | None -> p
          | Some b -> if b then And [] else Or []
          end
      | Atom _ -> assert false (* Caught in collect *)
      | Not p -> do_not (eval_rec p)
      | Or []|And [] -> p
      | Or ps -> eval_recs do_or ps
      | And ps -> eval_recs do_and ps
      | Implies (p1,p2) -> do_implies (eval_rec p1) (eval_rec p2)

      and eval_recs mk = function
        | [] -> assert false
        | [p] -> eval_rec p
        | p::ps ->
            let p1 = eval_rec p
            and p2 = eval_recs mk ps in
            mk p1 p2 in
      eval_rec

    let eval_pos loc v = eval (atom_pos loc v)
    let eval_neg loc vs = eval (atom_neg loc vs)

    let rec comp p =
      let m = collect M.empty p in
      if M.is_empty m then match p with
      | Or [] -> return false
      | And [] -> return true
      | _ -> assert false
      else
        let loc,vs = choose_loc m in
        let cls =
          ScalarSet.fold
            (fun v k -> (v,comp (eval_pos loc v p))::k)
            vs [] in
        let d = comp (eval_neg loc vs p) in
        switch loc cls d

    let compile p = comp p

(********)
(* Dump *)
(********)

    let count s =
      let t = HH.create 101 in
      let rec do_rec s =
        try
          let k = HH.find t s in
          HH.replace t s (k+1)
        with Not_found ->
          HH.add t s 1 ;
          match s.node with
          | Return _ -> ()
          | Switch (_,_,_,rhs) ->
              List.iter do_rec rhs in
      do_rec s ;
      t

    let extract_defs cs =
      (* Memory of seen nodes *)
      let tseen = HH.create 17 in
      let seen s =
        try HH.find tseen s ; true
        with Not_found ->
          HH.add tseen s () ; false in
      (* Scanning applies only once *)
      let rec extract_rec k s =
        if seen s then k
        else
          match s.node with
          | Return _ -> k
          | Switch (_,_,_,rhs)  ->
              let k =
                let c = try HH.find cs s with Not_found -> assert false in
                if c > 1 then s::k else k in
              List.fold_left extract_rec k rhs  in
      extract_rec []

    let rec dump_rec env i s cast_type = match s.node with
    | Return b -> O.fx i "return %c;" (if b then '1' else '0')
    | Switch (loc,cs,d,rhs) ->
        let rhs = Array.of_list rhs in
        let t = Array.make (Array.length rhs) [] in
        List.iter (fun (i,j) -> t.(j) <- i :: t.(j)) cs ;
        O.fx i "switch (%s%s) {" (cast_type loc) (I.Loc.dump loc) ;
        for k = 0 to Array.length t-1 do
          if k <> d then begin
            dump_cases i t.(k) ;
            dump_goto env (Indent.tab i) rhs.(k) cast_type
          end
        done ;
        begin match t.(d) with
        | [] -> ()
        | cs -> dump_cases i cs
        end ;
        O.fx i "default:" ;
        dump_goto env (Indent.tab i) rhs.(d) cast_type;
        O.fx i "}"

    and dump_cases i is =
      let pre = ref (Indent.as_string i) in
      List.iter
        (fun i ->
          O.fprintf "%scase %s:" !pre (Scalar.pp O.hexa i) ;
          pre := " ")
        is ;
      O.output "\n"

    and dump_goto env i s cast_type =
      try
        let name = HH.find env s in
        O.fx i "goto %s;" name
      with Not_found -> dump_rec env i s cast_type

    let dump i s cast_type =
      let cs = count s in
      let defs = extract_defs cs s in
      let env,_ =
        List.fold_right
          (fun s (env,k) -> (sprintf "label%02i" k,s)::env,k+1)
          defs ([],0) in
      let tenv = HH.create 17 in
      List.iter (fun (name,s) -> HH.add tenv s name) env ;
      dump_rec tenv i s cast_type;
      List.iter
        (fun (name,s) ->
          O.fx i "%s: /* occs=%i */ " name (HH.find cs s) ;
          dump_rec tenv (Indent.tab i) s cast_type)
        env ;
      ()
  end
