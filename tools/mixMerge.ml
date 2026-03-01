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

module Make(O:MixOption.S)(A:Arch_tools.S) : sig
  val merge : Name.t -> A.test -> Name.t -> A.test -> A.test
  val append : Name.t -> A.test -> Name.t -> A.test -> A.test
  val cat : Name.t -> A.test -> Name.t -> A.test -> A.test
end =
  struct

    open MiscParser
    module Alpha = Alpha.Make(O)(A)
    module MixPerm = MixPerm.Make(O)(A)


(* Merge of conditions *)
    open ConstrGen

    let or_cond c1 c2 = match c1,c2 with
    | ExistsState e1,ExistsState e2 -> ExistsState (Or [e1;e2])
    | ForallStates e1,ForallStates e2 -> ForallStates (Or [e1;e2])
    | NotExistsState e1,NotExistsState e2 -> NotExistsState (And [e1;e2])
    | _,_ ->
        let p1 = prop_of c1
        and p2 = prop_of c2 in
        ExistsState (Or [p1;p2])

    let and_cond c1 c2 = match c1,c2 with
    | ExistsState e1,ExistsState e2 -> ExistsState (And [e1;e2])
    | ForallStates e1,ForallStates e2 -> ForallStates (And [e1;e2])
    | NotExistsState e1,NotExistsState e2 -> NotExistsState (Or [e1;e2])
    | _,_ ->
        let p1 = prop_of c1
        and p2 = prop_of c2 in
        ExistsState (And [p1;p2])

    let choose_cond =
      let open MixOption in
      match O.cond with
      | Cond.Or -> fun _ -> or_cond
      | Cond.And -> fun _ -> and_cond
      | Cond.Auto -> fun c -> c
      | Cond.No ->  fun _ _ _ -> ForallStates (And [])

(* merge of locations *)

(*******)
(* Mix *)
(*******)

    let rec mix_list xs sx ys sy = match xs, ys with
    | [],[] -> []
    | x::rx, [] -> x::mix_list rx (sx-1) ys sy
    | [],y::ry -> y::mix_list xs sx ry (sy-1)
    | x::rx, y::ry ->
        if Random.int(sx+sy) < sx then
          x::mix_list rx (sx-1) ys sy
        else
          y::mix_list xs sx ry (sy-1)

    let clean_code =
      List.filter
        (fun i -> match i with
        | A.Nop -> false
        | A.Label _
        | A.Instruction _
        | A.Symbolic _
        | A.Macro _ 
        | A.Pagealign -> true
        | A.Skip _ -> assert false)

    let mix_code c1 c2 =
      let c1 = clean_code c1
      and c2 = clean_code c2 in
      mix_list c1 (List.length c1) c2 (List.length c2)

    let rec mix_prog p1 p2 = match p1,p2 with
    | ([],p)|(p,[]) -> p
    | (p,c1)::p1,(_,c2)::p2 -> (p,mix_code c1 c2)::mix_prog p1 p2


    let mix_cond = choose_cond or_cond

    let merge _doc1 t1 doc2 t2 =
      let t2 = MixPerm.perm doc2 t2 in
      let t2 = Alpha.alpha doc2 t1 t2 in
      { t1 with
        init = t1.init @ t2.init ;
        prog = mix_prog t1.prog t2.prog ;
        locations = t1.locations @ t2.locations ;
        condition = mix_cond t1.condition t2.condition ; }

(**********)
(* Append *)
(**********)

    let append_cond = choose_cond and_cond

    let append_code c1 c2 = c1@c2

    let rec append_prog p1 p2 = match p1,p2 with
    | ([],p)|(p,[]) -> p
    | (p,c1)::p1,(_,c2)::p2 -> (p,append_code c1 c2)::append_prog p1 p2


    let rec replicate x n = if n <= 0 then [] else x::replicate x (n-1)

    let rec add_end len = function
      | [] -> replicate A.Nop len
      | x::xs -> x::add_end (len-1) xs

    let same_length prg =
      let len =
        List.fold_left (fun m (_,c) -> max m (List.length c)) 0 prg in
      let prg =
        List.map
          (fun (p,c) -> p,add_end (len+1) c)
          prg in
      prg

    let append _doc1 t1 doc2 t2 =
      let t2 = MixPerm.perm doc2 t2 in
      let t2 = Alpha.alpha doc2 t1 t2 in
      { t1 with
        init = t1.init @ t2.init ;
        prog = append_prog (same_length t1.prog) t2.prog ;
        locations = t1.locations @ t2.locations ;
        condition = append_cond t1.condition t2.condition ; }

(********************************************)
(* Just concatenate in horizontal direction *)
(********************************************)

    let nprocs t = List.length t.prog

    let shift_location k loc = match loc with
    | A.Location_reg (i,r) -> A.Location_reg (i+k,r)
    | A.Location_global _ -> loc

    let shift_rloc k = ConstrGen.map_rloc (shift_location k)

    let shift_state_atom k (loc,v) = shift_location k loc,v

    let shift_state k = List.map (shift_state_atom k)

    let shift_locations k =
      let open LocationsItem in
      List.map
        (function
          | Loc (l,v) -> Loc (shift_rloc k l,v)
          | Fault ((i,lbls),x,ft) -> Fault ((i+k,lbls),x,ft))

    let shift_atom k a = match a with
    | LV (l,v) ->  LV (shift_rloc k l,v)
    | LL (a,b) -> LL (shift_location k a,shift_location k b)
    | FF ((i,lbls),x,ft) -> FF ((i+k,lbls),x,ft)

    let shift_constr k = ConstrGen.map_constr (shift_atom k)

    let shift_prog k prog =  List.map (fun ((i,ao,func),code) -> (i+k,ao,func),code) prog

    let shift k t =
      { t with
        init = shift_state k t.init;
        locations = shift_locations k t.locations;
        condition = shift_constr k t.condition;
        prog = shift_prog k t.prog;
      }

    let cat _doc1 t1 doc2 t2 =
      let t2 = Alpha.global doc2 t1 t2 in
      let n1 = nprocs t1 in
      let t2 = shift n1 t2 in
      { t1 with
        init = t1.init @ t2.init ;
        prog = t1.prog @ t2.prog ;
        locations = t1.locations @ t2.locations ;
        condition = append_cond t1.condition t2.condition ; }


end
