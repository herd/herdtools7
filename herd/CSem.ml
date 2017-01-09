(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
module Make (Conf:Sem.Config)(V:Value.S)  
    = 
  struct

    module C = CArch_herd.Make(Conf.PC)(V)
    module Act = CAction.Make(C)
    include SemExtra.Make(Conf)(C)(Act)
    let barriers = []
    let isync = None

(****************************)	  
(* Build semantics function *)
(****************************)	  

    let (>>=) = M.(>>=)
    let (>>*=) = M.(>>*=)
    let (>>|) = M.(>>|)
    let (>>::) = M.(>>::)
    let (>>!) = M.(>>!)
    let (>>>) = M.(>>>)
    let (>>>>) = M.(>>>>)

    module MOorAN = MemOrderOrAnnot
    let no_mo = MOorAN.AN []
    let mo_as_anmo mo = MOorAN.MO mo

    let read_loc is_data mo =
      M.read_loc is_data (fun loc v -> Act.Access (Dir.R, loc, v, mo, false))

    let read_exchange is_data vstored mo =
      M.read_loc is_data (fun loc v -> Act.RMW (loc,v,vstored,mo))

    let read_reg is_data r ii =
      read_loc is_data no_mo (A.Location_reg (ii.A.proc,r)) ii

    let read_mem is_data mo a =
      read_loc is_data mo (A.Location_global a)

    let read_mem_atomic is_data a loc =
      M.read_loc is_data
        (fun loc v -> Act.Access (Dir.R, loc, v, MOorAN.AN a, true))
        (A.Location_global loc)

    let write_loc mo loc v ii =
      M.mk_singleton_es (Act.Access (Dir.W, loc, v, mo, false)) ii >>! v

    let write_reg r v ii = write_loc no_mo (A.Location_reg (ii.A.proc,r)) v ii
    let write_mem mo a  = write_loc mo (A.Location_global a) 	     
    let write_mem_atomic a loc v ii =
      M.mk_singleton_es
        (Act.Access (Dir.W, A.Location_global loc, v, MOorAN.AN a, true)) ii >>! v

    let fetch_op op v mo loc =
      M.fetch op v (fun v vstored -> Act.RMW (loc,v,vstored,mo))

    let xchg is_data rloc re a ii =
      let add_mb = match a with
      | ["mb"] -> true | _ -> false in
      let aw = match a with
      | ["release"] -> a
      | _ -> ["once";]
      and ar = match a with
      | ["acquire"] -> a
      | _ -> ["once";] in
      let rmem = fun loc -> read_mem_atomic is_data ar loc ii
      and wmem = fun loc v -> write_mem_atomic aw loc v ii >>! () in
      let exch = M.linux_exch rloc re rmem wmem in
      if add_mb then
        M.mk_fence (Act.Fence  (MOorAN.AN a)) ii >>*=
        fun () -> exch >>*= 
          fun v -> M.mk_fence (Act.Fence  (MOorAN.AN a)) ii >>! v
      else exch

    let atomic_pair_allowed e1 e2 = match e1.E.iiid, e2.E.iiid with
    | Some i1,Some i2 -> i1 == i2
    | _,_ -> false


    let rec build_semantics_expr is_data e ii : V.v M.t = match e with
    | C.Const v ->
        M.unitT (V.maybevToV v)

    | C.LoadReg r -> read_reg is_data r ii
    | C.LoadMem(loc,mo) ->
        (let open MemOrderOrAnnot in
        match mo with
        | AN [] | MO _ -> build_semantics_expr is_data loc ii
        | AN (_::_) ->  begin match loc with
          | C.LoadMem (loc,AN []) ->
              build_semantics_expr is_data loc ii
          | _ ->
              Warn.user_error "Bad __load argument: %s"
                (C.dump_expr loc) end) >>=
        fun l -> read_mem is_data mo l ii
	    
    | C.Op(op,e1,e2) ->
        (build_semantics_expr is_data e1 ii >>| 
        build_semantics_expr is_data e2 ii) >>= fun (v1,v2) ->
          M.op op v1 v2

    | C.Exchange(l,e,(MOorAN.AN a)) ->
        let re = build_semantics_expr true e ii
        and rloc =  build_semantics_expr false l ii in
        xchg is_data rloc re a ii


    | C.Exchange(l,e,MOorAN.MO mo) ->
        (build_semantics_expr true e ii >>|
	build_semantics_expr false l ii)
	  >>= (fun (v,l) ->
            read_exchange is_data v mo (A.Location_global l) ii)

    | C.Fetch(l,op,e,mo) ->
	(build_semantics_expr true e ii >>|
	build_semantics_expr false l ii)
	  >>= (fun (v,l) ->
	    fetch_op op v mo (A.Location_global l) ii)


    | C.ECas(obj,exp,des,success,failure,strong) ->
        (* Obtain location of "expected" value *)
        build_semantics_expr false exp ii >>= fun loc_exp ->
        (* Obtain location of object *)
        build_semantics_expr false obj ii >>= fun loc_obj ->
       (* Non-atomically read the value at "expected" location *)
        read_mem true no_mo loc_exp ii >>*= fun v_exp ->
 (* Non-deterministic choice *)
        M.altT
           (read_mem true (mo_as_anmo failure) loc_obj ii >>*= fun v_obj ->
           (* For "strong" cas: fail only when v_obj != v_exp *)
           (if strong then M.neqT v_obj v_exp else M.unitT ()) >>= fun () -> 
           (* Non-atomically write that value into the "expected" location *)
           write_mem no_mo loc_exp v_obj ii >>!
           V.intToV 0)
          (* Obtain "desired" value *)
          (build_semantics_expr true des ii >>= fun v_des -> 
           (* Do RMW action on "object", to change its value from "expected"
              to "desired", using memory order "success" *)
           M.mk_singleton_es
             (Act.RMW (A.Location_global loc_obj,v_exp,v_des,success)) ii >>!
           V.intToV 1)


    | C.ECall _ -> Warn.fatal "Macro call in CSem"
	  
    let rec build_semantics ii : (A.program_order_index * B.t) M.t = 
      let ii =
        {ii with A.program_order_index = A.next_po_index ii.A.program_order_index;} in
      match ii.A.inst with
      | C.Seq insts -> 
          build_semantics_list insts ii 
	    
      | C.If(c,t,Some e) ->
          build_semantics_expr false c ii >>>> fun ret ->
            let ii' = 
              {ii with A.program_order_index = 
	       A.next_po_index ii.A.program_order_index;} 
            in
            let then_branch = build_semantics {ii' with A.inst = t} in
            let else_branch = build_semantics {ii' with A.inst = e} in
            M.choiceT ret then_branch else_branch
              
      | C.If(c,t,None) ->
          build_semantics_expr false c ii >>>> fun ret ->
            let ii' = 
              {ii with A.program_order_index = 
	       A.next_po_index ii.A.program_order_index;} 
            in
            let then_branch = build_semantics {ii' with A.inst = t} in
            M.choiceT ret then_branch (build_semantics_list [] ii)
      | C.DeclReg _ ->  M.unitT (ii.A.program_order_index, B.Next)	      
      | C.StoreReg(_,r,e) -> 
          build_semantics_expr true e ii >>=
          fun v -> write_reg r v ii >>=
            fun _ ->  M.unitT (ii.A.program_order_index, B.Next)
      | C.StoreMem(loc,e,mo) -> 
	  (begin
            let open MemOrderOrAnnot in
            match mo with
            | AN [] | MO _ ->  build_semantics_expr false loc ii
            | AN (_::_) -> match loc with
              | C.LoadMem (loc,AN []) -> build_semantics_expr false loc ii
              | _ ->
                  Warn.user_error "Bad __store argument: %s"
                    (C.dump_expr loc)                    
          end >>|
          build_semantics_expr true e ii) >>=
          fun (l,v) -> write_mem mo l v ii >>=
            fun _ -> M.unitT (ii.A.program_order_index, B.Next)
(* C11 mutex, not sure about them... *)
      | C.Lock (l,k) ->            
	  build_semantics_expr false l ii >>=
          fun l -> begin match k with
          | C.MutexC11 ->
	      M.altT
                (* successful attempt to obtain mutex *)
	        (M.mk_singleton_es
                   (Act.Lock (A.Location_global l, true,k)) ii)
                (* unsuccessful attempt to obtain mutex *)
                (M.mk_singleton_es
                   (Act.Lock (A.Location_global l, false,k)) ii)
          | C.MutexLinux ->
              M.mk_singleton_es
                (Act.Lock (A.Location_global l,true,k)) ii
          end
              >>= fun _ -> M.unitT (ii.A.program_order_index, B.Next)
      | C.Unlock (l,k) ->
          build_semantics_expr false l ii >>=
          fun l ->
	    M.mk_singleton_es (Act.Unlock (A.Location_global l,k)) ii
	      >>= fun _ -> M.unitT (ii.A.program_order_index, B.Next)
(********************)	  
      | C.Fence(mo) -> 
	  M.mk_fence (Act.Fence mo) ii
	    >>= fun _ -> M.unitT (ii.A.program_order_index, B.Next)
	        
      | C.Symb _ -> Warn.fatal "No symbolic instructions allowed."
      | C.PCall (f,_) -> Warn.fatal "Procedure call %s in CSem" f
            
    and build_semantics_list insts ii = match insts with
    | [] -> M.unitT (ii.A.program_order_index, B.Next)
    | inst :: insts ->
	let ii = {ii with A.inst=inst; } in
	build_semantics ii >>> fun (prog_order, _branch) -> 
          build_semantics_list insts {ii with  A.program_order_index = prog_order;}
	    
  end
    
