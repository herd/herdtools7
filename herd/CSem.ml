module Make (Conf:Sem.Config)(V:Value.S)  
    = 
  struct

    module C = CArch.Make(Conf.PC)(V)
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
		       
    let read_loc mo = M.read_loc (fun loc v -> Act.Access (Dir.R, loc, v, mo))
    let read_exchange vstored mo =
      M.read_loc (fun loc v -> Act.RMW (loc,v,vstored,mo))

    let read_reg r ii = read_loc None (A.Location_reg (ii.A.proc,r)) ii
    let read_mem mo a = read_loc mo (A.Location_global a)

    let write_loc mo loc v ii =
      M.mk_singleton_es (Act.Access (Dir.W, loc, v, mo)) ii >>! v

    let write_reg r v ii = write_loc None (A.Location_reg (ii.A.proc,r)) v ii
    let write_mem mo a  = write_loc mo (A.Location_global a) 	     
		 
    let fetch_op op v mo loc =
      M.fetch op v (fun v vstored -> Act.RMW (loc,v,vstored,mo))

    let rec build_semantics_expr e ii : V.v M.t = match e with
      | C.Const v -> 	
        M.unitT (V.maybevToV v)

      | C.Load(l,mo) ->
         begin match l with 
	  | C.Reg r -> read_reg r ii
	  | C.Mem r -> read_reg r ii >>= (fun l -> read_mem mo l ii)
	 end

      | C.Op(op,e1,e2) ->
        (build_semantics_expr e1 ii >>| 
         build_semantics_expr e2 ii) >>= fun (v1,v2) ->
        M.op op v1 v2

 (*   
      | CPP11.Elock loc ->
          build_semantics_expr loc ii >>= fun loc ->
          M.altT
          (* successful attempt to obtain mutex *)
	    (M.mk_singleton_es (Act.Lock (A.Location_global loc, true)) ii >>! 
             V.intToV 0)
          (* unsuccessful attempt to obtain mutex *)
            (M.mk_singleton_es (Act.Lock (A.Location_global loc, false)) ii >>! 
             V.intToV 1)

      | CPP11.Eunlock loc ->
          build_semantics_expr loc ii >>= fun loc ->
	  M.mk_singleton_es (Act.Unlock (A.Location_global loc)) ii >>! 
          V.intToV 0
  *)
        
    let rec build_semantics ii : (A.program_order_index * B.t) M.t = match ii.A.inst with
      | C.Seq insts -> 
        build_semantics_list insts ii 
      
      | C.If(c,t,Some e) ->
        build_semantics_expr c ii >>> fun ret ->
        let ii' = 
          {ii with A.program_order_index = 
		     A.next_po_index ii.A.program_order_index;} 
        in
        let then_branch = build_semantics {ii' with A.inst = t} in
        let else_branch = build_semantics {ii' with A.inst = e} in
        M.choiceT ret then_branch else_branch
 
      | C.If(c,t,None) ->
        build_semantics_expr c ii >>> fun ret ->
        let ii' = 
          {ii with A.program_order_index = 
		     A.next_po_index ii.A.program_order_index;} 
        in
        let then_branch = build_semantics {ii' with A.inst = t} in
        M.choiceT ret then_branch (build_semantics_list [] ii)
	
      | C.Store(l,e,mo) -> 
	 begin
	   match l with 
	   | C.Reg r -> 
              build_semantics_expr e ii >>= (fun v ->
	      write_reg r v ii)
	   | C.Mem r -> 
	      (read_reg r ii >>| 
	       build_semantics_expr e ii) >>= (fun (l,v) ->
	      write_mem mo l v ii)
	 end
	 >>= (fun _ -> M.unitT (ii.A.program_order_index, B.Next))
      
      | C.Fetch(l,op,e,mo) ->
	 (build_semantics_expr e ii >>|
	  (match l with
	  | C.Reg r -> read_reg r ii
	  | C.Mem r -> read_reg r ii >>= fun l -> 
		       read_mem (Some mo) l ii)) 
	 >>= (fun (v,l) ->
	 fetch_op op v mo (A.Location_global l) ii)
	 >>= fun _ -> M.unitT (ii.A.program_order_index, B.Next)
 		     
      | C.Exchange(l,e,mo) ->
         (build_semantics_expr e ii >>|
	    (match l with
	     | C.Reg r -> read_reg r ii
	     | C.Mem r -> read_reg r ii >>= fun l -> 
			  read_mem (Some mo) l ii)) 
	 >>= (fun (v,l) ->
         read_exchange v mo (A.Location_global l) ii)
	 >>= fun _ -> M.unitT (ii.A.program_order_index, B.Next)
      
      | C.Lock l ->
	 (match l with
	  | C.Reg r -> read_reg r ii
	  | C.Mem r -> read_reg r ii >>= fun l -> 
		       read_mem None l ii) >>= fun l ->
	 (M.altT
           (* successful attempt to obtain mutex *)
	   (M.mk_singleton_es (Act.Lock (A.Location_global l, true)) ii)

           (* unsuccessful attempt to obtain mutex *)
           (M.mk_singleton_es (Act.Lock (A.Location_global l, false)) ii)) 
	 >>= fun _ -> M.unitT (ii.A.program_order_index, B.Next)
	   
      | C.Unlock l ->
	 (match l with
	  | C.Reg r -> read_reg r ii
	  | C.Mem r -> read_reg r ii >>= fun l -> 
		       read_mem None l ii) >>= fun l ->
	 M.mk_singleton_es (Act.Unlock (A.Location_global l)) ii
	 >>= fun _ -> M.unitT (ii.A.program_order_index, B.Next)

      | C.Fence(C.F mo) -> 
	 M.mk_singleton_es (Act.Fence mo) ii
	 >>= fun _ -> M.unitT (ii.A.program_order_index, B.Next)

      | C.Symb _ -> Warn.fatal "No symbolic instructions allowed."

        
    and build_semantics_list insts ii = match insts with
      | [] -> M.unitT (ii.A.program_order_index, B.Next)
      | inst :: insts ->
	let ii = {ii with A.inst=inst; } in
	build_semantics ii >>> fun (prog_order, _branch) -> 
        build_semantics_list insts {ii with 
				     A.program_order_index = prog_order;}
     
  end
