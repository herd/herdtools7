(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** A monad for event structures *)

module type Config = sig
    val debug : Debug.t
end

module Make (C:Config) (A:Arch.S) (E:Event.S with module A = A and module Act.A = A) :
(Monad.S with module A = A and module E = E 
	  and type evt_struct = E.event_structure) = 
struct 
    
      module A = A
      module E = E
      module V = A.V
      module VC =
        Valconstraint.Make
          (struct let debug = C.debug.Debug.solver end)
          (A)


(* LM: What'a that ??? is  Pervasives.compare adequate here ???
   module Evt =
   Set1.Make
   (struct
   type 'a t = ('a * VC.cnstrnts * E.event_structure)
   let compare = Pervasives.compare
   end)
 *)

(* LM Use lists for polymorphism.
   It is assumed that list elts are pairwise distinct.
*)
     module Evt = struct
	type 'a t =  ('a * VC.cnstrnts * E.event_structure) list

	let empty = []

	let singleton x = [x]

	let is_empty = function
	  | [] -> true
	  | _::_ -> false

       let as_singleton = function
         | [x] -> x
         | _ -> assert false

	let add x s = x::s

	let map = List.map

	let fold f xs y0 =
	  List.fold_left (fun x y -> f y x) y0 xs

	let filter = List.filter

	let union = (@)

	let elements (x:'a t) = x
      end

      type 'a t = int -> int * ('a Evt.t) (* Threading through eiid *)
	  
      let zeroT : 'a t
	  = (fun eiid_next -> (eiid_next, Evt.empty))
	  
      let unitT (v : 'a) : 'a t =
	fun eiid_next ->
	  eiid_next,Evt.singleton (v, [], E.empty_event_structure)
	      
      let (=**=) = E.(=**=)
      let (=*$=) = E.(=*$=)
      let (=|=) = E.(=|=)
      let (+|+) = E.(+|+)

(* Bind the result *)
      let (>>=) : 'a t -> ('a -> 'b t) -> ('b) t
	  = fun s f ->
	    (fun eiid ->
	      let (eiid_next, sact) = s eiid in
	      Evt.fold (fun (v1, vcl1, es1) (eiid1,acc) -> 
		let b_set = f v1 in
		let (eiid_b,b_setact) = b_set eiid1 in
		Evt.fold (fun (v2,vcl2,es2) (eiid2,acc_inner) -> 
		  match es1 =*$= es2 with
                  | None -> (eiid2, acc_inner)
		  | Some es -> (eiid2,Evt.add (v2,vcl2@vcl1,es) acc_inner)
			 )
		  b_setact (eiid_b,acc)
		       ) 
		sact (eiid_next,Evt.empty))

(* Bind the result *)
      let (>>*=) : 'a t -> ('a -> 'b t) -> ('b) t
	  = fun s f ->
	    (fun eiid ->
	      let (eiid_next, sact) = s eiid in
	      Evt.fold (fun (v1, vcl1, es1) (eiid1,acc) -> 
		let b_set = f v1 in
		let (eiid_b,b_setact) = b_set eiid1 in
		Evt.fold (fun (v2,vcl2,es2) (eiid2,acc_inner) -> 
		  match es1 =**= es2 with
                  | None -> (eiid2, acc_inner)
		  | Some es -> (eiid2,Evt.add (v2,vcl2@vcl1,es) acc_inner)
			 )
		  b_setact (eiid_b,acc)
		       ) 
		sact (eiid_next,Evt.empty))

(* Exchange combination *)
     let exch : 'a t -> 'a t -> ('a -> 'b t) ->  ('a -> 'b t) ->  ('b * 'b) t
         = fun rx ry wx wy ->
           fun eiid ->
             let eiid,rxact = rx eiid in
             let eiid,ryact = ry eiid in
             let (vrx,vclrx,esrx) = Evt.as_singleton rxact
             and (vry,vclry,esry) = Evt.as_singleton ryact in
             let eiid,wxact = wx vry eiid in
             let eiid,wyact = wy vrx eiid in
             let vwx,vclwx,eswx = Evt.as_singleton wxact
             and vwy,vclwy,eswy = Evt.as_singleton wyact in
             let es = 
               E.exch esrx esry eswx eswy in
             eiid,Evt.singleton((vwx,vwy),vclrx@vclry@vclwx@vclwy,es)
           
(* stu comninator *)
  let stu : 'a t -> 'b t -> ('a -> unit t) -> (('a * 'b) -> unit t) -> unit t
      = fun rD rEA wEA wM  ->
        fun eiid ->
          let eiid,rd = rD eiid in
          let eiid,rea = rEA eiid in
          let (vrd,vclrd,esrd) = Evt.as_singleton rd
          and (vrea,vclrea,esrea) = Evt.as_singleton rea in
          let eiid,wea = wEA vrea eiid in
          let eiid,wm = wM (vrd,vrea) eiid in
          let (_vwea,vclwea,eswea) = Evt.as_singleton wea
          and (_vwm,vclwm,eswm) = Evt.as_singleton wm in
          let es = E.stu esrd esrea eswea eswm in
          eiid,Evt.singleton ((),vclrd@vclrea@vclwea@vclwm,es)

(* Combine the results *)
      let (>>|) : 'a t -> 'b t -> ('a * 'b)  t
	  = fun s1 s2 ->
	    (fun eiid ->
	      let (eiid_next, s1act) = s1 eiid in
              Evt.fold (fun (v1,vcl1,es1) (eiid1,acc) ->
		let (eiid2,s2act) = s2 eiid1 in
		Evt.fold (fun (v2,vcl2,es2) (eiid3,acc_inner) -> 
		  match es1 =|= es2 with 
                  | None -> (eiid3,acc_inner)
		  | Some es -> (eiid3,Evt.add ((v1,v2),vcl2@vcl1,es) acc_inner)) 
		  s2act (eiid2, acc)) 
		s1act (eiid_next,Evt.empty)) 
	      
(* Combine the results *)
      let (>>::) : 'a t -> 'a list t -> 'a list  t
	  = fun s1 s2 ->
	    (fun eiid ->
	      let (eiid_next, s1act) = s1 eiid in
              Evt.fold (fun (v1,vcl1,es1) (eiid1,acc) ->
		let (eiid2,s2act) = s2 eiid1 in
		Evt.fold (fun (v2,vcl2,es2) (eiid3,acc_inner) -> 
		  match es1 =|= es2 with 
                  | None -> (eiid3,acc_inner)
		  | Some es -> (eiid3,Evt.add (v1 :: v2,vcl2@vcl1,es) acc_inner)) 
		  s2act (eiid2, acc)) 
		s1act (eiid_next,Evt.empty)) 
	      
      let lockT : 'a t -> 'a t
	  = fun s ->
	    fun eiid ->
	      let (eiid1,sact) = s eiid in
              (eiid1,
	       Evt.map (fun (v, vcl,es) -> (v, vcl, E.lock_all_events es))
		 sact)
		

(* Force monad value *)
      let forceT : 'a -> 'b t -> 'a t =
	fun v s eiid ->
	let (eiid,sact) = s eiid in
	(eiid,Evt.map
           (fun (_,vcl,es) -> (v,vcl,es))
           sact)

      let (>>!) s v = forceT v s
(*      let (>>!!) s f a = forceT (f a) s *)

      let discardT : 'a t -> unit t =
	fun s eiid -> forceT () s eiid

	      
(* Add a value *)
      let addT : 'a -> 'b t -> ('a * 'b) t
	  = fun v s eiid ->
	    let (eiid1,sact) = s eiid in
	    (eiid1,Evt.map
	       (fun (vin, vcl, es) -> ((v, vin), vcl, es))
	       sact)

(* Filter by values *)
      let filterT : V.v -> V.v t -> V.v t
	  = fun v s eiid ->
	    let (eiid1, sact) = s eiid in
(* In concrete value world, the next line filters out only the value exactly equal as specified.
   In symbolic value world, we have a choice. The inner check can always return true, and checking
   can be postponed, OR, we can do a tiny bit of constraint solving now, and throw out the
   ones we know are impossible already *)
	    let poss_with_old_cnstrnts = 
	      Evt.filter (fun (vin,_,_) -> V.equalityPossible vin v) sact in 
	    let poss_with_upd_cnstrnts = 
	      Evt.map (fun (vin,vcl,es) -> (vin, (VC.Assign (vin, (VC.Atom v))) :: vcl, es)) poss_with_old_cnstrnts in 
	    (eiid1, poss_with_upd_cnstrnts)


(* Add an inequality constraint *)
 let neqT : V.v -> V.v -> unit t
   = fun v1 v2 eiid ->
     (eiid, Evt.singleton 
        ((), [VC.Assign (v1, VC.Unop(Op.Not, v2))], E.empty_event_structure))
            
(* Choosing dependant upon flag,
   notice that, once determined v is either one or zero *)
      let choiceT : V.v -> 'a t -> 'a t -> 'a t =
	fun v l r eiid -> 
	  if V.is_var_determined v then
	    if V.is_zero v  then r eiid else l eiid
	  else 
	    let (eiid, lact) = l eiid in
	    let (eiid, ract) = r eiid in
	    let un = 
	      Evt.union 
		(Evt.map (fun (r,cs,es) ->
		  (r,(VC.Assign (v,VC.Atom V.one)) :: cs,es))
		   lact)
		(Evt.map
		   (fun (r,cs,es) ->
		     (r,(VC.Assign (v,VC.Atom V.zero)) :: cs, es))
		   ract) in
	    (eiid, un)

      let altT : 'a t -> 'a t -> 'a t =
	fun m1 m2 eiid -> 
	    let (eiid, act1) = m1 eiid in
	    let (eiid, act2) = m2 eiid in
	    let un =  Evt.union  act1 act2 in
(*
		(Evt.map (fun (r,cs,es) -> (r,cs,es))  act1)
		(Evt.map (fun (r,cs,es) -> (r,cs, es)) act2)
*)
	    (eiid, un)

	
      let (|*|) : unit t -> unit t -> unit t
	  = fun s1 s2 ->
	    (fun eiid ->
	      let (eiid_next, s1act) = s1 eiid in
	      let (eiid_final, s2act) = s2 eiid_next in
	      let s1lst = Evt.elements s1act in
	      let s2lst = Evt.elements s2act in
	      (eiid_final,
	       if Evt.is_empty s2act then s1act 
	       else if Evt.is_empty s1act then s2act
	       else 
		 List.fold_left
		   (fun acc (_,vcla,evta) ->
		     List.fold_left
		       (fun acc_inner (_,vclb,evtb) ->
			 match evta +|+ evtb with
			 | Some evtc -> Evt.add ((), vcla@vclb, evtc) acc_inner
			 | None      -> acc_inner
		       )
                       acc s2lst
		   )
		   Evt.empty s1lst))

(* For combining instruction + next instructions.
   Notice: no causality from s to f v1 *)
      let (>>>) s f = fun eiid ->
	let (eiid_next, sact) = s eiid in
	Evt.fold
	  (fun (v1, vcl1, es1) (eiid1,acc) -> 
	    let b_set = f v1 in
	    let eiid_b,b_setact = b_set eiid1 in
	    Evt.fold
	      (fun (v2,vcl2,es2) (eiid2,acc_inner) ->
		match es1 +|+ es2 with
                | None -> eiid2, acc_inner
		| Some es ->
		    eiid2,Evt.add (v2,vcl2@vcl1,es) acc_inner)
	      b_setact (eiid_b,acc)) 
	  sact (eiid_next,Evt.empty)
	      
(* trivial event_structure with just one event
   and no relation *)

      let do_trivial es =
      	{ E.procs = [] ;
	  events = es ;
	  intra_causality_data = E.EventRel.empty ;
	  intra_causality_control = E.EventRel.empty ;
	  atomicity = E.Atomicity.empty ; }

      let trivial_event_structure e = do_trivial (E.EventSet.singleton e)

      let read_loc mk_action loc ii =
	fun eiid ->
	  V.fold_over_vals
	    (fun v (eiid1,acc_inner) ->
	      (eiid1+1,
	       Evt.add 
		 (v, [],
		  trivial_event_structure
		    {E.eiid = eiid1 ;
		     E.iiid = Some ii;
		     E.action = mk_action loc v })
		 acc_inner)) (eiid,Evt.empty)	

      let mk_singleton_es a ii =
	fun eiid ->
	  (eiid+1,
	   Evt.singleton
	     ((), [],
	      trivial_event_structure
		{E.eiid = eiid ;
		 E.iiid = Some ii;
		 E.action = a }))

(*
      let mk_singleton_es_eq a x rr y ii =
	fun eiid ->
	  (eiid+1,
	   Evt.singleton
	     ((), [VC.Assign (x, VC.Atom y); VC.Assign (rr,VC.Atom V.one)],
	      trivial_event_structure
		{E.eiid = eiid ;
		 E.iiid = Some ii;
		 E.action = a }))
*)
      let mk_singleton_es_eq a eqs ii =
	fun eiid ->
	  (eiid+1,
	   Evt.singleton
	     ((), eqs,
	      trivial_event_structure
		{E.eiid = eiid ;
		 E.iiid = Some ii;
		 E.action = a }))


      let any_op mk_v mk_c =
	(fun eiid_next -> 
	  eiid_next,
	  begin try
	    let v = mk_v () in
	     Evt.singleton
	       (v, [], E.empty_event_structure)
	  with V.Undetermined ->
	    let v = V.fresh_var () in
	    Evt.singleton
	      (v, [VC.Assign (v, mk_c ())], E.empty_event_structure)
	  end)
	
      let op1 op v1 =
	any_op
	  (fun () -> V.op1 op v1)
	  (fun () -> VC.Unop (op,v1))

      and op op v1 v2 =
	any_op
	  (fun () -> V.op op v1 v2)
	  (fun () -> VC.Binop (op,v1,v2))

      and op3 op v1 v2 v3 =
	any_op
	  (fun () -> V.op3 op v1 v2 v3)
	  (fun () -> VC.Terop (op,v1,v2,v3))

      let add =  op Op.Add

      let assign v1 v2 =
	fun eiid ->
	  eiid,
	   Evt.singleton
	     ((), [(VC.Assign (v1,VC.Atom v2))],
	      E.empty_event_structure)

     let fetch op arg mk_action ii =
       fun eiid ->
	 V.fold_over_vals
	   (fun v (eiid1,acc_inner) ->
             let vstored = V.fresh_var () in
	     (eiid1+1,
	      Evt.add 
		(v, [VC.Assign (vstored,VC.Binop (op,v,arg))],
		 trivial_event_structure
		   {E.eiid = eiid1 ;
		    E.iiid = Some ii;
		    E.action = mk_action v vstored})
		acc_inner)) (eiid,Evt.empty)	

      let tooFar _msg = zeroT
(*
	fun eiid ->
	  eiid,
	   Evt.singleton
	     ((), [VC.Unroll msg],E.empty_event_structure)
*)
	  
      type evt_struct = E.event_structure
      type output = VC.cnstrnts * evt_struct

      let initwrites env =
        fun eiid ->
          let eiid,es =
            List.fold_left
              (fun (eiid,es) (loc,v) ->
                let ew =
                  {E.eiid = eiid ;
		   E.iiid = None ;
 		   E.action = E.Act.mk_init_write loc v ;} in
                (eiid+1,ew::es))
              (eiid,[]) env in
          let es = E.EventSet.of_list es in
(*          Printf.eprintf "Init writes %a\n" E.debug_events es; *)
          eiid,
          Evt.singleton ((),[],do_trivial es) 
          

      let get_output = 
	fun et -> 
	  let (_,es) = et 0 in
	  List.map (fun (_,vcl,evts) -> (vcl,evts)) (Evt.elements es)
    end
