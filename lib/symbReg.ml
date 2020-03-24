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

open MiscParser
open Printf

module type S = sig
  type v
  type location
  type pseudo

  type ('loc,'v) t = ('loc,'v, pseudo) MiscParser.r3
      
  val allocate_regs :
    (MiscParser.location, MiscParser.maybev) t -> (location,v) t
end


module type Arch = sig
  include ArchBase.S

(* Values and global locations and their creators *)
  type v

  val maybevToV : MiscParser.maybev -> v
  type global
  val maybevToGlobal : MiscParser.maybev -> global

(* Manifest location type *)
  type location = 
    | Location_global of global
    | Location_deref of global * int
    | Location_reg of int * reg
end

module Make (A:Arch) : S 
with type v = A.v
and type location = A.location
and type pseudo = A.pseudo
 = struct

   type v = A.v
   type location = A.location
   type pseudo = A.pseudo
   type ('loc,'v) t = ('loc,'v, pseudo) MiscParser.r3
      
(******************************************************)
(* All those to substitute symbolic regs by real ones *)
(******************************************************)

  let get_reg name = match A.parse_reg name with
  | Some r -> r
  | None -> Warn.user_error "%s is not a register" name

  let finish_reg = get_reg

  let finish_location f_reg loc = match loc with
  | Location_global m -> A.Location_global (A.maybevToGlobal m)
  | Location_deref (m,i) -> A.Location_deref (A.maybevToGlobal m,i)
  | Location_reg (i,r) -> A.Location_reg (i,finish_reg r)
  | Location_sreg reg  ->
      let p,r = f_reg reg in A.Location_reg (p,r)

  let finish_state_atom f_reg (loc,(t,v)) =
    finish_location f_reg loc, (t,A.maybevToV v)

  let finish_state f_reg = List.map (finish_state_atom f_reg)

  let finish_locations f_reg =
    List.map (fun (loc,t) -> finish_location f_reg loc,t)

  let finish_atom f_reg a =
    let open ConstrGen in
    match a with
    | LV (loc,v) -> LV (finish_location f_reg loc, A.maybevToV v)
    | LL (l1,l2) -> LL (finish_location f_reg l1,finish_location f_reg l2)
    | FF (p,v) -> FF (p,A.maybevToV v)

   let finish_prop f_reg = ConstrGen.map_prop (finish_atom f_reg)

  let finish_filter f_reg = function
    | None -> None
    | Some p -> Some (finish_prop f_reg p)

  let finish_constr f_reg = ConstrGen.map_constr (finish_atom f_reg)

  let finish_pseudo f_reg =
    A.pseudo_map (A.map_regs (fun r -> r) f_reg)

  let finish_code f_reg = List.map (finish_pseudo f_reg)


(**********************************)	
(* All those to collect registers *)
(**********************************)	

  module ProcRegSet = 
    MySet.Make
      (struct
	type t = int * A.reg
	let compare (p1,r1) (p2,r2) = match compare p1 p2 with
	| 0 -> A.reg_compare r1 r2
	| r -> r
      end)

  module RegSet =
    MySet.Make
      (struct
	type t = A.reg
	let compare = A.reg_compare
      end)


  let collect_pseudo f =
    A.pseudo_fold
      (fun k ins -> A.fold_regs f k ins)

  let collect_prog =
    List.map
      (List.fold_left
	 (collect_pseudo  (RegSet.add,StringSet.add))
	 (RegSet.empty,StringSet.empty))
      

  let collect_location loc (regs,symbs as c) = match loc with
  | Location_reg (p,r) ->
      ProcRegSet.add (p,get_reg r) regs,symbs
  | Location_sreg reg ->
      regs,StringSet.add reg symbs
  | Location_global _|Location_deref _ -> c

  let collect_state_atom (loc,(_,(_:maybev))) = collect_location loc

  let collect_state = List.fold_right collect_state_atom

  let collect_atom a =
    let open ConstrGen in
    match a with
    | LV (loc,_) -> collect_location loc
    | LL (loc1,loc2) ->
        fun c -> collect_location loc1 (collect_location loc2 c)
    | FF _ -> Misc.identity

   let collect_prop = ConstrGen.fold_prop collect_atom

  let collect_filter = function
    | None -> Misc.identity
    | Some p -> collect_prop p

  let collect_constr = ConstrGen.fold_constr collect_atom

  let collect_locs = List.fold_right (fun (loc,_) -> collect_location loc)

(*********************************************)
(* Here we go: collect, allocate, substitute *)
(*********************************************)
  open MiscParser

(*
  let pp_reg_set chan rs =
    RegSet.pp chan "," (fun chan r -> fprintf chan "%s" (A.pp_reg r)) rs
  and pp_string_set chan s =
      StringSet.pp chan "," (fun chan r -> fprintf chan "%s" r) s
*)
  let allocate_regs test =
    let initial = test.init
    and prog = test.prog
    and final = test.condition
    and filter = test.filter
    and locs = test.locations in
    (* Collect all registers, either real or symbolic *)
    let regs,symbs =
      collect_filter test.filter
      (collect_constr final
         (collect_locs locs
	    (collect_state initial
	       (ProcRegSet.empty,StringSet.empty))))
    in

    let in_code = collect_prog (List.map snd prog) in
    (* Control register usage, ambiguity is possible,
       for unconstrained symbolic regs *)
    let (_,bad) =
      List.fold_left
	(fun (seen,bad) (_,symbs_p) ->
	  let symbs_p = StringSet.inter symbs_p symbs in
	  let bad =
	    StringSet.union
	      bad (StringSet.inter seen symbs_p) in
	  let seen = StringSet.union symbs_p seen in
	  seen,bad)
	(StringSet.empty,StringSet.empty)
	in_code in
    if not (StringSet.is_empty bad) then begin
      let msg =
	sprintf "ambiguous symbolic register(s): {%s}"
	  (String.concat ","
	     (StringSet.elements bad)) in
      Warn.user_error "%s" msg
    end ;
    (* Perform allocation of symbolic registers to real ones *)
    let envs =
      List.map2
	(fun ((p,_),_) (regs_p,symbs_p) ->
	  let regs_cstr =
	    ProcRegSet.fold
	      (fun (q,reg) k ->
		if p=q then RegSet.add reg k else k)
	      regs RegSet.empty in
	  let free_regs =
	    RegSet.diff (RegSet.of_list A.allowed_for_symb)
	      (RegSet.union regs_p regs_cstr) in
	  let env,_ =
	    StringSet.fold
	      (fun name (env,free_regs) -> match free_regs with
	      | [] ->
		  Warn.fatal
		    "not enough registers for all those symbolic registers"
	      | next::free_regs ->
		  (name,next)::env,free_regs)
	      symbs_p ([],RegSet.elements free_regs) in
	  p,env)
	prog in_code in
    (* Replace symbolic registers *)
    let prog =
      List.map2
	(fun (proc,code) (_,env) ->
	  let replace name =
	    try List.assoc name env
	    with Not_found -> assert false in
	  proc,finish_code replace code)
	prog envs in
    
    let env =
      List.fold_left
	(fun k (p,env_p) ->
	  List.fold_left
	    (fun k (symb,reg) -> (symb,(p,reg))::k)
	    k env_p)
	[] envs in
    let replace name =
      try List.assoc name env
      with Not_found ->
	Warn.user_error
	  "symbolic register %%%s does not appear in code" name in
    { test with
      init = finish_state replace initial ;
      prog = prog;
      filter = finish_filter replace filter;
      condition = finish_constr replace final;
      locations = finish_locations replace locs;
    }

end
