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

type ('prog,'nice_prog,'start,'state,'constr,'loc,'locset) t =
    {
     arch : Archs.t ; 
     name : Name.t ;
     info : MiscParser.info ;
     program : 'prog ;
     nice_prog : 'nice_prog ;
     start_points : 'start ;
     init_state : 'state ;
     cond : 'constr ;
     flocs : 'loc list ;
     observed : 'locset ;
     scope_tree : ScopeTree.scope_tree option ;
     mem_space_map : MemSpaceMap.mem_space_map ;
     param_map : CAst.param list list ;
   }

(* Name and nothing else *)
let simple_name test = test.name.Name.name

(* human-readable test name/filename combination *)
let readable_name test =  test.name.Name.name

(* and just the first part of that, for use in latex index *)
let very_readable_name test =  test.name.Name.name

(* Name from filename *)
let basename test = Filename.chop_extension (Filename.basename test.name.Name.file)

module Make(A:Arch.S) =
  struct

    type result =
        (A.program, A.nice_prog, A.start_points,
         A.state, A.constr, A.location, A.LocSet.t) t

(* Symb register allocation is external, since litmus needs it *)
    module ArchAlloc = struct
      include A

      (* Here values and global (addresses) are identical,
         NB: this is not the case for litmus! *)
      type v = A.V.v
      let maybevToV = V.maybevToV
      type global = A.V.v
      let maybevToGlobal = V.maybevToV 
    end
        
   module Alloc = SymbReg.Make(ArchAlloc)
(* Code loader is external, since litmus tests need it too *)
    module Load = Loader.Make(A) 

    let build name t =
      let t = Alloc.allocate_regs t in
      let
          {MiscParser.init = init ;
           info = info ;
           prog = nice_prog ;
           condition = final ; 
           locations = locs ;
           gpu_data = gpu_data ;
	 } = t in

      let prog,starts = Load.load nice_prog in
      let flocs = List.map fst locs in
      let observed =
        let locs = A.LocSet.of_list flocs in
        ConstrGen.fold_constr
          (fun a r -> match a with
          | ConstrGen.LV (loc,_v) -> A.LocSet.add loc r
          | ConstrGen.LL (l1,l2) -> A.LocSet.add l1 (A.LocSet.add l2 r))
          final locs in
(* Hum, half satisfactory,  but steems from the test structure having
   three fields that are gpu-specific *)
      let scope_tree, mem_space_map,param_map = match gpu_data with
      | None -> None,[],[]
      | Some { MiscParser.scope_tree; mem_space_map; param_map; } ->
          scope_tree, mem_space_map, param_map in
      {
       arch = A.arch ;
       name = name ;
       info = info ;
       program = prog ;
       nice_prog = nice_prog ;
       start_points = starts ;
       init_state = A.build_state init ;
       cond = final ;
       flocs = flocs ;
       observed = observed ;       
       scope_tree = scope_tree ;
       mem_space_map = mem_space_map ;
       param_map = param_map ;
     }


    let find_our_constraint test = test.cond 

end
