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

type ('prog,'nice_prog,'start,'state,'size_env, 'prop,'loc,'locset) t =
    {
     arch : Archs.t ;
     name : Name.t ;
     info : MiscParser.info ;
     program : 'prog ;
     nice_prog : 'nice_prog ;
     start_points : 'start ;
     init_state : 'state ;
     size_env : 'size_env ;
     filter : 'prop option ;
     cond : 'prop ConstrGen.constr ;
     flocs : 'loc list ;
     observed : 'locset ;
     displayed : 'locset ;
     extra_data : MiscParser.extra_data ;
     access_size : MachSize.sz list ;
   }

(* Name and nothing else *)
let simple_name test = test.name.Name.name

(* human-readable test name/filename combination *)
let readable_name test =  test.name.Name.name

(* and just the first part of that, for use in latex index *)
let very_readable_name test =  test.name.Name.name

(* Name from filename *)
let basename test =
  let base = Filename.basename test.name.Name.file in
  try Filename.chop_extension base
  with Invalid_argument _ -> base

module Make(A:Arch_herd.S) =
  struct

    type result =
        (A.program, A.nice_prog, A.start_points,
         A.state, A.size_env, A.prop, A.location, A.LocSet.t) t

(* Symb register allocation is external, since litmus needs it *)
    module ArchAlloc = struct
      include A

      (* Here values and global (addresses) are identical,
         NB: this is not the case for litmus! *)
      let maybevToV = V.maybevToV
      type global = A.V.v
      let maybevToGlobal = V.maybevToV
    end

   module Alloc = SymbReg.Make(ArchAlloc)

(* Code loader is external, since litmus tests need it too *)
    module Load = Loader.Make(A)

    let collect_atom a r = match a with
    | ConstrGen.LV (loc,_v) -> A.LocSet.add loc r
    | ConstrGen.LL (l1,l2) -> A.LocSet.add l1 (A.LocSet.add l2 r)
    | ConstrGen.FF _ ->  r

(* Mem size access *)
    let mem_access_size_of_code sz code =
      List.fold_left
        (A.pseudo_fold
           (fun sz0 ins -> match A.mem_access_size ins with
           | Some sz -> MachSize.Set.add sz sz0
           | None -> sz0))
        sz
        code

    let mem_access_size_prog p =
      let s =
        List.fold_left
          (fun sz (_,code) -> mem_access_size_of_code sz code)
          MachSize.Set.empty p in
      MachSize.Set.elements s

    let build name t =
      let t = Alloc.allocate_regs t in
      let
          {MiscParser.init = init ;
           info = info ;
           prog = nice_prog ;
           filter = filter ;
           condition = final ;
           locations = locs ;
           extra_data = extra_data ;
         } = t in

      let prog,starts = Load.load nice_prog in
      let flocs = List.map fst locs in
      let displayed =
        let flocs = A.LocSet.of_list flocs in
        ConstrGen.fold_constr collect_atom final flocs in
      let observed = match filter with
      | None -> displayed
      | Some filter ->
          ConstrGen.fold_prop collect_atom filter displayed in
      {
       arch = A.arch ;
       name = name ;
       info = info ;
       program = prog ;
       nice_prog = nice_prog ;
       start_points = starts ;
       init_state = A.build_state init ;
       filter = filter ;
       cond = final ;
       flocs = flocs ;
       observed = observed ;
       displayed = displayed ;
       extra_data = extra_data ;
       size_env = A.build_size_env init ;
       access_size =
       begin
         if A.is_mixed then mem_access_size_prog nice_prog
         else []
       end ;
     }

    let empty_test =
      let empty_name =
        {
          Name.name = "";
          Name.file = "";
          Name.texname = "";
          Name.doc = "";
        }
      in
    let fake_constr =
      ConstrGen.ExistsState (ConstrGen.And [])
      in
      {
       arch = A.arch ;
       name = empty_name ;
       info = [] ;
       program = A.LabelMap.empty ;
       nice_prog = [] ;
       start_points = [] ;
       init_state = A.state_empty; size_env = A.size_env_empty ;
       filter = None ;
       cond = fake_constr ;
       flocs = [] ;
       observed = A.LocSet.empty; displayed = A.LocSet.empty;
       extra_data = MiscParser.empty_extra;
       access_size = [];
      }

    let find_our_constraint test = test.cond

end
