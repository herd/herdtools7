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

open Printf

module Make(O:MixOption.S)(A:Arch_tools.S) : sig

  val alpha : Name.t -> A.test (* avoid *) -> A.test (* alpha *) -> A.test
  val global : Name.t -> A.test (* avoid *) -> A.test (* alpha *) -> A.test
end =
struct

  open MiscParser

(****************)
(* Various sets *)
(****************)

  module OR = struct
    type t = A.reg
    let compare = A.reg_compare
  end

  let pp_rs rs = sprintf "{%s}" (A.RegSet.pp_str "," A.pp_reg rs)


  module RegMap = MyMap.Make(OR)

  module OPR = struct
    type t = int * A.reg
    let compare (p1,r1) (p2,r2) = match Misc.int_compare p1 p2 with
    | 0 -> A.reg_compare r1 r2
    | r -> r
  end

  module ProcRegSet = MySet.Make(OPR)
  module ProcRegMap = MyMap.Make(OPR)


(********************************)
(* Find all (integer) registers *)
(********************************)

  module Collect =  CollectRegs.Make(A)

  let collect_location loc regs = match loc with
  | A.Location_reg (p,r) -> ProcRegSet.add (p,r) regs
  | A.Location_global _|A.Location_deref _ -> regs

  let collect_state_atom (loc,_) = collect_location loc

  let collect_atom a regs =
    let open ConstrGen in
    match a with
    | LV (loc,_) -> collect_location loc regs
    | LL (loc1,loc2) ->  collect_location loc1 (collect_location loc2 regs)

  let collect_state st = List.fold_right collect_state_atom st

  let collect_constr = ConstrGen.fold_constr collect_atom

  let collect_locs = List.fold_right (fun (loc,_) -> collect_location loc)


(***************************)
(* Alpha conversion proper *)
(***************************)


(* group according to proc *)
  let by_proc rs =
    ProcRegSet.fold
      (fun (p,r) m ->
        let rs = A.ProcMap.safe_find A.RegSet.empty p m in
        A.ProcMap.add p (A.RegSet.add r rs) m)
      rs A.ProcMap.empty

(* Lift reg -> reg map into a procreg -> procreg map *)
  let add_reg_map p m =
    RegMap.fold
      (fun r1 r2 -> ProcRegMap.add (p,r1) (A.Location_reg (p,r2)))
      m
(* alpha convert decorations *)

  let alpha_location f = function
    | A.Location_reg (p,r) -> f (p,r)
    | A.Location_global _|A.Location_deref _ as loc -> loc

  let alpha_atom f a =
    let open ConstrGen in
    match a with
    | LV (loc,v) -> LV (alpha_location f loc,v)
    | LL (loc1,loc2) -> LL (alpha_location f loc1,alpha_location f loc2)

  let alpha_state_atom f (loc,x) = alpha_location f loc,x

  let alpha_state f = List.map (alpha_state_atom f)

  let alpha_locations f = alpha_state f (* Oups *)

  let alpha_constr f = ConstrGen.map_constr (alpha_atom f)

(* alpha convert code, also returns the conversion map. *)
  let alpha_code ars rs ac c =
    (* Registers to avoid *)
    let ars = A.RegSet.union (Collect.collect_code ac) ars in
    (* Registers to convert *)
    let rs = A.RegSet.union (Collect.collect_code c) rs in
    let free = A.RegSet.diff (A.RegSet.of_list A.allowed_for_symb) ars in
    if O.verbose > 0 then begin
      eprintf "Free=%s\n" (pp_rs free)
    end ;
    let free = A.RegSet.elements free in
    let env,_ =
      A.RegSet.fold
        (fun r (env,free) -> match free with
        | [] -> Warn.user_error "not enough registers for alpha conversion"
        | f::free ->
            RegMap.add r f env,free)
        rs (RegMap.empty,free) in
    let c =
      List.map
        (A.pseudo_map
           (A.map_regs
              (fun r -> try RegMap.find r env with Not_found -> assert false)
              (fun _sr -> assert false)))
        c in
    c,env

  let rec alpha_prog ma mt apcs pcs = match apcs,pcs with
  | _,[] -> [],ProcRegMap.empty
  | [],(p,c)::pcs ->
      let c,cm =
        alpha_code
          (A.ProcMap.safe_find A.RegSet.empty p ma)
          (A.ProcMap.safe_find A.RegSet.empty p mt)
          [] c in
      let pcs,m = alpha_prog ma mt [] pcs in
      (p,c)::pcs,add_reg_map p cm m
  | (_,ac)::apcs,(p,c)::pcs ->
      let c,cm =
        alpha_code
          (A.ProcMap.safe_find A.RegSet.empty p ma)
          (A.ProcMap.safe_find A.RegSet.empty p mt)
          ac c in
      let pcs,m = alpha_prog ma mt apcs pcs in
      (p,c)::pcs,add_reg_map p cm m


(*********************************)
(* Again for global locations... *)
(*********************************)

  module Addr = struct
    open Constant

    let nolabel_value () = Warn.user_error "No label value for %s" Sys.argv.(0)
    let notag_value () = Warn.user_error "No tag value for %s" Sys.argv.(0)

    let collect_value f v k = match v with
    | Symbolic ((s,_),_) -> f s k
    | Concrete _ -> k
    | Label _ -> nolabel_value ()
    | Tag _ -> notag_value ()

    let map_value f v = match v with
    | Symbolic ((s,t),o) -> Symbolic ((f s,t),o)
    | Concrete _ -> v
    | Label _ -> nolabel_value ()
    | Tag _ -> notag_value ()

    let collect_pseudo f =
      A.pseudo_fold
        (fun k ins -> A.fold_addrs (collect_value f) k ins)

    let map_pseudo f =
      A.pseudo_map (fun ins -> A.map_addrs (map_value f) ins)

    let collect_code f c k =
      List.fold_left (collect_pseudo f) k c

    let map_code f = List.map (map_pseudo f)

    let collect_prog f = List.fold_right (fun (_,c) -> collect_code f c)

    let map_prog f = List.map  (fun (p,c) -> p,map_code f c)

    let collect_location f loc k = match loc with
    | A.Location_reg _ -> k
    | A.Location_global v|A.Location_deref (v,_) -> collect_value f v k

    let map_location f loc = match loc with
    | A.Location_reg _ -> loc
    | A.Location_global v -> A.Location_global (map_value f v)
    | A.Location_deref (v,idx) -> A.Location_deref (map_value f v,idx)

    let collect_state_atom f (loc,(_,v)) k =
      collect_location f loc (collect_value f v k)

    let collect_atom f a k =
      let open ConstrGen in
      match a with
      | LV (loc,v) ->
          collect_location f loc (collect_value f v k)
      | LL (loc1,loc2) ->
          collect_location f loc1 (collect_location f loc2 k)

    let map_state_atom f (loc,(t,v)) = map_location f loc,(t,map_value f v)

    let map_atom f a =
      let open ConstrGen in
      match a with
      | LV (loc,v) ->
          LV (map_location f loc,map_value f v)
      | LL (loc1,loc2) ->
          LL (map_location f loc1,map_location f loc2)

    let collect_state f = List.fold_right (collect_state_atom f)

    let map_state f = List.map (map_state_atom f)

    let collect_constr f = ConstrGen.fold_constr (collect_atom f)

    let map_constr f = ConstrGen.map_constr (map_atom f)

    let collect_locs f =
      List.fold_right (fun (loc,_) -> collect_location f loc)

    let map_locs f =
      List.map (fun (loc,t) -> map_location f loc,t)

    module StringSet = MySet.Make(String)

    let collect_test t =
      let f = StringSet.add in
      let k = StringSet.empty in
      let k = collect_state f t.init k in
      let k = collect_locs f t.locations k in
      let k = collect_constr f t.condition k in
      collect_prog f t.prog k

    let all_addrs =
      StringSet.of_list
        ["x";"y";"z";"t";
         "a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";
         "o";"p";"q";"r";"s";"u";"v";"w";]

    module StringMap = MyMap.Make(String)

    let alpha_test env t =
      let f s =
        try StringMap.find s env
        with Not_found -> assert false in
      { t with
        init = map_state f t.init ;
        locations = map_locs f t.locations ;
        condition = map_constr f t.condition ;
        prog = map_prog f t.prog ; }

    let alpha avoid test =
      let ass = collect_test avoid
      and tss = collect_test test in
      let free = StringSet.diff all_addrs ass in
      let free = StringSet.elements free in
      let env,_ =
        StringSet.fold
          (fun s (env,free) -> match free with
          | [] -> Warn.user_error "not enough names for alpha conversion"
          | f::free ->
              StringMap.add s f env,free)
          tss (StringMap.empty,free) in
      alpha_test env test

  end
  module T = TestHash.Make(A)

  let collect_decorations t =
    collect_constr t.condition
      (collect_locs t.locations (collect_state t.init ProcRegSet.empty))

  let alpha doc avoid test =
    (* Collect all integer registers from program decoration *)
    let apregs = collect_decorations avoid
    and pregs  = collect_decorations test in

    let prog,env =
      alpha_prog
        (by_proc apregs) (by_proc pregs) avoid.prog test.prog in
    let alpha_preg r =
      try ProcRegMap.find r env with Not_found -> assert false in
    let initial = alpha_state alpha_preg test.init
    and final = alpha_constr alpha_preg test.condition
    and locs = alpha_locations alpha_preg test.locations in
    let prog = T.refresh_labels "X" prog in
    let r =
      { test with
        prog = prog; init = initial;
        condition = final; locations = locs; } in
    let r = Addr.alpha avoid r in
    if O.verbose > 0 then begin
      let module D = Dumper.Make(A) in
      D.dump stderr doc r
    end ;
    r

  let global doc avoid test =
    let r = Addr.alpha avoid test in
    if O.verbose > 0 then begin
      let module D = Dumper.Make(A) in
      D.dump stderr doc r
    end ;
    r

end
