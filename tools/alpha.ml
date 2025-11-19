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
  | A.Location_global _ -> regs

  let collect_rloc = ConstrGen.fold_rloc collect_location

  let collect_state_atom (loc,_) = collect_location loc

  let collect_atom a regs =
    let open ConstrGen in
    match a with
    | LV (rloc,_) -> collect_rloc rloc regs
    | LL (loc1,loc2) ->  collect_location loc1 (collect_location loc2 regs)
    | FF (_,None,_) -> regs
    | FF (_,Some x,_) -> collect_location (A.Location_global x) regs

  let collect_state st = List.fold_right collect_state_atom st

  let collect_constr = ConstrGen.fold_constr collect_atom

  let collect_locs =
    let open LocationsItem in
    List.fold_right
      (fun i -> match i with
      | Loc (l,_) -> ConstrGen.fold_rloc collect_location l
      | Fault (_,None,_) -> Misc.identity
      | Fault (_,Some x,_) -> collect_location (A.Location_global x))

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
    | A.Location_global _ as loc -> loc

  let alpha_rloc f = ConstrGen.map_rloc (alpha_location f)

  let alpha_atom f a =
    let open ConstrGen in
    match a with
    | LV (rloc,v) -> LV (alpha_rloc f rloc,v)
    | LL (loc1,loc2) -> LL (alpha_location f loc1,alpha_location f loc2)
    | FF (_,None,_) -> a
    | FF (_,Some x,_) -> ignore (Constant.check_sym x) ; a

  let alpha_state_atom f (loc,x) = alpha_location f loc,x

  let alpha_state f = List.map (alpha_state_atom f)

  let alpha_locations f =
    let open LocationsItem in
    List.map
      (function
        | Loc (x,t) -> Loc (alpha_rloc f x,t)
        | Fault (_,None,_) as a -> a
        | Fault (_,Some x,_) as a -> ignore (Constant.check_sym x); a)

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
  | [],((p,_,_) as id,c)::pcs ->
      let c,cm =
        alpha_code
          (A.ProcMap.safe_find A.RegSet.empty p ma)
          (A.ProcMap.safe_find A.RegSet.empty p mt)
          [] c in
      let pcs,m = alpha_prog ma mt [] pcs in
      (id,c)::pcs,add_reg_map p cm m
  | (_,ac)::apcs,((p,_,_) as id,c)::pcs ->
      let c,cm =
        alpha_code
          (A.ProcMap.safe_find A.RegSet.empty p ma)
          (A.ProcMap.safe_find A.RegSet.empty p mt)
          ac c in
      let pcs,m = alpha_prog ma mt apcs pcs in
      (id,c)::pcs,add_reg_map p cm m


(*********************************)
(* Again for global locations... *)
(*********************************)

  module Addr = struct
    open Constant

    let nolabel_value () = Warn.user_error "No label value for %s" Sys.argv.(0)
    let notag_value () = Warn.user_error "No tag value for %s" Sys.argv.(0)
    let nopte_value () = Warn.user_error "No pteval_t value for %s" Sys.argv.(0)
    let noaddrreg_value () = Warn.user_error "No parel1_t value for %s" Sys.argv.(0)
    let noinstr_value () = Warn.user_error "No instruction value for %s" Sys.argv.(0)

    let rec collect_value f v k = match v with
    | Symbolic (Virtual {name=Symbol.Label _; _}) -> nolabel_value ()
    | Symbolic (Virtual {name=s;_}) -> f (Symbol.pp s) k
    | Symbolic (System ((PTE|PTE2),s)) -> f s k
    | Concrete _ -> k
    | ConcreteVector vs ->
       List.fold_left (fun k v -> collect_value f v k) k vs
    | ConcreteRecord vs ->
      StringMap.fold_values (collect_value f) vs k
    | Tag _ -> notag_value ()
    | PteVal _ -> nopte_value ()
    | AddrReg _ -> noaddrreg_value ()
    | Instruction _ -> noinstr_value ()
    | Symbolic (Physical _|TagAddr _|System (TLB,_))
    | Frozen _
      -> assert false


    let rec map_value f v = match v with
    | Symbolic (Virtual {name=Symbol.Label _; _}) -> nolabel_value ()
    | Symbolic (Virtual sym) -> Symbolic (Virtual {sym with name=Symbol.map f sym.name; })
    | Symbolic (System (PTE,s)) ->  Symbolic (System (PTE,f s))
    | Symbolic (System (PTE2,s)) ->  Symbolic (System (PTE2,f s))
    | Concrete _ -> v
    | ConcreteVector vs ->
       ConcreteVector (List.map (map_value f) vs)
    | ConcreteRecord vs ->
       ConcreteRecord (StringMap.map (map_value f) vs)
    | Tag _ -> notag_value ()
    | PteVal _ -> nopte_value ()
    | AddrReg _ -> noaddrreg_value ()
    | Instruction _ -> noinstr_value ()
    | Frozen _|Symbolic (Physical _|TagAddr _|System (TLB,_))
      -> assert false


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
    | A.Location_global v -> collect_value f v k

    let collect_rloc f  = ConstrGen.fold_rloc (collect_location f)

    let map_location f loc = match loc with
    | A.Location_reg _ -> loc
    | A.Location_global v ->
       let w = map_value f v in
       A.Location_global w

    let my_map_rloc f = ConstrGen.map_rloc (map_location f)

    let collect_state_atom f (loc,(_,v)) k =
      collect_location f loc (collect_value f v k)

    let collect_atom f a k =
      let open ConstrGen in
      match a with
      | LV (rloc,v) ->
          collect_rloc f rloc (collect_value f v k)
      | LL (loc1,loc2) ->
          collect_location f loc1 (collect_location f loc2 k)
      | FF (_,None,_) -> k
      | FF (_,Some x,_) ->
          collect_location f (A.Location_global x) k

    let map_state_atom f (loc,(t,v)) = map_location f loc,(t,map_value f v)

    let map_global f x = match map_location f (A.Location_global x) with
    | A.Location_global x -> x
    | _ -> assert false

    let map_atom f a =
      let open ConstrGen in
      match a with
      | LV (rloc,v) ->
          LV (my_map_rloc f rloc,map_value f v)
      | LL (loc1,loc2) ->
          LL (map_location f loc1,map_location f loc2)
      | FF(p,x,ft) -> FF (p,Misc.map_opt (map_global f) x,ft)

    let collect_state f = List.fold_right (collect_state_atom f)

    let map_state f = List.map (map_state_atom f)

    let collect_constr f = ConstrGen.fold_constr (collect_atom f)

    let map_constr f = ConstrGen.map_constr (map_atom f)

    let collect_locs f = LocationsItem.fold_locs (collect_location f)

    let map_locs f =
      let open LocationsItem in
      List.map
        (function
          | Loc (x,t) -> Loc (my_map_rloc f x,t)
          | Fault (p,x,ft)-> Fault (p,Misc.map_opt (map_global f) x,ft))

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
      if O.verbose > 0 then begin
        let pp_set = StringSet.pp_str "," Misc.identity in
        eprintf
          "avoid={%s}, source={%s}\n" (pp_set ass) (pp_set tss)
      end ;
      let free = StringSet.diff all_addrs ass in
      let free = StringSet.elements free in
      let env,_ =
        StringSet.fold
          (fun s (env,free) -> match free with
          | [] -> Warn.user_error "not enough names for alpha conversion"
          | f::free ->
              StringMap.add s f env,free)
          tss (StringMap.empty,free) in
      if O.verbose > 0 then begin
          let pp_map =
            StringMap.pp_str (fun x y -> sprintf "%s -> %s" x y) in
          eprintf "env={%s}\n" (pp_map env)
      end ;
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
