(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2019-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** External view of faults, which are part of final state *)
open Printf

module type I = sig
  type arch_global
  val pp_global : arch_global -> string
  val global_compare : arch_global -> arch_global -> int
  val same_id_fault : arch_global -> arch_global -> bool

  type fault_type
  val pp_fault_type : fault_type -> string
  val fault_type_compare : fault_type -> fault_type -> int
end

type ('loc, 'ftype) atom =
  (Proc.t * Label.t option) * 'loc option * 'ftype option

let pp_gen f pp_lbl pp_loc pp_ft (lbl,x,ft) =
  let x = Misc.pp_opt_arg pp_loc x
  and ft = Misc.pp_opt_arg pp_ft ft in
  sprintf "%s(%s%s%s)" f (pp_lbl lbl) x ft

let pp_fatom pp_loc =
  pp_gen "fault"
    (fun (p,lbl) -> match lbl with
    | None -> Proc.pp p
    | Some lbl -> sprintf "%s:%s" (Proc.pp p) (Label.pp lbl))
    pp_loc

let atom_compare v_compare ft_compare ((p1,lbl1),v1,ft1) ((p2,lbl2),v2,ft2) = match Proc.compare p1 p2 with
| 0 ->
    begin match Misc.opt_compare String.compare lbl1 lbl2 with
    | 0 ->
       begin match Misc.opt_compare v_compare v1 v2 with
       | 0 -> Misc.opt_compare ft_compare ft1 ft2
       | r -> r
       end
    | r -> r
    end
| r -> r

let map_value f (p,v,ft) = (p,Misc.map_opt f v,ft)

module type S = sig

  type loc_global
  type fault_type

  type fault =
    (Proc.t * Label.Set.t) * loc_global option *
    fault_type option * string option

  val pp_fault : fault -> string
  module FaultSet : MySet.S with type elt = fault

  type fatom = (loc_global,fault_type) atom

  val pp_fatom : fatom -> string
  val check_one_fatom : fault -> fatom -> bool
  val check_fatom : FaultSet.t -> fatom -> bool
  module FaultAtomSet : MySet.S with type elt = fatom
end

module Make(A:I) =
  struct

    type loc_global = A.arch_global
    type fault_type = A.fault_type
    type fault =
      (Proc.t * Label.Set.t) * loc_global option
      * fault_type option * string option

    let pp_lbl (p,lbl) = match Label.Set.as_small 1 lbl with
    | Some [] ->  Proc.pp p
    | Some [lbl] -> sprintf "%s:%s" (Proc.pp p) (Label.pp lbl)
    | _ ->
        sprintf "%s:{%s}" (Proc.pp p)
          (Label.Set.pp_str "," Label.pp lbl)


    let pp_fault (lbl,x,ftype,msg) =
      let x = Misc.pp_opt_arg A.pp_global x
      and ftype = Misc.pp_opt_arg A.pp_fault_type ftype
      and msg = Misc.pp_opt_arg Misc.identity msg in
      sprintf "Fault(%s%s%s%s)" (pp_lbl lbl) x ftype msg

    let compare_lbl (p1,lbl1) (p2,lbl2) = match Proc.compare p1 p2 with
    | 0 -> Label.Set.compare lbl1 lbl2
    | r -> r

    let compare (lbl1,x1,ftype1,msg1) (lbl2,x2,ftype2,msg2) =
      match compare_lbl lbl1 lbl2 with
      | 0 ->
         begin match Misc.opt_compare A.global_compare x1 x2 with
         | 0 ->
            begin match Misc.opt_compare A.fault_type_compare ftype1 ftype2 with
            | 0 -> Misc.opt_compare String.compare msg1 msg2
            | r -> r
            end
         | r -> r
         end
      | r -> r

    module FaultSet =
      MySet.Make
        (struct
          type t = fault
          let compare = compare
        end)

    type fatom = (loc_global,fault_type) atom

    let pp_fatom = pp_fatom A.pp_global A.pp_fault_type

    let check_one_fatom ((p0,lbls0),x0,ftype0,_)  ((p,lblo),x,ftype) =
      Proc.compare p p0 = 0 &&
      begin match lblo with
      | None -> true
      | Some lbl -> Label.Set.mem lbl lbls0
      end &&
      begin match ftype0, ftype with
      | _, None -> true
      | Some ft0, Some ft when ft0 = ft -> true
      | _, _ -> false
      end &&
      begin match x0,x with
      | _,None -> true
      | Some x0, Some x ->  A.same_id_fault x x0
      | None,_ -> false (* or assert false ? *)
      end

    let check_fatom flts a =
      FaultSet.exists
        (fun flt -> check_one_fatom flt a)
        flts

    module FaultAtomSet =
      MySet.Make
        (struct
          type t = fatom
          let compare ((p0,lbl0),x0,ft0)  ((p1,lbl1),x1,ft1) =
            match Proc.compare p0 p1 with
            | 0 ->
                begin match Misc.opt_compare Label.compare lbl0 lbl1 with
                | 0 ->
                   begin
                     match Misc.opt_compare A.global_compare x0 x1 with
                     | 0 -> Misc.opt_compare A.fault_type_compare ft0 ft1
                     | r -> r
                   end
                | r -> r
                end
            | r -> r
        end)
  end
