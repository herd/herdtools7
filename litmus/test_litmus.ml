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

module type Cfg = sig
  val hexa : bool
end

module type S = sig
  type instruction
  module A : Arch_litmus.Base with type instruction = instruction
  module C : Constr.S with
           module V = A.V and
           type location = A.location and module LocSet = A.LocSet and
           module RLocSet = A.RLocSet and module FaultType = A.FaultType
  module P : PseudoAbstract.S with type ins = A.instruction
  module FaultType : FaultType.S

  type src =
    ((A.location * (TestType.t * A.V.v)) list, P.code list,
          C.prop, A.location, A.V.v, FaultType.t)
         MiscParser.result

  type 'a type_env = ('a * CType.t) list
  type env_volatile = string list

  type t =
    { init : A.state ;
      info : MiscParser.info ;
      code : (Proc.t * (A.Out.t * (A.reg type_env * env_volatile))) list ;
      condition : C.cond ;
      filter : C.prop option ;
      globals : string type_env ;
      flocs : A.location ConstrGen.rloc list ;
      ffaults : (A.V.v,FaultType.t) Fault.atom list;
      global_code : string list;
      src : src ;
      type_env : CType.t A.LocMap.t * CType.t StringMap.t ;
      bellinfo : BellInfo.test option ; }

  val find_our_constraint : t -> C.cond
  val get_nprocs : t -> int
  val has_asmhandler : t -> bool
  val has_defaulthandler : t -> bool
  val partition_asmhandlers : t -> int list * int list

  module D : CoreDumper.S
    with
      type test =  (A.fullstate, P.code list, C.prop, A.location, A.V.v, FaultType.t)  MiscParser.result

  val find_offset : P.code list -> Proc.t -> string -> int
  val code_exists : (P.ins -> bool) -> t -> bool
  val get_exported_labels_init_code :
      A.state -> P.code list ->  Label.Full.Set.t
  val get_exported_labels : t -> Label.Full.Set.t
  val from_labels : t -> (Label.Full.full * P.ins) list
end



module Make(Cfg:Cfg)(A:Arch_litmus.Base)
(P:PseudoAbstract.S with type ins = A.instruction) : S
with type instruction = A.instruction
and module A = A
and module P = P
and module FaultType = A.FaultType =
struct
  type instruction = A.instruction
  module A  = A
  module C = Constr.Make(A)
  module P = P
  module FaultType = A.FaultType

  type 'a type_env = ('a * CType.t) list
  type src =
    ((A.location * (TestType.t * A.V.v)) list, P.code list,
          C.prop, A.location,A.V.v,A.FaultType.t)
         MiscParser.result

  type env_volatile = string list

  type t =
    { init : A.state ;
      info : MiscParser.info ;
      code : (Proc.t * (A.Out.t * (A.reg type_env * env_volatile))) list ;
      condition : C.cond ;
      filter : C.prop option ;
      globals : string type_env ; (* Virtual addresses only *)
      flocs : A.location ConstrGen.rloc list ;
      ffaults : (A.V.v, A.FaultType.t) Fault.atom list;
      global_code : string list;
      src : src ;
      type_env : CType.t A.LocMap.t * CType.t StringMap.t ;
      bellinfo : BellInfo.test option ; }

  let find_our_constraint test = test.condition

  let get_nprocs t = List.length t.code

  let has_asmhandler t =
    List.exists
      (fun (_,(c,_)) -> A.Out.has_asmhandler c)
      t.code

  let has_defaulthandler t =
    List.exists
      (fun (_,(c,_)) -> not (A.Out.has_asmhandler c))
      t.code

  let partition_asmhandlers t =
    let _,ok,no =
      List.fold_left
        (fun (i,ok,no) (_,(c,_)) ->
          if A.Out.has_asmhandler c then
            i+1,i::ok,no
          else
            i+1,ok,i::no)
        (0,[],[]) t.code in
    ok,no

  module D =
    struct
      include
        CoreDumper.Make
          (struct

            let arch = A.arch

            type prog = P.code list
            let print_prog = P.print_prog
            let dump_prog_lines = P.dump_prog_lines

            type v = A.V.v
            let dump_v = A.V.pp Cfg.hexa

            let dump_state_atom =
              MiscParser.dump_state_atom A.is_global A.pp_location dump_v

            type state = A.fullstate

            let dump_state st =
              DumpUtils.dump_state
                dump_state_atom
                (A.env_for_pp st)

            type prop = C.prop

            let dump_atom a =
              ConstrGen.dump_atom A.pp_location A.pp_location_brk dump_v A.FaultType.pp a

            let dump_prop = ConstrGen.prop_to_string dump_atom
            let dump_constr = ConstrGen.constraints_to_string dump_atom

            type location = A.location
            let dump_location loc = A.pp_location loc

            type fault_type = A.FaultType.t
            let dump_fault_type = A.FaultType.pp
          end)
    end

  let find_offset = P.find_offset

  let code_exists p t =
    let src = t.src in
    let code = src.MiscParser.prog in
    List.exists (P.code_exists p) code

  let get_exported_labels_init =
    let open Constant in
    List.fold_left
      (fun k (_,v) ->
        match v with
        | Label (p,lbl) -> Label.Full.Set.add (p,lbl) k
        | _ -> k)
      Label.Full.Set.empty

  let get_exported_labels_init_code init prog =
    Label.Full.Set.union
      (P.exported_labels_code prog)
      (get_exported_labels_init init)

  let get_exported_labels  { init; src; _ } =
     get_exported_labels_init_code  init src.MiscParser.prog

  let from_labels { init; src; _ } =
    let lbls = get_exported_labels_init_code init src.MiscParser.prog in
    P.from_labels lbls src.MiscParser.prog

end
