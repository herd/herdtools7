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
  module A : Arch_litmus.Base
  module C : Constr.S with
  module V = A.V and
  type location = A.location and module LocSet = A.LocSet
  module P : PseudoAbstract.S

  type src =
    ((A.location * (MiscParser.run_type * A.V.v)) list, P.code list,
          C.prop, A.location)
         MiscParser.result

  type 'a type_env = ('a * CType.t) list
  type env_volatile = string list

  type t =
    { init : A.state ;
      info : MiscParser.info ;
      code : (int * (A.Out.t * (A.reg type_env * env_volatile))) list ;
      condition : C.cond ;
      filter : C.prop option ;
      globals : string type_env ;
      flocs : A.location list ;
      global_code : string list;
      src : src ;
      type_env : CType.t A.LocMap.t * CType.t StringMap.t ;  }

  val find_our_constraint : t -> C.cond
  val get_nprocs : t -> int

  module D :
  module type of
    TestDump.Make
      (struct
        let hexa = false
        module A=A
        module C=C
        module P=P
      end)

  val find_offset : P.code list -> int -> string -> int
end



module Make(Cfg:Cfg)(A:Arch_litmus.Base)(P:PseudoAbstract.S) : S
with module A = A and module P = P =
struct
  module A  = A
  module C = Constr.Make(A)
  module P = P

  type 'a type_env = ('a * CType.t) list
  type src =
    ((A.location * (MiscParser.run_type * A.V.v)) list, P.code list,
          C.prop, A.location)
         MiscParser.result

  type env_volatile = string list

  type t =
    { init : A.state ;
      info : MiscParser.info ;
      code : (int * (A.Out.t * (A.reg type_env * env_volatile))) list ;
      condition : C.cond ;
      filter : C.prop option ;
      globals : string type_env ; (* Virtual addresses only *)
      flocs : A.location list ;
      global_code : string list;
      src : src ;
      type_env : CType.t A.LocMap.t * CType.t StringMap.t ; }

  let find_our_constraint test = test.condition

  let get_nprocs t = List.length t.code

  module D =
    TestDump.Make
      (struct
        let hexa = Cfg.hexa
        module A = A
        module C = C
        module P = P
      end)
  let find_offset code p lbl = P.find_offset code p lbl
end
