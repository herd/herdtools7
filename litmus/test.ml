(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type Cfg = sig
  val hexa : bool
end

module type S = sig
  module A : Arch.Base
  module C : Constr.S with module A = A
  module P : PseudoAbstract.S

  type src =
    ((A.location * (MiscParser.run_type * Constant.v)) list, P.code list,
          C.constr, A.location)
         MiscParser.result

  type 'a type_env = ('a * CType.t) list
  type env_volatile = string list

  type t =
    { init : A.state ;
      info : MiscParser.info ;
      code : (int * (A.Out.t * (A.reg type_env * env_volatile))) list ;
      condition : C.constr ;
      globals : string type_env ;
      flocs : A.location list ;
      global_code : string list;
      src : src ; }

  val find_our_constraint : t -> C.constr
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
end



module Make(Cfg:Cfg)(A:Arch.Base)(P:PseudoAbstract.S) : S
with module A = A and module P = P =
struct
  module A  = A
  module C = Constr.Make(A)
  module P = P

  type 'a type_env = ('a * CType.t) list
  type src =
    ((A.location * (MiscParser.run_type * Constant.v)) list, P.code list,
          C.constr, A.location)
         MiscParser.result

  type env_volatile = string list

  type t =
    { init : A.state ;
      info : MiscParser.info ;
      code : (int * (A.Out.t * (A.reg type_env * env_volatile))) list ;
      condition : C.constr ;
      globals : string type_env ;
      flocs : A.location list ;
      global_code : string list;
      src : src ; }

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
end
