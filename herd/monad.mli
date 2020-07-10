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

(** A monad for event structures *)

(* Define a monad, which is a composition of event set state and a single variable state
   (to pick new eiids *)

module type S =
  sig
    module A : Arch_herd.S

    module E : Event.S
    with module Act.A = A

    module VC    : Valconstraint.S
    with type atom = A.V.v
    and type cst = A.V.Cst.v
    and type solution = A.V.solution
    and type location = A.location
    and type state = A.state

    type 'a t

    type 'a code
    val zeroT        : 'a t
    val zerocodeT        : 'a code
    val unitT        : 'a -> 'a t
    val unitcodeT        : 'a -> 'a code
    val delay : 'a t -> ('a * 'a t) t
    val (>>=) : 'a t -> ('a -> 'b t) -> ('b) t
    val (>>==) : 'a t -> ('a -> 'b t) -> ('b) t (* Output event stay in first arg *)
    val (>>*=) : 'a t -> ('a -> 'b t) -> ('b) t
    val bind_ctrl_avoid : 'c t -> 'a t -> ('a -> 'b t) -> 'b t
    val check_tags : 'v t -> ('v -> 'v t) -> ('v -> 'v t) -> 'x t -> 'v t
    val exch : 'a t -> 'a t -> ('a -> 'b t) ->  ('a -> 'c t) ->  ('b * 'c) t

(*
  Those amo_strict and swp are the AArch64 combinators,
  with complete internal dependencies.

  In particular there is a iico_ctrl dependency
  from read_register to write_register.
  The dependency from read_mem to write_mem is iico_ctrl for swp
  and iico_data for amo_strict.
*)
    val swp : ('loc t) ->
      ('loc -> A.V.v t) -> A.V.v t (* read reg *) ->
        ('loc -> A.V.v -> unit t) ->  (A.V.v -> unit t) (* Write reg *)
          -> unit t

    val amo_strict : Op.op -> ('loc t) ->
      ('loc -> A.V.v t) -> A.V.v t (* read reg *) ->
        ('loc -> A.V.v -> unit t) ->  (A.V.v -> unit t) (* Write reg *)
          -> unit t


    val linux_exch :
        'loc t -> 'v t -> ('loc -> 'w t) -> ('loc -> 'v -> unit t) -> 'w t

(* Weak amo, without control dependency from read read to write reg.
  In fact, the write reg is absent *)
    val amo : Op.op ->
      'loc t -> A.V.v t -> ('loc -> A.V.v t) -> ('loc -> A.V.v -> unit t) -> A.V.v t

    val linux_cmpexch_ok :
        'loc t -> 'v t -> 'v t -> ('loc -> 'v t) ->
          ('loc -> 'v -> unit t) -> ('v -> 'v -> unit t) -> 'v t

    val linux_cmpexch_no :
        'loc t -> 'v t -> ('loc -> 'v t) ->
          ('v -> 'v -> unit t) -> 'v t
    val linux_add_unless_ok :
        'loc t -> 'v t -> 'v t ->
          ('loc -> 'v t) -> ('loc -> 'v -> unit t) ->
            ('v -> 'v -> unit t) -> ('v -> 'v -> 'v t) -> 'v option -> 'v t
    val linux_add_unless_no :
        'loc t -> 'v t -> ('loc -> 'v t) -> ('v -> 'v -> unit t) -> 'v option -> 'v t

    val riscv_store_conditional :
        A.V.v t -> A.V.v t -> A.V.v t -> (* read reserve, data, address *)
          (unit t) -> (* write reserve *)
            (A.V.v -> unit t) -> (* write result *)
              (A.V.v -> A.V.v -> A.V.v -> unit t) -> (* write mem *)
                unit t

    val aarch64_cas_ok :
        'loc t -> 'v t -> 'v t ->
          ('v -> unit t) -> ('loc -> 'v t) -> ('loc -> 'v -> unit t) ->
            ('v -> 'v -> unit t) -> unit t
    val stu : 'a t -> 'a t -> ('a -> unit t) -> (('a * 'a) -> unit t) -> unit t
    val cseq : 'a t -> ('a -> 'b t) -> 'b t
    type poi = int

    val add_instr :
        ('b -> bool) -> (poi -> (poi * 'a) t) -> ('a -> 'b code) -> 'b code
    val (>>>) : (poi -> (poi * 'a) t) -> ('a -> 'b code) -> 'b code
    val (>>>>) : 'a t -> ('a -> 'b t) -> 'b t

(* Parallel composition *)
    val (>>|) : 'a t -> 'b t -> ('a * 'b)  t

    val (>>::) : 'a t -> 'a list t -> 'a list t
    val (|*|)   : bool code -> unit code -> unit code   (* Cross product *)
(*    val lockT : 'a t -> 'a t *)
    val forceT : 'a -> 'b t -> 'a t
    val (>>!) : 'a t -> 'b -> 'b t

    val discardT : 'a t -> unit t
    val addT : 'a -> 'b t -> ('a * 'b) t

    val choiceT : A.V.v -> 'a t -> 'a t -> 'a t 
    val condPredT : A.V.v -> unit t -> 'a t -> 'a t -> 'a t
    val condJumpT : A.V.v -> 'a code -> 'a code -> 'a code

    val altT : 'a t -> 'a t -> 'a t

    val tooFar : string -> 'a -> 'a t
    val tooFarcode : string -> 'a -> 'a code

        (* read_loc is_data mk_action loc ii
           for each value v that could be read,
           make an event structure comprising a single event with
           instruction id "ii", and action "mk_action v loc".
           is_data charaterizes the data port of a store *)

    val read_loc : bool -> (A.location -> A.V.v -> E.action) ->
      A.location -> A.inst_instance_id -> A.V.v t

    module Mixed :
    functor (SZ : ByteSize.S) -> sig

      val read_mixed : bool ->MachSize.sz ->
        (MachSize.sz -> A.location -> A.V.v -> E.action) ->
          A.V.v ->  A.inst_instance_id -> A.V.v t

      val write_mixed : MachSize.sz ->
      (MachSize.sz -> A.location -> A.V.v -> E.action) ->
        A.V.v -> A.V.v ->  A.inst_instance_id -> unit t

      val initwrites : (A.location * A.V.v) list -> A.size_env -> unit code

    end

   (* mk_singleton_es a ii:
      make an event structure comprising a single event with
           instruction id "ii", and action "a". *)
    val mk_singleton_es : E.action -> A.inst_instance_id -> unit t
    val mk_singleton_es_success : E.action -> A.inst_instance_id -> unit t
    val mk_singleton_es_eq : E.action -> VC.cnstrnts -> A.inst_instance_id -> unit t
        (* Similar, explicit empty output *)
    val mk_fence : E.action -> A.inst_instance_id -> unit t


    val op1 : Op.op1 -> A.V.v -> A.V.v t
    val op : Op.op -> A.V.v -> A.V.v -> A.V.v t
    val op3 : Op.op3 -> A.V.v -> A.V.v -> A.V.v -> A.V.v t
    val add : A.V.v -> A.V.v -> A.V.v t

(* Equality *)
    val assign : A.V.v -> A.V.v -> unit t
    val eqT : A.V.v -> A.V.v -> unit t
(* Acts as an inequality equation *)
    val neqT : A.V.v -> A.V.v -> unit t

(* Buid evt structure for fetch_and_op *)
    val fetch :
        Op.op -> A.V.v -> (A.V.v -> A.V.v -> E.action) ->
          A.inst_instance_id -> A.V.v t

(* Read out monad *)
    type evt_struct
    type output = VC.cnstrnts * evt_struct

    val get_output  : 'a code -> output list
  end
