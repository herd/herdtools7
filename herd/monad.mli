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
    val delay_kont : string -> 'a t -> ('a ->  'a t -> 'b t) -> 'b t
    val delay : 'a t -> ('a * 'a t) t

(* Data composition, entry for snd monad: minimals for iico_data *)
    val (>>=) : 'a t -> ('a -> 'b t) -> ('b) t

    val (>>==) : 'a t -> ('a -> 'b t) -> 'b t (* Output events stay in first arg *)
(* Control composition *)
    val (>>*=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>>*==) : 'a t -> ('a -> 'b t) -> 'b t (* Output events stay in first argument *)

    (* Control composition, avoid events from first argument) *)
    val bind_ctrl_avoid : 'c t -> 'a t -> ('a -> 'b t) -> 'b t

    (* Data composition, entry for snd monad: minimals for complete iico *)
    val bind_data_to_minimals : 'a t -> ('a -> 'b t) -> ('b) t

    (* Hybrid composition m1 m2 m3, m1 -ctrl+data-> m3 and m2 -data-> m3.
       ctrl+data -> ctrl from maximal commit evts + data from
       monad output *)
    val bind_ctrldata_data : 'a t -> 'b t -> ('a -> 'b -> 'c t) -> 'c t

    (* Similar, no second argument *)
    val bind_ctrldata : 'a t -> ('a -> 'c t) -> 'c t

    (* Same as bind_ctrldata, all output from first argument *)
    val (>>**==) : 'a t -> ('a -> 'b t) -> 'b t

    (* Identical control dep only, all output from firtst argument *)
    val bind_ctrl_first_outputs : 'a t -> ('a -> 'b t) -> 'b t

    val check_tags : 'v t -> ('v -> 'v t) -> ('v -> 'v t) -> 'x t -> 'v t
    val exch : 'a t -> 'a t -> ('a -> 'b t) ->  ('a -> 'c t) ->  ('b * 'c) t

(*
  Those amo_strict and swp are the AArch64 combinators,
  with complete internal dependencies.

  In particular there is a iico_ctrl dependency
  from read_register to write_register.
  The dependency from read_mem to write_mem is iico_ctrl for swp
  and iico_data for amo_strict.

  First bool argument <=> physical access.
*)
    val swp : bool -> ('loc t) ->
      ('loc -> A.V.v t) -> A.V.v t (* read reg *) ->
        ('loc -> A.V.v -> unit t) ->  (A.V.v -> unit t) (* Write reg *)
          -> unit t

    val amo_strict : bool -> Op.op -> ('loc t) ->
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

    val aarch64_store_conditional :
      bool -> (* must fail *)
        A.V.v t -> A.V.v t -> A.V.v t -> (* read reserve, data, address *)
          (unit t) -> (* write reserve *)
          (A.V.v -> unit t) -> (* write result *)
              (A.V.v -> A.V.v -> A.V.v -> unit t) -> (* write mem *)
                unit t

    val aarch64_cas_no :
      bool -> (* physical access *)
        'loc t -> 'v t ->
          ('v -> unit t) -> ('loc -> 'v t) ->
            ('v -> 'v -> unit t) -> unit t

    val aarch64_cas_ok :
      bool -> (* physical access *)
        'loc t -> 'v t -> 'v t ->
          ('v -> unit t) -> ('loc -> 'v t) -> ('loc -> 'v -> unit t) ->
            ('v -> 'v -> unit t) -> unit t
    (* Temporary morello variation of CAS *)
    val aarch64_cas_ok_morello :
        'loc t -> 'v t -> 'v t -> ('loc -> 'v -> unit t) -> unit t
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
    val (|||) : unit t -> unit t -> unit t

    val (|*|)   : bool code -> unit code -> unit code   (* Cross product *)
(*    val lockT : 'a t -> 'a t *)
    val forceT : 'a -> 'b t -> 'a t
    val (>>!) : 'a t -> 'b -> 'b t

    val discardT : 'a t -> unit t
    val addT : 'a -> 'b t -> ('a * 'b) t

    val assertT : A.V.v -> 'a t -> 'a t
    val choiceT : A.V.v -> 'a t -> 'a t -> 'a t
    val condPredT : A.V.v -> unit t -> 'a t -> 'a t -> 'a t
    val condJumpT : A.V.v -> 'a code -> 'a code -> 'a code

    val altT : 'a t -> 'a t -> 'a t

    val tooFar : string -> E.A.inst_instance_id -> 'v -> 'v t

    (**********************************************************)
    (* A few action instruction instance -> monad constructors *)
    (**********************************************************)
    val mk_singleton_es : E.action -> A.inst_instance_id -> unit t
    val mk_singleton_es_success : E.action -> A.inst_instance_id -> unit t
    val mk_singleton_es_eq :
      E.action -> VC.cnstrnts -> A.inst_instance_id -> unit t

    (****************)
    (* Basic monads *)
    (****************)

        (* read_loc is_data mk_action loc ii
           for each value v that could be read,
           make an event structure comprising a single event with
           instruction id "ii", and action "mk_action v loc".
           is_data charaterizes the data port of a store *)

    (* Read, the first, boolean, argument identifies a store data port *)
    val do_read_loc : bool -> (A.location -> A.V.v -> E.action) ->
      A.location -> E.iiid -> A.V.v t
    val read_loc : bool -> (A.location -> A.V.v -> E.action) ->
      A.location -> A.inst_instance_id -> A.V.v t

    val do_write_loc :
      (A.location -> E.action) -> A.location -> E.iiid -> unit t
    val write_loc :
      (A.location -> E.action) -> A.location -> A.inst_instance_id -> unit t
    (* Fence, must be used when output is absent *)
    val mk_fence : E.action -> A.inst_instance_id -> unit t
    (* Fetch and op *)
    val fetch :
        Op.op -> A.V.v -> (A.V.v -> A.V.v -> E.action) ->
          A.inst_instance_id -> A.V.v t

    (**********************)
    (* Morello extensions *)
    (**********************)

    val add_atomic_tag_read : A.V.v t -> A.V.v -> (A.location -> A.V.v ->
      E.action) -> A.inst_instance_id -> A.V.v t
    val add_atomic_tag_write : unit t -> A.V.v -> A.V.v -> (A.location ->
      A.V.v -> E.action) -> A.inst_instance_id -> unit t

    module Mixed :
    functor (SZ : ByteSize.S) -> sig

      val read_mixed : bool ->MachSize.sz ->
        (MachSize.sz -> A.location -> A.V.v -> E.action) ->
          A.V.v ->  A.inst_instance_id -> A.V.v t

      val write_mixed : MachSize.sz ->
      (MachSize.sz -> A.location -> A.V.v -> E.action) ->
        A.V.v -> A.V.v ->  A.inst_instance_id -> unit t

      (* Generate initial code monad, first argument represent additional
         events, beyond intwrites themselves *)
      val initwrites :
        (unit t -> unit t) ->
          (A.location * A.V.v) list -> A.size_env -> unit code

      (* Generate a code monad for initial instructions *)
      val initinstructions :
        (int -> Label.Set.t) ->
          (A.proc * ((int * A.instruction) list)) list -> unit code

      (* Generate a code monad for initial events for instructions and data;
         combines the functionality of initwrites and initinstructions *)
      val init_writes_and_instr :
        (unit t -> unit t) ->
            (A.location * A.V.v) list -> A.size_env ->
        (int -> Label.Set.t) ->
          (A.proc * ((int * A.instruction) list)) list -> unit code

    end

(* Operations *)
    val op1 : Op.op1 -> A.V.v -> A.V.v t
    val op : Op.op -> A.V.v -> A.V.v -> A.V.v t
    val op3 : Op.op3 -> A.V.v -> A.V.v -> A.V.v -> A.V.v t
    val add : A.V.v -> A.V.v -> A.V.v t

(* Equality *)
    val assign : A.V.v -> A.V.v -> unit t
    val eqT : A.V.v -> A.V.v -> unit t
(* Acts as an inequality equation *)
    val neqT : A.V.v -> A.V.v -> unit t

(* Read out monad *)
    type evt_struct
    type output = VC.cnstrnts * evt_struct

    val get_output  : 'a code -> output list -> output list
  end
