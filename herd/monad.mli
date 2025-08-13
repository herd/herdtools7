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
(* Authors:                                                                 *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(* Hadrien Renaud, University College London, UK.                           *)
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
    and type arch_op1 = A.V.arch_op1
    and type arch_op = A.V.arch_op

    type 'a t

    type 'a code
    val zeroT        : 'a t
    val zerocodeT        : 'a code
    val unitT        : 'a -> 'a t
    val warnT : string -> 'a -> 'a t
    val failT : exn -> 'a -> 'a t
    val ignore : 'a -> unit t
    val unitcodeT        : 'a -> 'a code
    val failcodeT        : exn -> 'a -> 'a code
    val warncodeT        : string -> 'a -> 'a code
    val delay_kont : string -> 'a t -> ('a ->  'a t -> 'b t) -> 'b t
    val delay : 'a t -> ('a * 'a t) t

    val set_standard_input_output : 'a t -> 'a t

    (* [restrict constraints] is an empty monad with the constraints [constraints] *)
    val restrict : VC.cnstrnts -> unit t

    (* Data composition, entry for snd monad: minimals for iico_data *)
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
 (* Input to second arg *)
    val data_input_next : 'a t -> ('a -> 'b t) -> 'b t
 (* Input to both args *)
    val data_input_union : 'a t -> ('a -> 'b t) -> 'b t

    (* Same as [>>=] but sequences partial_po *)
    val asl_data : 'a t -> ('a -> 'b t) -> 'b t

    val (>>==) : 'a t -> ('a -> 'b t) -> 'b t (* Output events stay in first arg *)
    (* Input to both args *)

    val data_output_union : 'a t -> ('a -> 'b t) -> 'b t
    (** [data_output_union s f] returns a composition of the event structures of
        [s] and the result of [f] where the [iico_causality_data] includes pairs
        (e1, e2) where e1 is an output event of e1 and e2 an input event of the
        result of [f]. The output of the resulting event structure is the union
        of the output events of [s] and the output events of the result of [f].
        *)

(* Control composition *)
    val (>>*=) : 'a t -> ('a -> 'b t) -> 'b t

    val control_input_union :  'a t -> ('a -> 'b t) -> 'b t
    (** Input is union of both arg inputs *)

    val control_input_next :  'a t -> ('a -> 'b t) -> 'b t
    (** Input is second arg's input *)

    val (>>*==) : 'a t -> ('a -> 'b t) -> 'b t (* Output events stay in first argument *)
    val bind_control_set_data_input_first :
      'a t -> ('a -> 'b t) -> 'b t (* Data input fixed in first argumst *)

    (* Control composition, avoid events from first argument) *)
    val bind_ctrl_avoid : 'c t -> 'a t -> ('a -> 'b t) -> 'b t

    (* Data composition, entry for snd monad: minimals for complete iico *)
    val bind_data_to_minimals : 'a t -> ('a -> 'b t) -> ('b) t

    val bind_data_to_output : 'a t -> ('a -> 'b t) -> 'b t
    (** [bind_data_to_output s f] returns a composition of the event structures
        of [s] and the result of [f] where the [iico_causality_data] includes
        pairs (e1, e2) where e1 is an output event of e1 and e2 an onput event
        of the result of [f]. The output of the resulting event structure is the
        union of the output events of [s] and the output events of the result of
        [f]. *)

    (* Control compoisition, but output events might be in first event if
       second is empty. *)
    val bind_ctrl_seq_data : 'a t -> ('a -> 'b t) -> 'b t

    (* Similar as [bind_ctrl_seq_data] but sequences partial_po *)
    val asl_ctrl : 'a t -> ('a -> 'b t) -> 'b t

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

    (* Same as [>>=] but with order deps instead of data between the arguments. *)
    val bind_order : 'a t -> ('a -> 'b t) -> 'b t

    val short : (E.event -> bool) -> (E.event -> bool) -> 'a t -> 'a t
    (** [short p1 p2 s] adds iico_causality_data relations to [s].
        New relation start from all events in [s] that satisfy [p1]
        and finish on all events in [s] that satisfy [p2]. *)

    (* Another ad-hoc transformation. [upOneRW p m]
     * Let r be iico_data, e1 and e2 be events s.t. p is true, e1 is 1 read e2 is a write,
     * and there exists e0, s.r. e1 -r-> e0 -r-> e2, then replace e0 -r0-> e2 by e1 -r-> e2.
     *)
    val upOneRW : (E.event -> bool) -> 'a t -> 'a t

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

    val amo_strict : bool -> A.V.op_t -> ('loc t) ->
      ('loc -> A.V.v t) -> A.V.v t (* read reg *) ->
        ('loc -> A.V.v -> unit t) ->  (A.V.v -> unit t) (* Write reg *)
          -> unit t


    val linux_exch :
        'loc t -> 'v t -> ('loc -> 'w t) -> ('loc -> 'v -> unit t) -> 'w t

(* Weak amo, without control dependency from read read to write reg.
  In fact, the write reg is absent *)
    val amo : A.V.op_t ->
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
              ('v -> unit t) -> ('loc -> 'v t) -> ('loc -> 'v -> unit t) ->
              ('loc -> unit t) ->
                ('v -> 'v -> unit t) -> unit t

    val aarch64_cas_ok :
      bool -> (* physical access *)
        'loc t -> 'v t -> 'v t ->
          ('v -> unit t) -> ('loc -> 'v t) -> ('loc -> 'v -> unit t) ->
           ('loc -> unit t) ->
            ('v -> 'v -> unit t) -> unit t
    (* Temporary morello variation of CAS *)
    val aarch64_cas_ok_morello :
        'loc t -> 'v t -> 'v t -> ('loc -> 'v -> unit t) -> unit t
    val stu : 'a t -> 'a t -> ('a -> unit t) -> (('a * 'a) -> unit t) -> unit t

    type poi = int

    val add_instr :
        ('b -> bool) -> (poi -> (poi * 'a) t) -> ('a -> 'b code) -> 'b code
    val (>>>) : (poi -> (poi * 'a) t) -> ('a -> 'b code) -> 'b code
    val (>>>>) : 'a t -> ('a -> 'b t) -> 'b t

(* Parallel composition *)
    val (>>|) : 'a t -> 'b t -> ('a * 'b)  t
    val para_atomic :  'a t -> 'b t -> ('a * 'b)  t (* For single copy atomic memory accesses *)
    val para_input_right : 'a t -> 'b t -> ('a * 'b)  t (* Input in second argument *)
    val (>>::) : 'a t -> 'a list t -> 'a list t
    val (|||) : unit t -> unit t -> unit t

    val cseq : 'a t -> ('a -> 'b t) -> 'b t
    (** [cseq s1 s2] similar to [>>|], but binding style. *)

    val para_bind_output_right : 'a t -> ('a -> 'b t) -> 'b t
    (** [para_bind_output_right s f] returns a parallel composition of
        the event structures of [s] and the result of [f] where the
        input of the new event structure is the union of the inputs of
        [s] and the result of [f], like [cseq]. Unlike [cseq] the output of
        the resulting event structure is set to the result of [f]. *)

    val asl_seq : 'a t -> ('a -> 'b t) -> 'b t
    (** [asl_seq s f] returns a parallel composition of [s] and the result of
        [f] where the input of the new event structure is the union of the
        inputs of [s] and the result of [f], like [cseq] and
        [para_bind_output_right]. Unlike the later, a [partial_po] arrow is put
        between the two structures. *)

    val seq_mem : 'a t -> 'b t -> ('a * 'b) t
    (** [seq_mem s1 s2] returns a composition of the event structures
        of [s1] and [s2] where in addition to the existing relations,
        every memory event in [s1] is iico_order before every memory
        event in [s2] *)

    val seq_mem_list : 'a t -> 'a list t -> 'a list t
    (** [seq_mem_list] is similar to [seq_mem], but cons the results instead of
        pairing them *)

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
    val indirectJumpT :
      A.V.v -> Label.Full.Set.t -> (Label.t -> 'a code) -> 'a code

    val altT : 'a t -> 'a t -> 'a t

    val cutoffT : string -> E.A.inst_instance_id -> 'v -> 'v t

    val debugT : string -> 'a t -> 'a t
    (** [debugT str s] prints [str] followed by a string
        representation of the input event structure [s], and returns
        the input [s] without making any changes to it *)

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

        (* read_loc is_addr mk_action loc ii
           for each value v that could be read,
           make an event structure comprising a single event with
           instruction id "ii", and action "mk_action v loc".
           is_addr charaterizes the address ports of a memory effect *)

    (* Read, the first, boolean, argument identifies a store data port *)
    val do_read_loc : Port.t -> (A.location -> A.V.v -> E.action) ->
      A.location -> E.iiid -> A.V.v t
    val read_loc : Port.t -> (A.location -> A.V.v -> E.action) ->
      A.location -> A.inst_instance_id -> A.V.v t

    val do_write_loc :
      (A.location -> E.action) -> A.location -> E.iiid -> unit t
    val write_loc :
      (A.location -> E.action) -> A.location -> A.inst_instance_id -> unit t
    (* Fence, must be used when output is absent *)
    val mk_fence : E.action -> A.inst_instance_id -> unit t
    (* Fetch and op *)
    val fetch :
        A.V.op_t -> A.V.v -> (A.V.v -> A.V.v -> E.action) ->
          A.inst_instance_id -> A.V.v t

    (* [as_addr_port m] flags all events in [m] as being of a given port. *)
    val as_port : Port.t -> 'a t -> 'a t

    (* [as_addr_port m] flags all events in [m] as address. *)
    val as_addr_port : 'a t -> 'a t

    (**********************)
    (* Morello extensions *)
    (**********************)

    val add_atomic_tag_read : A.V.v t -> A.V.v -> (A.location -> A.V.v ->
      E.action) -> A.inst_instance_id -> A.V.v t
    val add_atomic_tag_write : unit t -> A.V.v -> A.V.v -> (A.location ->
      A.V.v -> E.action) -> A.inst_instance_id -> unit t

    module Mixed :
    functor (SZ : ByteSize.S) -> sig

      val read_mixed : Port.t ->MachSize.sz ->
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

    end

(* Operations *)
    val op1 : A.V.op1_t -> A.V.v -> A.V.v t
    val op : A.V.op_t -> A.V.v -> A.V.v -> A.V.v t
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

    (* Force executed only once. *)
    val force_once : 'a t -> 'a t

    (* Squash this execution. Use with caution. *)
    val prune_execution : unit -> 'a t
  end
