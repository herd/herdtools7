(*
 * The Armv8 Application Level Memory Model.
 *
 * This is a machine-readable, executable and formal artefact, which aims to be
 * an equivalence proof between the three formulations of the
 * latest stable version of the Armv8 memory model.
 * If you have comments on the content of this file, please send an email to
 * memory-model@arm.com, referring to version number:
 * 814a6fc1610ec1a24f2cbd178e171966375626ac
 * For a textual version of the model, see section B2.3 of the Armv8 ARM:
 *   https://developer.arm.com/docs/ddi0487/latest/arm-architecture-reference-manual-armv8-for-armv8-a-architecture-profile
 *
 * Author: Jade Alglave <jade.alglave@arm.com>
 *
 * Copyright (C) 2016-2021, Arm Ltd.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in
 *       the documentation and/or other materials provided with the
 *       distribution.
 *     * Neither the name of ARM nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

Require Import ZArith.
Require Import Ensembles.
Require Import Classical_Prop.
Hypothesis excluded_middle : forall A, A \/ ~A.
Set Implicit Arguments.

Ltac decide_equality := decide equality; auto with equality arith.

(* Some facts about natural numbers *)
Lemma nat_eq_or_not_eq :
  forall (n1 n2 : nat), n1 = n2 \/ n1 <> n2.
Proof.
  intros n1 n2.
  rewrite <- eq_nat_is_eq.
  destruct (eq_nat_decide n1 n2); auto.
Qed.

Lemma nat_neq_implies_lt :
  forall (n1 n2 : nat), n1 <> n2 -> (n1 < n2 \/ n2 < n1).
Proof.
  intros n1 n2 Hneq.
  generalize (nat_total_order n1 n2).
  intro Htot.
  apply Htot. auto.
Qed.

(** * Events **)
(** ** Events: Definitions *)
Definition Acquire := Set.
Definition Release := Set.
Definition RMW := Set.
Definition Read := Set.
Hypothesis read_eq_or_not_eq :
  forall (r1 r2 : Read), r1 = r2 \/ r1 <> r2.

Definition Write := Set.
Hypothesis write_eq_or_not_eq :
  forall (w1 w2 : Write), w1 = w2 \/ w1 <> w2.

Definition Fence := Set.
Hypothesis fence_eq_or_not_eq :
  forall (f1 f2 : Fence), f1 = f2 \/ f1 <> f2.

Inductive Effect :=
  | read : Read -> Effect
  | write : Write -> Effect
  | fence : Fence -> Effect.

Lemma eff_eq_or_not_eq :
  forall (eff1 eff2 : Effect), eff1 = eff2 \/ eff1 <> eff2.
Proof.
intros eff1 eff2;
case_eq eff1.
  (*eff1 is a read*)
  intros r1 Hr1; case_eq eff2; intros r2 Hr2.
    (*eff2 is a read*)
    generalize (read_eq_or_not_eq r1 r2); intros [Heq12 | Hneq12].
      rewrite Heq12; auto.
      right; intro Heq; inversion Heq; apply Hneq12; auto.
    (*eff2 is a write*)
    right; intro Heq; inversion Heq.
    (*eff2 is a fence*)
    right; intro Heq; inversion Heq.

  (*eff1 is a write*)
  intros w1 Hr1; case_eq eff2; intros w2 Hr2.
    (*eff2 is a read*)
    right; intro Heq; inversion Heq.
    (*eff2 is a write*)
    generalize (write_eq_or_not_eq w1 w2); intros [Heq12 | Hneq12].
      rewrite Heq12; auto.
      right; intro Heq; inversion Heq; apply Hneq12; auto.
    (*eff2 is a fence*)
    right; intro Heq; inversion Heq.

  (*eff1 is a fence*)
  intros f1 Hf1; case_eq eff2; intros f2 Hf2.
    (*eff2 is a read*)
    right; intro Heq; inversion Heq.
    (*eff2 is a write*)
    right; intro Heq; inversion Heq.
    (*eff2 is a fence*)
    generalize (fence_eq_or_not_eq f1 f2); intros [Heq12 | Hneq12].
      rewrite Heq12; auto.
      right; intro Heq; inversion Heq; apply Hneq12; auto.
Qed.

Definition Id := nat.
Definition Location := nat.
Definition Value := nat.

Record Event := mkev {
  id     : Id;        (* Unique identifier, consistent with program order for a given thread *)
  tid    : Id;        (* Thread identifier *)
  effect : Effect;    (* Effect type (read, write, etc) *)
  loc    : Location;  (* Location accessed by the effect *)
  val    : Value      (* Value read or written by the effect *)
}.

Definition is_write e : Prop :=
  match effect e with
  | write _ => True
  | _ => False
  end.

Definition is_read e : Prop :=
  match effect e with
  | read _ => True
  | _ => False
  end.

Hypothesis event_id_uniq : forall e1 e2, e1 <> e2 -> id e1 <> id e2.

(** ** Events: Lemmas *)
Lemma event_eq_or_not_eq :
  forall (e1 e2 : Event), e1 = e2 \/ e1 <> e2.
Proof.
intros [id1 tid1 eff1 l1 v1] [id2 tid2 eff2 l2 v2].
generalize (nat_eq_or_not_eq id1 id2); intro Horid.
generalize (nat_eq_or_not_eq tid1 tid2); intro Hortid.
generalize (nat_eq_or_not_eq l1 l2); intro Horloc.
generalize (nat_eq_or_not_eq v1 v2); intro Horval.
generalize (eff_eq_or_not_eq eff1 eff2); intro Horeff.
inversion Horid as [Heqid | Hneqid]; clear Horid.
inversion Hortid as [Heqtid | Hneqtid]; clear Hortid.
inversion Horloc as [Heqloc | Hneqloc]; clear Horloc.
inversion Horval as [Heqval | Hneqval]; clear Horval.
inversion Horeff as [Heqeff | Hneqeff]; clear Horeff.
left.
  rewrite Heqid; rewrite Heqtid; rewrite Heqeff; rewrite Heqloc; rewrite Heqval; auto.
right.
  intro Heq;
  injection Heq as _ _ Heff _ _;
  apply Hneqeff; auto.
right.
  intro Heq;
  injection Heq as _ _ _ _ Hval;
  apply Hneqval; auto.
right.
  intro Heq;
  injection Heq as _ _ _ Hloc _;
  apply Hneqloc; auto.
right.
  intro Heq;
  injection Heq as _  Htid _ _ _;
  apply Hneqtid; auto.
right.
  intro Heq;
  injection Heq as Hid _ _ _ _;
  apply Hneqid; auto.
Qed.

(** * Sets and relations ****)
(** ** Sets and relations: Definitions *)

Definition set := Ensemble.
Definition Rln (A:Type) := A -> A -> Prop.

Definition dom (A:Type) (r:Rln A) : set A := fun x => exists y, r x y.
Definition ran (A:Type) (r:Rln A) : set A := fun y => exists x, r x y.

Inductive transitive_closure (r : Rln Event) (e1 e2 : Event) : Prop :=
  | _base : r e1 e2 -> transitive_closure r e1 e2
  | _trans : forall e, r e1 e -> transitive_closure r e e2 -> transitive_closure r e1 e2.

Definition irreflexive (r : Rln Event) : Prop := ~(exists x, r x x).
Definition acyclic (r : Rln Event) : Prop := irreflexive (transitive_closure r).

Definition rel_incl (A:Type) (r1 r2 : Rln A) : Prop :=
  forall x y, r1 x y -> r2 x y.
Definition rel_equal (r1 r2 : Rln Event) : Prop :=
  rel_incl r1 r2 /\ rel_incl r2 r1.
Definition rel_seq (r1 r2 : Rln Event) : Rln Event := 
  fun e1 e2 => exists e, r1 e1 e /\ r2 e e2.
Definition rel_union (r1 r2 : Rln Event) : Rln Event := 
  fun e1 e2 => r1 e1 e2 \/ r2 e1 e2.
Definition maybe r : Rln Event :=
  fun e1 e2 => e1 = e2 \/ r e1 e2.
Definition transitive (A:Type) (r:Rln A) : Prop :=
  (forall x1 x2 x3, (r x1 x2) -> (r x2 x3) -> (r x1 x3)).

Axiom Extensionality_Rlns : forall R1 R2:Rln Event, rel_incl R1 R2 /\ rel_incl R2 R1 -> R1 = R2.

Definition Class (A:Type) := set A.
Definition class_of (A:Type) (r : Rln A) e := fun e' => r e e'.
Definition classes (A:Type) (r:Rln A) : set (Class A) :=
  fun c => exists x, c = class_of r x.

Definition lift (A:Type) (C:set (Class A)) (r:Rln A) : Rln (Class A) :=
  fun c1 => fun c2 => 
    C c1 /\ C c2 /\ exists e1, exists e2, c1 e1 /\ c2 e2 /\ r e1 e2.
Definition delift (A:Type) (C : set (Class A)) (rC : Rln (Class A)) : Rln A :=
  fun e1 => fun e2 =>
    forall c1 c2, C c1 -> C c2 -> c1 e1 -> c2 e2 -> rC c1 c2.

(** ** Sets and relations: Lemmas *)

Lemma tc_trans r e1 e2 e3 :
  transitive_closure r e1 e2 ->
  transitive_closure r e2 e3 ->
  transitive_closure r e1 e3.
Proof.
intros H12 H23; induction H12.
  apply _trans with e2; auto.
  apply _trans with e; auto.
Qed.

Lemma seq_tc_reorg (r1 r2 : Rln Event) x y : 
  rel_seq r1 (rel_seq r2 r1) x y ->
  rel_seq (transitive_closure (rel_seq r1 r2)) r1 x y.
Proof.
intros [e1 [Hx1 [e2 [H12 H2y]]]]; exists e2; split; auto; apply _base; exists e1; split; auto.
Qed.

Lemma tc_seq_inv (r1 r2: Rln Event) x z :
  transitive_closure (rel_seq (maybe r1) r2) x z ->
  exists y1, exists y2, (maybe r1) x y1 /\ 
                        transitive_closure (rel_seq r2 (maybe r1)) y1 y2 /\
                        (maybe r2) y2 z.
Proof.
intro Hxz; induction Hxz. Focus 2.
  destruct H as [y1 [H1y1 Hy1e]]; destruct IHHxz as [e' [y2 [Hee' [He'y2 Hy22]]]].
    exists y1; exists y2; split; auto; split; auto.
    apply _trans with e'; auto; exists e; split; auto.

  destruct H as [y1 [H1y1 Hy12]]; exists y1; exists e2; split; auto; split; [apply _base; exists e2; split; auto; left|left]; auto.
Qed.
    
Lemma tc_seq_left (r1 r2 : Rln Event) x y z :
  transitive r1 ->
  r1 x y ->
  transitive_closure (rel_seq r1 r2) y z ->
  transitive_closure (rel_seq r1 r2) x z.
Proof.
intros Htr1 Hxy Hyz; induction Hyz. Focus 2.
  apply _trans with e; auto.
    destruct H as [e' [H1' H'e]]; exists e'; split; auto; apply Htr1 with e1; auto.

  apply _base; destruct H as [e' [H1' H'2]]; exists e'; split; auto; apply Htr1 with e1; auto.
Qed.

Lemma tc_seq_right (r1 r2 : Rln Event) x y z : 
  transitive r2 ->
  transitive_closure (rel_seq r1 r2) x y ->
  r2 y z ->
  transitive_closure (rel_seq r1 r2) x z.
Proof.
intros Htr2 Hxy Hyz; induction Hxy. Focus 2.
  apply _trans with e; auto.

  apply _base; destruct H as [x [H1x Hx2]]; exists x; split; auto; apply Htr2 with e2; auto.
Qed.

Lemma seq_tc_reorg2 (r1 r2 : Rln Event) x y z : 
  transitive r1 ->
  rel_seq r1 (rel_seq r2 r1) x y ->
  rel_seq (transitive_closure (rel_seq r1 r2)) r1 y z ->
  rel_seq (transitive_closure (rel_seq r1 r2)) r1 x z.
Proof.
intros Htr1 [e1 [Hx1 [e2 [H12 H2y]]]]; intros [e3 [Hy3 H3z]]; exists e3; split; auto;
apply tc_trans with e2; auto.
  apply _base; exists e1; auto.
  apply tc_seq_left with y; auto. 
Qed.

Lemma tc_seq_incl (r r1 r2 : Rln Event) x y :
  rel_incl r1 r2 ->
  rel_seq (transitive_closure r1) r x y ->
  rel_seq (transitive_closure r2) r x y.
Proof.
intros Hincl [e [Hxe Hey]]; exists e; split; auto; clear Hey; induction Hxe.
  apply _base; apply Hincl; auto.
  apply _trans with e; auto. 
Qed.

Lemma tc_seq_reorg (r1 r2 : Rln Event) x y z : 
  transitive r1 ->
  transitive_closure (rel_seq r1 (rel_seq r2 r1)) x y ->
  r1 y z ->
  transitive_closure (rel_seq r1 (rel_seq r2 r1)) x z.
Proof.
intros Htr1 Hxy Hyz; induction Hxy.
  destruct H as [e [H1e He2]]; apply _base; exists e; split; auto.
    destruct He2 as [e' [Hee' He'2]]; exists e'; split; auto; apply Htr1 with e2; auto.
  apply _trans with e; auto.
Qed.

Lemma r_in_evts_implies_tc_in_evts (E : set Event) (r : Rln Event) :
  Included _ (Union _ (dom r) (ran r)) E ->
  Included _ (Union _ (dom (transitive_closure r)) (ran (transitive_closure r))) E.
Proof.
intros Hincl _x [x Hdom | y Hran].
  inversion Hdom as [y Htc]; induction Htc.
    apply Hincl; left; exists e2; auto.
    apply Hincl; left; exists e; auto.

  inversion Hran as [x Htc]; induction Htc.
    apply Hincl; right; exists e1; auto.
    apply IHHtc; auto.
Qed.

Lemma tc_incl r1 r2 :
  rel_incl r1 r2 ->
  rel_incl (transitive_closure r1) (transitive_closure r2).
Proof.
intros Hincl x y Hxy; induction Hxy.
  apply _base; apply Hincl; auto.
  apply _trans with e; auto.
Qed. 

Lemma seq_tc_seq r1 r2 e1 e2 x y :
  transitive r2 ->
  transitive_closure (rel_seq r1 r2) e1 e2 ->
  maybe (rel_seq r1 r2) e2 x ->
  r2 x y ->
  transitive_closure (rel_seq r1 r2) e1 y.
Proof.
intros Htr2 H12 H2x Hxy.
inversion H2x as [Heq2x | Hs2x]; clear H2x.
  rewrite Heq2x in H12; apply tc_seq_right with x; auto.
  apply tc_trans with e2; auto; apply _base; destruct Hs2x as [e [H2e Hex]]; 
  exists e; split; auto; apply Htr2 with x; auto.
Qed.

(** * Linear and linear strict orders ****)
(** ** Orders: Definitions *)
Definition partial_order (A:Type) (r:Rln A) (xs:set A) : Prop :=
  Included _(Union _ (dom r) (ran r)) xs /\ (* If an event is in the relation, then it's also in the set *)
  transitive r /\ (* Transitivity *)
  (forall x, ~(r x x)). (* Irreflexivity *) 
Ltac destruct_part H := destruct H as [Hinc [Htrans Hirr]].

Definition linear_strict_order (A : Type) (r : Rln A) (xs : set A) : Prop :=
  partial_order r xs /\
  (forall x1 x2,
    (x1 <> x2) -> (xs x1) -> (xs x2) -> (r x1 x2) \/ (r x2 x1)). (* Total *)

Ltac destruct_lin H := destruct H as [Hpart Htot].

Parameter linearisations : Rln Event -> set Event -> set (Rln Event).
Parameter clinearisations : Rln (Class Event) -> set (Class Event) -> set (Rln (Class Event)).

Hypothesis order_ext : forall E r,
  partial_order r E ->
  (exists lin_ext, (linearisations r E) lin_ext).

Hypothesis lin_ext_prop : forall E r lin_ext,
  (linearisations r E) lin_ext <->
  rel_incl r lin_ext /\ linear_strict_order lin_ext E.

Hypothesis corder_ext : forall E r,
  partial_order r E ->
  (exists lin_ext, (clinearisations r E) lin_ext).

Hypothesis clin_ext_prop : forall E r lin_ext,
  (clinearisations r E) lin_ext <->
  rel_incl r lin_ext /\ linear_strict_order lin_ext E.

(** ** Orders: Lemmas *)
Lemma lin_of_big_is_lin_of_little :
  forall (s : set Event) (r1 r2 : Rln Event) (l : Rln Event),
  rel_incl r1 r2 ->
  (linearisations r2 s) l ->
  (linearisations r1 s) l.
Proof.
  intros s r1 r2 l.
  intros Hincl_r1r2 Hlin.
  rewrite lin_ext_prop in Hlin.
  rewrite lin_ext_prop.
  destruct Hlin as [Hincl_r2l Hlin].
  split.
    unfold rel_incl.
    intros e1 e2.
    intro Hinr1.
    generalize (Hincl_r1r2 e1 e2 Hinr1).
    apply Hincl_r2l.
  apply Hlin.
Qed.

Lemma clin_of_big_is_clin_of_little :
  forall (s : set (Class Event)) (r1 r2 : Rln (Class Event)) (l : Rln (Class Event)),
  rel_incl r1 r2 ->
  (clinearisations r2 s) l ->
  (clinearisations r1 s) l.
Proof.
  intros s r1 r2 l.
  intros Hincl_r1r2 Hlin.
  rewrite clin_ext_prop in Hlin.
  rewrite clin_ext_prop.
  destruct Hlin as [Hincl_r2l Hlin].
  split.
    unfold rel_incl.
    intros e1 e2.
    intro Hinr1.
    generalize (Hincl_r1r2 e1 e2 Hinr1).
    apply Hincl_r2l.
  apply Hlin.
Qed.
(** * Built-in herd relations ****)
(** ** Herd: Definitions*)
Definition internal (E : set Event) (e1 e2 : Event) : Prop := tid e1 = tid e2 /\ E e1 /\ E e2.
Definition external (E : set Event) (e1 e2 : Event) : Prop := ~(internal E e1 e2).

Definition po (E : set Event) (e1 e2 : Event) : Prop := internal E e1 e2 /\ lt (id e1) (id e2).
Definition po_loc (E : set Event) (e1 e2 : Event) : Prop := po E e1 e2 /\ loc e1 = loc e2.

Definition rf (E : set Event) (e1 e2 : Event) : Prop :=
  is_write e1 /\ is_read e2 /\ loc e1 = loc e2 /\ val e1 = val e2 /\ E e1 /\ E e2.
Ltac destruct_rf H := destruct H as [Hisw [Hisr [Hloceq [Hvaleq [Hinw Hinr]]]]].
Definition pre_co (E : set Event) (e1 e2 : Event) : Prop := 
  is_write e1 /\ is_write e2 /\ loc e1 = loc e2 /\ E e1 /\ E e2.
Definition fr (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) : Prop :=
  exists w, is_write w /\ rf E w e1 /\ co E w e2.

Definition rf_well_formed (E : set Event) : Prop :=
  partial_order (rf E) E /\
  forall (r : Event), is_read r -> 
    (exists w, rf E w r) /\
    (forall w1 w2, rf E w1 r -> rf E w2 r -> w1 = w2).
Ltac destruct_rf_wf H := destruct H as [Hpart_rf Hex_uni].

Definition is_write_same_loc (l : Location) (e : Event) : Prop :=
  is_write e /\ loc e = l.

Definition co_well_formed (E : set Event) (co : set Event -> Rln Event) : Prop :=
  (rel_incl (co E) (pre_co E)) /\
  (forall (l : Location), 
    linear_strict_order (co E) (Intersection _ E (is_write_same_loc l))).

Definition rfi (E : set Event) (e1 e2 : Event) : Prop := rf E e1 e2 /\ internal E e1 e2.
Definition coi (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) : Prop := co E e1 e2 /\ internal E e1 e2.
Definition fri (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) : Prop := fr E co e1 e2 /\ internal E e1 e2.

Definition rfe (E : set Event) (e1 e2 : Event) : Prop := rf E e1 e2 /\ external E e1 e2.
Definition coe (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) : Prop := co E e1 e2 /\ external E e1 e2.
Definition fre (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) : Prop := fr E co e1 e2 /\ external E e1 e2.

Definition corf E (co : set Event -> Rln Event) (e1 e2 : Event) :=
  exists e, co E e1 e /\ rf E e e2.
Definition corfe E (co : set Event -> Rln Event) (e1 e2 : Event) :=
  exists e, co E e1 e /\ rfe E e e2.
Definition coirf E (co : set Event -> Rln Event) (e1 e2 : Event) :=
  exists e, coi E co e1 e /\ rf E e e2.
Definition coerf E (co : set Event -> Rln Event) (e1 e2 : Event) :=
  exists e, coe E co e1 e /\ rf E e e2.
Definition coerfe E (co : set Event -> Rln Event) (e1 e2 : Event) :=
  exists e, coe E co e1 e /\ rfe E e e2.

Definition frrf E (co : set Event -> Rln Event) (e1 e2 : Event) :=
  exists e, fr E co e1 e /\ rf E e e2.
Definition frrfe E (co : set Event -> Rln Event) (e1 e2 : Event) :=
  exists e, fr E co e1 e /\ rfe E e e2.
Definition frrfi E (co : set Event -> Rln Event) (e1 e2 : Event) :=
  exists e, fr E co e1 e /\ rfi E e e2.
Definition frerf E (co : set Event -> Rln Event) (e1 e2 : Event) :=
  exists e, fre E co e1 e /\ rf E e e2.

Definition complus E (co : set Event -> Rln Event) e1 e2 := 
  rf E e1 e2 \/ co E e1 e2 \/ fr E co e1 e2 \/ 
  rel_seq (co E) (rf E) e1 e2 \/ rel_seq (fr E co) (rf E) e1 e2.

Definition ER E := ran (rfe E).
Definition IR E := ran(rfi E).
Definition M E := fun e => E e /\ (is_write e \/ is_read e). 

Definition si_well_formed (E:set Event) (si:Rln Event) :=
  (forall x, dom si x -> M E x) /\
  (forall x, ran si x -> M E x) /\
  (forall x, si x x) /\
  (forall x y, si x y -> si y x) /\
  (forall x y z, si x y -> si y z -> si x z) /\
  (forall x y, si x y -> is_write x -> is_write y) /\
  (forall x y, si x y -> is_read x -> is_read y).
Ltac destruct_siwf H := destruct H as [Hdom [Hran [Hrefl [Hsym [Htrans [Hw Hr]]]]]].

Definition scaob E si e1 e2 := si E e1 e2 /\ ER E e1 /\ IR E e2.
Definition rfisw E si := 
  fun r1 => fun r2 => exists w1, exists w2, rfi E w1 r1 /\ rfi E w2 r2 /\ si E w1 w2.

Definition erln E si := 
  fun e1 => fun e2 => M E e1 /\ M E e2 /\ si E e1 e2 /\ 
    ((is_write e1 /\ is_write e2) \/ (ER E e1 /\ ER E e2) \/ (rfisw E si e1 e2)).
Definition MemC E si := classes (erln E si).

(** ** Herd: Lemmas *)
Lemma int_or_ext (E : set Event) :
  forall (e1 e2 : Event), E e1 -> E e2 -> internal E e1 e2 \/ external E e1 e2.
Proof.
  intros e1 e2 Hin1 Hin2.
  unfold external.
  unfold internal.
  generalize (nat_eq_or_not_eq (tid e1) (tid e2)).
  intro Hor. inversion Hor as [Heq|Hdiff]; clear Hor.
    left; auto.
    right.
      unfold not.
      intros [Heq [? ?]].
      unfold not in Hdiff.
      apply Hdiff.
      apply Heq.
Qed.

Lemma int_ext_contrad E e1 e2 :
  internal E e1 e2 ->
  external E e1 e2 -> 
  False.
Proof.
intros Hint Hext; apply Hext; auto. 
Qed.

Lemma internal_trans E x y z :
  internal E x y ->
  internal E y z ->
  internal E x z.
Proof.
intros [Heqxy [? ?]] [Heqyz [? ?]]; split; [|split]; auto.
rewrite Heqxy; auto.
Qed.

Lemma internal_implies_po_or_po_minus_1 E x y :
  internal E x y ->
  x <> y ->
  po E x y \/ po E y x.
Proof.
intros Hint Hdiff.

assert (id x <> id y) as Hneq.
  apply event_id_uniq; auto.

generalize (nat_neq_implies_lt Hneq); intros [Hxy | Hyx]; [left | right]; split; auto.
destruct Hint as [Heq [? ?]]; split; [rewrite Heq|]; auto.
Qed.

Lemma dom_po_in_evts (E : set Event) (e1 e2 : Event) :
  po E e1 e2 -> E e1.
Proof.
  intros [[? [? ?]] ?]; auto.
Qed.

Lemma ran_po_in_evts (E : set Event) (e1 e2 : Event) :
  po E e1 e2 -> E e2.
Proof.
  intros [[? [? ?]] ?]; auto.
Qed.

Lemma dom_rf_in_evts (E : set Event) (e1 e2 : Event) :
  rf E e1 e2 -> E e1.
Proof.
  intros [_ [_ [_ [_ [He1_in_E _]]]]].
  auto.
Qed.

Lemma ran_rf_in_evts (E : set Event) (e1 e2:Event) :
  rf E e1 e2 -> E e2.
Proof.
  intros [_ [_ [_ [_ [_ He2_in_E]]]]].
  auto.
Qed.

Lemma dom_rf_is_write (E : set Event) (e1 e2:Event) :
  rf E e1 e2 -> is_write e1.
Proof.
  intros [? ?]; auto.
Qed.

Lemma ran_rf_is_read (E : set Event) (e1 e2:Event) :
  rf E e1 e2 -> is_read e2.
Proof.
  intros [? [Hr ?]].
  auto.
Qed.

Lemma rf_implies_same_loc (E : set Event) (e1 e2 : Event) :
  rf E e1 e2 -> loc e1 = loc e2.
Proof.
  intros [? [? [? ?]]]; auto.
Qed.

Lemma rf_implies_same_val (E : set Event) (e1 e2 : Event) :
  rf E e1 e2 -> val e1 = val e2.
Proof.
  intros [? [? [? [? ?]]]]; auto.
Qed.

Lemma dom_co_in_evts (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) :
  co_well_formed E co ->
  co E e1 e2 -> E e1.
Proof.
  intros [Hincl ?] Hco. destruct (Hincl e1 e2) as [? [? [? [? ?]]]]; auto.
Qed.

Lemma ran_co_in_evts (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) :
  co_well_formed E co ->
  co E e1 e2 -> E e2.
Proof.
  intros [Hincl ?] Hco. destruct (Hincl e1 e2) as [? [? [? [? ?]]]]; auto.
Qed.

Lemma dom_co_is_write (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) :
  co_well_formed E co ->
  co E e1 e2 -> is_write e1.
Proof.
  intros [Hincl ?] Hco; auto.
  generalize (Hincl e1 e2 Hco); intros [? [? ?]];
  auto.
Qed.

Lemma ran_co_is_write (E : set Event) (co : set Event -> Rln Event) (e1 e2:Event) :
  co_well_formed E co ->
  co E e1 e2 -> is_write e2.
Proof.
  intros [Hincl ?] Hco; auto.
  generalize (Hincl e1 e2 Hco); intros [? [? ?]].
  auto.
Qed.

Lemma co_implies_same_loc (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) :
  co_well_formed E co ->
  co E e1 e2 -> loc e1 = loc e2.
Proof.
  intros [Hincl ?] Hco; auto.
  generalize (Hincl e1 e2 Hco);intros [? [? [? ?]]]; auto.
Qed.

Lemma dom_fr_in_evts (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) :
  fr E co e1 e2 -> E e1.
Proof.
  intros [? [? [Hrf ?]]].
  destruct Hrf as [_ [_ [_ [_ [_ He1_in_E]]]]].
  auto.
Qed.

Lemma ran_fr_in_evts (E : set Event) (co : set Event -> Rln Event) (e1 e2:Event) :
  co_well_formed E co ->
  fr E co e1 e2 -> E e2.
Proof.
intros [Hincl ?] [w [Hw [Hrf Hco]]]; auto.
generalize (Hincl w e2 Hco); intros [? [? [? [? ?]]]]; auto.
Qed.

Lemma dom_fr_is_read (E : set Event) (co : set Event -> Rln Event) (e1 e2:Event) :
  fr E co e1 e2 -> is_read e1.
Proof.
  intros [x [? [Hrf ?]]]; apply ran_rf_is_read with E x; auto.
Qed.

Lemma ran_fr_is_write (E : set Event) (co : set Event -> Rln Event) (e1 e2:Event) :
  co_well_formed E co ->
  fr E co e1 e2 -> is_write e2.
Proof.
  intros Hcowf [x [? [? Hco]]]; apply ran_co_is_write with E co x; auto.
Qed.

Lemma fr_implies_same_loc (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) :
  co_well_formed E co ->
  fr E co e1 e2 -> loc e1 = loc e2.
Proof.
  intros [Hincl ?] [w [Hw [Hrf Hco]]].
  generalize (Hincl w e2 Hco); intros [? [? [Hloc ?]]].
  destruct_rf Hrf; rewrite <- Hloceq; auto.
Qed.

Lemma read_write_contrad (e : Event) :
  is_read e -> is_write e -> False.
Proof.
intros Hr Hw; unfold is_read in Hr; unfold is_write in Hw;
case_eq (effect e).

  intros r Her; rewrite Her in Hw; auto.
  intros w Hew; rewrite Hew in Hr; auto.
  intros f Hef; rewrite Hef in Hr; auto.
Qed.

Lemma fr_implies_diff (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) :
  co_well_formed E co ->
  fr E co e1 e2 ->
  e1 <> e2.
Proof.
intros [Hincl ?] [w [Hw [Hrf Hco]]].
destruct_rf Hrf.
generalize (Hincl w e2 Hco); intros [? [Hwe2 [? ?]]].
intro Heq; rewrite Heq in Hisr.
apply read_write_contrad with e2; auto.
Qed.

Lemma rf_fr_is_co E co e1 e2 e3 :
  rf_well_formed E ->
  rf E e1 e2 -> fr E co e2 e3 -> co E e1 e3.
Proof.
intros [? Hr] Hrf12 [w [Hw [Hrfw2 Hco]]].
generalize (ran_rf_is_read Hrf12); intro Hr2;
generalize (Hr e2 Hr2); intros [? Huni]; 
generalize (Huni e1 w Hrf12 Hrfw2); intro Heq; rewrite <- Heq in Hco; auto.
Qed.

Lemma co_trans E co e1 e2 e3 :
  co_well_formed E co ->
  co E e1 e2 -> co E e2 e3 -> co E e1 e3.
Proof.
intros Hcowf H12 H23; 
generalize (co_implies_same_loc e1 e2 Hcowf H12); intro Hl12;
generalize (co_implies_same_loc e2 e3 Hcowf H23); intro Hl23.
destruct Hcowf as [Hincl Hlin];
generalize (Hlin (loc e1)); clear Hlin; intro Hlin; destruct_lin Hlin;
destruct_part Hpart; apply Htrans with e2; auto.
Qed.

Lemma fr_co_is_fr E co e1 e2 e3 :
  co_well_formed E co ->
  fr E co e1 e2 -> co E e2 e3 -> fr E co e1 e3.
Proof.
intros Hcowf [w [Hw [Hrf Hco]]]; exists w; split; auto; split; auto; apply co_trans with e2; auto.
Qed.

Lemma rfe_in_rf E x y :
  rfe E x y -> rf E x y.
Proof.
intros [? ?]; auto.
Qed.

Lemma corfe_in_corf E co x y :
  corfe E co x y -> corf E co x y.
Proof.
intros [e [Hco [? ?]]]; exists e; split; auto.
Qed.

Lemma frrfe_in_frrf E co x y :
  frrfe E co x y -> frrf E co x y.
Proof.
intros [e [Hfr [? ?]]]; exists e; split; auto.
Qed.

Lemma rf_complus_in_complus E co e1 e2 e3 :
  rf_well_formed E ->
  co_well_formed E co -> 
  rf E e1 e2 -> complus E co e2 e3 -> complus E co e1 e3.
Proof.
intros Hrfwf [Hincl ?] Hrf12 [Hrf23 | [Hco23 | [Hfr23 | [Hcorf23 | Hfrrf23]]]]. 
  assert False as Ht.
    destruct Hrf12 as [? [Hr2 ?]]; destruct Hrf23 as [Hw2 ?]; apply (read_write_contrad e2 Hr2 Hw2).
  inversion Ht.
  assert False as Ht.
    generalize (Hincl e2 e3 Hco23); intro Hpco23.
    destruct Hrf12 as [? [Hr2 ?]]; destruct Hpco23 as [Hw2 ?]; apply (read_write_contrad e2 Hr2 Hw2).
  inversion Ht.
  right; left; apply rf_fr_is_co with e2; auto.
  assert False as Ht.
    destruct Hrf12 as [? [Hr2 ?]]; destruct Hcorf23 as [e [Hco Hrf]]; 
    generalize (Hincl e2 e Hco); intro Hpco.
    destruct Hpco as [Hw2 ?]; apply (read_write_contrad e2 Hr2 Hw2).
  inversion Ht.
  right; right; right; left; destruct Hfrrf23 as [e [Hfr Hrf]]; exists e; split; auto; apply rf_fr_is_co with e2; auto.
Qed.

Lemma co_complus_in_complus E co e1 e2 e3 :
  co_well_formed E co ->
  co E e1 e2 -> complus E co e2 e3 -> complus E co e1 e3.
Proof.
intros Hcowf Hco12 [Hrf23 | [Hco23 | [Hfr23 | [Hcorf23 | Hfrrf23]]]].
  right; right; right; left; exists e2; auto.
  right; left; apply co_trans with e2; auto.
  assert False as Ht.
    generalize (ran_co_is_write e1 e2 Hcowf Hco12); intro Hw2; generalize (dom_fr_is_read Hfr23); intro Hr2.
    apply (read_write_contrad e2 Hr2 Hw2).
  inversion Ht.
  right; right; right; left; destruct Hcorf23 as [e [Hcoe2e Hrfee3]]; exists e; split; auto; apply co_trans with e2; auto.
  assert False as Ht.
    destruct Hfrrf23 as [e [Hfr Hrf]].  
    generalize (ran_co_is_write e1 e2 Hcowf Hco12); intro Hw2; generalize (dom_fr_is_read Hfr); intro Hr2.
    apply (read_write_contrad e2 Hr2 Hw2).
  inversion Ht.
Qed.

Lemma fr_complus_in_complus E co e1 e2 e3 :
  co_well_formed E co ->
  fr E co e1 e2 -> complus E co e2 e3 -> complus E co e1 e3.
Proof.
intros Hcowf Hfr12 [Hrf23 | [Hco23 | [Hfr23 | [Hcorf23 | Hfrrf23]]]].
  right; right; right; right; exists e2; auto.
  right; right; left; apply fr_co_is_fr with e2; auto.
  assert False as Ht.
    generalize (ran_fr_is_write Hcowf Hfr12); intro Hw2; generalize (dom_fr_is_read Hfr23); intro Hr2;
    apply (read_write_contrad e2 Hr2 Hw2); auto.
  inversion Ht.
  destruct Hcorf23 as [e [Hco Hrf]]; right; right; right; right; exists e; split; auto; apply fr_co_is_fr with e2; auto.

  assert False as Ht.
    destruct Hfrrf23 as [e [Hfr Hrf]].
    generalize (ran_fr_is_write Hcowf Hfr12); intro Hw2; generalize (dom_fr_is_read Hfr); intro Hr2;
    apply (read_write_contrad e2 Hr2 Hw2); auto.
  inversion Ht.
Qed.

Lemma mcomplus_trans E co :
  rf_well_formed E ->
  co_well_formed E co ->
  transitive (maybe (complus E co)).
Proof.
intros Hrfwf Hcowf x y z Hxy Hyz.
inversion Hxy as [Heqxy | Hmcpxy]; clear Hxy; inversion Hyz as [Heqyz | Hmcpyz]; clear Hyz.
  left; rewrite Heqxy; auto.
  right; rewrite Heqxy; auto.
  right; rewrite <- Heqyz; auto.
  right; inversion Hmcpxy as [Hrf | [Hco | [Hfr | [[e [Hco Hrf]] | [e [Hfr Hrf]]]]]]; clear Hmcpxy.
    apply rf_complus_in_complus with y; auto.
    apply co_complus_in_complus with y; auto.
    apply fr_complus_in_complus with y; auto.
    apply co_complus_in_complus with e; auto; apply rf_complus_in_complus with y; auto.
    apply fr_complus_in_complus with e; auto; apply rf_complus_in_complus with y; auto.    
Qed.

Lemma complus_irr E co x :
  rf_well_formed E ->
  co_well_formed E co ->
  complus E co x x -> False.
Proof.
intros Hrfwf Hcowf; generalize Hcowf; intros [? Hlin]; intros [Hrf | [Hco | [Hfr | [Hcorf | Hfrrf]]]].
  apply (read_write_contrad x); [apply ran_rf_is_read with E x | apply dom_rf_is_write with E x]; auto.
  generalize (Hlin (loc x)); clear Hlin; intro Hlin; destruct_lin Hlin; destruct_part Hpart; generalize Hco; apply Hirr; auto.
  destruct Hfr as [w [Hw [Hrf Hco]]]; apply (read_write_contrad x); [apply ran_rf_is_read with E w | apply ran_co_is_write with E co w]; auto.
  destruct Hcorf as [e [Hco Hrf]]; apply (read_write_contrad x); [apply ran_rf_is_read with E e | apply dom_co_is_write with E co e]; auto.
  destruct Hfrrf as [w [[w' [Hw' [Hrf' Hco]]] Hrf]]. 
    generalize (ran_rf_is_read Hrf'); intro Hrx; destruct_rf_wf Hrfwf; 
    generalize (Hex_uni x Hrx); intros [_ Huni]; generalize (Huni w' w Hrf' Hrf);
    intro Heq; rewrite Heq in Hco; generalize (Hlin (loc w')); clear Hlin; 
    intro Hlin; destruct_lin Hlin; destruct_part Hpart; generalize Hco; 
    apply Hirr; auto.
Qed.

Lemma rfe_fri_is_coe E co x z y :
  rf_well_formed E ->
  co_well_formed E co ->
  rfe E x z ->
  fri E co z y ->
  coe E co x y.
Proof.
intros Hrfwf Hcowf [Hrfxz Hextxz] [Hfrzy [Heqzy [? ?]]]; split.
  apply rf_fr_is_co with z; auto.
  intros [Heqtidxy [? ?]]; apply Hextxz. split; [rewrite Heqzy|split]; auto.
Qed.

Lemma fri_coi_is_fri E co x y z :
  co_well_formed E co ->
  fri E co x y ->
  coi E co y z ->
  fri E co x z.
Proof.
intros Hcowf [Hfrxy Hintxy] [Hcoyz Hintyz]; split; auto.
  apply fr_co_is_fr with y; auto.
  apply internal_trans with y; auto.
Qed.

(** * ARMv8 axiomatic relations ****)
(** ** ARMv8: Definitions *)
Definition internal_visibility (E : set Event) (co : set Event -> Rln Event) : Prop :=
  acyclic (fun e1 e2 => rf E e1 e2 \/ co E e1 e2 \/ fr E co e1 e2 \/ po_loc E e1 e2).

Definition obs (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) : Prop :=
  rfe E e1 e2 \/ co E e1 e2 \/ fr E co e1 e2.
Definition obsplus E co :=
  transitive_closure (obs E co).

Inductive ob (E : set Event) (co : set Event -> Rln Event) (si lob : Rln Event) (e1 e2 : Event) : Prop :=
  | _obs : rel_seq (obs E co) si e1 e2 -> ob E co si lob e1 e2
  | _lob : lob e1 e2 -> ob E co si lob e1 e2
  | _ob : forall e, ob E co si lob e1 e -> ob E co si lob e e2 -> ob E co si lob e1 e2.

Definition external_visibility (E : set Event) (co : set Event -> Rln Event) (si lob : set Event -> Rln Event) : Prop :=
  irreflexive (ob E co (si E) (lob E)).

(** Well-formed lob: a relation lob over a set of events E is well-formed when:
    - lob is irreflexive
    - lob is transitive 
    - lob starts with a read or write memory event
    - lob is included in po 
    - lob; erln is included in lob
    - lob; scaob is included in lob 
    - scaob; lob is included in lob*)

Definition lob_well_formed (E:set Event) (si lob : set Event -> Rln Event) :=
  irreflexive (lob E) /\
  transitive (lob E) /\
  (forall e1 e2, lob E e1 e2 -> is_write e1 \/ is_read e1) /\
  rel_incl (lob E) (po E) /\
  rel_incl (rel_seq (lob E) (erln E si)) (lob E) /\
  rel_incl (rel_seq (lob E) (scaob E si)) (lob E) /\
  rel_incl (rel_seq (scaob E si) (lob E)) (lob E).   
Ltac destruct_lob_wf H := destruct H as [Hirr_lob [Htrans_lob [Hdom_lob [Hincl_po [Hlob_erln [Hlob_scaob Hscaob_lob]]]]]].

(** ** ARMv8: Lemmas *)
Lemma obs_in_mop (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) : 
  obs E co e1 e2 ->
  maybe (obsplus E co) e1 e2.
Proof.
right; left; auto.
Qed.

Lemma obsplus_dec E co e1 e2 :
  rf_well_formed E ->
  co_well_formed E co ->
  transitive_closure (obs E co) e1 e2 ->
  rfe E e1 e2 \/ co E e1 e2 \/ fr E co e1 e2 \/ corfe E co e1 e2 \/ frrfe E co e1 e2.
Proof.
intros Hrfwf Hcowf H12; generalize Hcowf; intros [Hincl Hcolin]; 
  induction H12 as [e1 e2 Hb | e1 e2 e H1e He2].
  inversion Hb as [Hrfe | [Hco | Hfr]]; auto.
  inversion H1e as [Hrfe1e | [Hco1e | Hfr1e]];
  inversion IHHe2 as [Hrfee2 | [Hcoe2 | [Hfre2 | [Hcorfee2 | Hfrrfee2]]]].

  destruct Hrfe1e as [[? [Hre ?]] ?]; destruct Hrfee2 as [[Hwe ?] ?]; generalize (read_write_contrad e Hre Hwe); intro Ht; inversion Ht.
  generalize (Hincl e e2 Hcoe2); intro Hpcoe2;
  destruct Hrfe1e as [[? [Hre ?]] ?]; destruct Hpcoe2 as [Hwe ?]; generalize (read_write_contrad e Hre Hwe); intro Ht; inversion Ht.
  destruct Hrfe1e as [? ?]; right; left; apply rf_fr_is_co with e; auto.
  destruct Hrfe1e as [[? [Hre ?]] ?]; destruct Hcorfee2 as [w [Hcoew Hrfe]]; 
  generalize (Hincl e w Hcoew); intro Hpcoew;
  destruct Hpcoew as [Hwe ?]; 
    generalize (read_write_contrad e Hre Hwe); intro Ht; inversion Ht.
  destruct Hrfe1e as [? ?]; destruct Hfrrfee2 as [w [Hfrew Hrfw2]]; right; right; right; left; exists w; split; auto; apply rf_fr_is_co with e; auto.
  right; right; right; left; exists e; split; auto.
  right; left; apply co_trans with e; auto.
  generalize (Hincl e1 e Hco1e); intro Hpco1e;
  destruct Hpco1e as [? [Hwe ?]]; destruct Hfre2 as [? [? [[? [Hre ?]] ?]]]; generalize (read_write_contrad e Hre Hwe); intro Ht; inversion Ht.
  destruct Hcorfee2 as [w [Hcoew Hrfw2]]; right; right; right; left; exists w; split; auto; apply co_trans with e; auto.
  destruct Hfrrfee2 as [w [Hfrew Hrfw2]]; 
    generalize (dom_fr_is_read Hfrew); intro Hre; generalize (ran_co_is_write e1 e Hcowf Hco1e); intro Hwe; generalize (read_write_contrad e Hre Hwe); intro Ht; inversion Ht.
  right; right; right; right; exists e; split; auto.
  right; right; left; apply fr_co_is_fr with e; auto.

  destruct Hfr1e as [? [? [? Hco]]]; generalize (Hincl x e Hco); intros [? [Hwe ?]];
  destruct Hfre2 as [? [? [[? [Hre ?]] ?]]]; generalize (read_write_contrad e Hre Hwe); intro Ht; inversion Ht.   
  destruct Hcorfee2 as [w [Hcoew Hrfewe2]]; right; right; right; right; exists w; split; auto; apply fr_co_is_fr with e; auto.
  destruct Hfrrfee2 as [w [Hfrew Hrfewe2]]; generalize (dom_fr_is_read Hfrew); intro Hre; generalize (ran_fr_is_write Hcowf Hfr1e); intro Hwe;
  generalize (read_write_contrad e Hre Hwe); intro Ht; inversion Ht.
Qed.

Lemma op_trans E co e1 e2 e3 :
  (obsplus E co) e1 e2 ->
  (obsplus E co) e2 e3 ->
  (obsplus E co) e1 e3.
Proof.
intros H12 H23; apply tc_trans with e2; auto.
Qed.   

Lemma mop_trans E co e1 e2 e3 :
  maybe (obsplus E co) e1 e2 ->
  maybe (obsplus E co) e2 e3 ->
  maybe (obsplus E co) e1 e3.
Proof.
intros [Heq12 | Htc12].
  rewrite Heq12; auto.
  intros [Heq23 | Htc23].
    rewrite <- Heq23; right; auto.
    right; apply op_trans with e2; auto.
Qed.

Lemma obsplus_in_scpv E co x y :
  rf_well_formed E ->
  co_well_formed E co ->
  obsplus E co x y ->
  transitive_closure (fun e1 e2 : Event => rf E e1 e2 \/ co E e1 e2 \/ fr E co e1 e2 \/ po_loc E e1 e2) x y.
Proof.
intros Hrfwf Hcowf Hxy; generalize (obsplus_dec Hrfwf Hcowf Hxy); intros [[Hrf ?] | [Hco | [Hfr | [[z [Hco [Hrf ?]]] | [z [Hfr [Hrf ?]]]]]]].
  apply _base; left; auto.
  apply _base; right; left; auto.
  apply _base; right; right; left; auto.
  apply _trans with z; [right |]; left; auto. 
  apply _trans with z; [right; right |]; left; auto. 
Qed.

Lemma posRW_is_fri E co x y :
  rf_well_formed E ->
  co_well_formed E co ->
  internal_visibility E co ->
  is_read x -> is_write y ->
  po E x y -> loc x = loc y ->
  fri E co x y.
Proof.
intros Hrfwf [? Hlin] Hintv Hrx Hwy Hpoxy Hlocxy; split.
  destruct_rf_wf Hrfwf; generalize (Hex_uni x Hrx); clear Hex_uni; intros [[w Hrf] Huni]; 
  exists w; split; [apply dom_rf_is_write with E x | split]; auto.

  generalize (Hlin (loc x)); clear Hlin; intro Hlin; destruct_lin Hlin.
  assert (Intersection Event E (is_write_same_loc (loc x)) w) as Hw.
    split; [apply dom_rf_in_evts with x| split; [apply dom_rf_is_write with E x|apply rf_implies_same_loc with E]]; auto.
  assert (Intersection Event E (is_write_same_loc (loc x)) y) as Hy.
    split; [apply ran_po_in_evts with x | split]; auto.
  assert (w <> y) as Hdiff.
    intro Heq; rewrite Heq in Hrf; apply Hintv; exists x; apply _trans with y; [right; right; right; split|
                                                          apply _base]; auto.
  generalize (Htot w y Hdiff Hw Hy); intros [? | Hcoyw]; auto.
  assert False as Ht.
    apply Hintv; exists x; apply _trans with y; [right; right; right; split|
                           apply _trans with w; [|apply _base]]; auto.
  inversion Ht.

  destruct Hpoxy as [? ?]; auto.
Qed.

Lemma posWW_is_coi E co w w' :
  co_well_formed E co ->
  internal_visibility E co ->
  is_write w ->
  is_write w' ->
  po E w w' -> 
  loc w = loc w' ->
  co E w w'.
Proof.
intros [? Hlin] Hintv Hisw Hisw' Hpoww' Hlocww'.
destruct (Hlin (loc w)) as [Hpart Htot].
assert (w <> w') as Hd.
  intro Heq; rewrite Heq in Hpoww'.
  destruct Hpoww' as [? Hlt]; generalize Hlt; apply lt_irrefl.
assert (Intersection Event E (is_write_same_loc (loc w)) w) as Hw.
  split; auto; [apply dom_po_in_evts with w'|split]; auto.
assert (Intersection Event E (is_write_same_loc (loc w)) w') as Hw'.
  split; auto; [apply ran_po_in_evts with w|split]; auto.
generalize (Htot w w' Hd Hw Hw'); intros [|Hw'w]; auto.
assert False as Ht.
  apply Hintv; exists w; apply _trans with w'; [right; right; right; split|apply _base]; auto.
inversion Ht.
Qed. 

Lemma rfi_implies_po E co w r :
  internal_visibility E co ->
  rfi E w r ->
  po E w r.
Proof.
intros Hintv [Hrf Hint].
assert (w <> r) as Hdiff.
  intro Heq; rewrite Heq in Hrf; assert False as Ht.
    apply read_write_contrad with r; [apply ran_rf_is_read with E r | apply dom_rf_is_write with E r]; auto.
  inversion Ht.
generalize (internal_implies_po_or_po_minus_1 Hint Hdiff); intros [|Hporw]; auto.
assert False as Ht.
  apply Hintv; exists w; auto; apply _trans with r; [left | apply _base; right; right; right; split]; auto.
    rewrite rf_implies_same_loc with E w r; auto.
inversion Ht.
Qed.

(** * Definitions and lemmas relative to total order based models in general ****)

Definition order_to_co (E : set Event) (o : Rln Event) (e1 e2 : Event) : Prop :=
  E e1 /\ E e2 /\ is_write e1 /\ is_write e2 /\ loc e1 = loc e2 /\ o e1 e2.

Lemma co_in_order E co o x y :
  rel_equal (co E) (order_to_co E o) ->
  co E x y ->
  o x y.
Proof.
intros Hcoeq Hco; destruct Hcoeq as [Hcoincl ?]; generalize (Hcoincl x y Hco); intros [? [? [? [? [? ?]]]]]; auto.
Qed.

Lemma co_order_incl (E : set Event) (co : set Event -> Rln Event) (r o : Rln Event) :
  co_well_formed E co ->
  linearisations r E o ->
  rel_incl (co E) r ->
  rel_incl (co E) (order_to_co E o).
Proof.
intros Hcowf Hlin Hcor; generalize (lin_ext_prop E r o); 
  intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; intros x y Hxy.
split.
  apply dom_co_in_evts with co y; auto.
split.
  apply ran_co_in_evts with co x; auto.
split; [apply dom_co_is_write with E co y | split; [apply ran_co_is_write with E co x|
        split; [apply co_implies_same_loc with E co|apply Hincl; apply Hcor]]]; auto.
Qed.


Lemma co_corder_incl (E : set Event) (co : set Event -> Rln Event) (C : set (Class Event)) (r : Rln Event) (o : Rln (Class Event)) :
  co_well_formed E co ->
  clinearisations (lift C r) C o ->
  rel_incl (co E) (delift C o) ->
  rel_incl (co E) (order_to_co E (delift C o)).
Proof.
intros Hcowf Hlin Hcor; generalize (clin_ext_prop C (lift C r) o); 
  intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; intros x y Hxy.
split.
  apply dom_co_in_evts with co y; auto.
split.
  apply ran_co_in_evts with co x; auto.
split; [apply dom_co_is_write with E co y | split; [apply ran_co_is_write with E co x |
        split; [apply co_implies_same_loc with E co |]]]; auto.
Qed.

Lemma fr_implies_order_or_order_minus_1 (E : set Event) (co : set Event -> Rln Event) (r o : Rln Event) (e1 e2 : Event) :
  co_well_formed E co ->
  linearisations r E o ->
  fr E co e1 e2 ->
  o e1 e2 \/ o e2 e1.
Proof.
intros Hcowf Hlin H12.
generalize (lin_ext_prop E r o); intros [Himpl ?];
generalize (Himpl Hlin); intros [Hincl Hlso]; destruct_lin Hlso.
apply Htot; auto.
  apply fr_implies_diff with E co; auto.
  apply dom_fr_in_evts with co e2; auto.
  apply ran_fr_in_evts with co e1; auto.
Qed. 

Definition equiv_rel (A:Type) (eqr:Rln A) :=
  (forall x, eqr x x) /\
  (forall x y, eqr x y -> eqr y x) /\
  (forall x y z, eqr x y -> eqr y z -> eqr x z).
Ltac destruct_eqrln H := destruct H as [Hrefl [Hsym Htrans]].

Lemma class_of_in_classes (A:Type) (eqr:Rln A) C x :
  equiv_rel eqr ->
  C = classes eqr ->
  C (class_of eqr x).
Proof.
intros Heqr HeqC; rewrite HeqC; exists x; auto.
Qed.

Lemma class_of_refl (A:Type) (eqr:Rln A) x :
  equiv_rel eqr ->
  class_of eqr x x.
Proof.
intros Heqr; destruct_eqrln Heqr; apply Hrefl.
Qed.
      
Lemma ran_of_rfe_si_is_read E si x y z :
  si_well_formed E (si E) ->
  rfe E x y -> 
  si E y z ->
  is_read z.
Proof.
intros Hsiwf [Hrf ?] Hsi.
destruct_siwf Hsiwf; apply Hr with y; auto.
apply ran_rf_is_read with E x; auto.
Qed.

Lemma read_is_ER_or_IR E x :
  rf_well_formed E ->
  is_read x -> 
  ER E x \/ IR E x.
Proof.
intros Hrfwf Hr; destruct_rf_wf Hrfwf;
generalize (Hex_uni x Hr); intros [[w Hrf] ?].
assert (E w) as HEw.
  apply dom_rf_in_evts with x; auto.
assert (E x) as HEx.
  apply ran_rf_in_evts with w; auto.
generalize (int_or_ext E w x HEw HEx); intros [Hint | Hext]; 
  [right | left]; exists w; split; auto.
Qed.

Lemma equiv_elts_have_equal_classes (A:Type) (eqr:Rln A) x y :
  equiv_rel eqr ->
  eqr x y -> 
  class_of eqr x = class_of eqr y.
Proof.
intros Hequivr Hxy; unfold class_of; destruct_eqrln Hequivr;
apply Extensionality_Ensembles; split; intros e He; unfold In in * |- *.
  apply Htrans with x; apply Hsym; auto.
  apply Htrans with y; auto.
Qed.

Lemma obsp_si_in_order E co si r eqr C o x y :
  erln E si = eqr ->
  equiv_rel eqr ->
  C = classes eqr ->
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  clinearisations (lift C r) C o ->
  rel_incl (rfe E) (delift C o) ->
  rel_incl (co E) (delift C o) ->
  rel_incl (fr E co) (delift C o) ->
  rel_incl (scaob E si) r ->
  rel_seq (obsplus E co) (si E) x y ->
  o (class_of eqr x) (class_of eqr y).
Proof.
intros Heqr Hequivr HC Hsiwf Hrfwf Hcowf Hlin Hrfeincl Hcoincl Hfrincl Hscaobincl [e [Hxe Hey]];
generalize (clin_ext_prop C (lift C r) o); intros [Himpl _];
generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; destruct_lin Hlso; destruct_part Hpart;
generalize (class_of_in_classes x Hequivr HC); intro Hinx;
generalize (class_of_in_classes e Hequivr HC); intro Hine;
generalize (class_of_in_classes y Hequivr HC); intro Hiny;
generalize (class_of_refl x Hequivr); intro Hreflx;
generalize (class_of_refl e Hequivr); intro Hrefle;
generalize (class_of_refl y Hequivr); intro Hrefly;
generalize (obsplus_dec Hrfwf Hcowf Hxe); intros [Hrfe | [Hco | [Hfr | [Hcorfe | Hfrrfe]]]].

    (*x -rfe-> e -si- y*)
    generalize (Hrfeincl x e Hrfe); intro Hdrfe;
    generalize (Hdrfe (class_of eqr x) (class_of eqr e) Hinx Hine Hreflx Hrefle);
    intros Hoxe.
    assert (ER E e) as HERe.
      exists x; auto.
    assert (ER E y \/ IR E y) as Hory.
      apply read_is_ER_or_IR; auto; 
      apply ran_of_rfe_si_is_read with E si x e; auto.
    inversion Hory as [HERy | HIRy]; clear Hory.

      assert (class_of eqr e = class_of eqr y) as Hceqey.
        clear Htrans; destruct_siwf Hsiwf.
        apply equiv_elts_have_equal_classes; auto; 
        rewrite <- Heqr; auto; split; auto; 
          [apply Hdom; exists y | split; [apply Hran; exists e|]]; auto.
        
      rewrite <- Hceqey; auto.

      apply Htrans with (class_of eqr e); auto; apply Hincl; auto;
      split; auto; split; auto; exists e; exists y; split; auto; split; auto;
      apply Hscaobincl; split; auto.

    (*x -co-> e -si- y*)
    generalize (Hcoincl x e Hco); intro Hdco;
    generalize (Hdco (class_of eqr x) (class_of eqr e) Hinx Hine Hreflx Hrefle);
    intros Hoxe.

    assert (is_write e /\ is_write y) as Hww.
      generalize (ran_co_is_write x e Hcowf Hco); intro Hwe.
      split; [| clear Htrans; destruct_siwf Hsiwf; apply Hw with e]; auto. 

    assert (class_of eqr e = class_of eqr y) as Hceqey.
      clear Htrans; destruct_siwf Hsiwf.
      apply equiv_elts_have_equal_classes; auto; 
      rewrite <- Heqr; auto; split; auto; 
        [apply Hdom; exists y | split; [apply Hran; exists e|]]; auto.

    rewrite <- Hceqey; auto.

    (*x -fr-> e -si- y*)
    generalize (Hfrincl x e Hfr); intro Hdfr;
    generalize (Hdfr (class_of eqr x) (class_of eqr e) Hinx Hine Hreflx Hrefle);
    intros Hoxe.

    assert (is_write e /\ is_write y) as Hww.
      generalize (ran_fr_is_write Hcowf Hfr); intro Hwe.
      split; [| clear Htrans; destruct_siwf Hsiwf; apply Hw with e]; auto. 

    assert (class_of eqr e = class_of eqr y) as Hceqey.
      clear Htrans; destruct_siwf Hsiwf.
      apply equiv_elts_have_equal_classes; auto; 
      rewrite <- Heqr; auto; split; auto; 
        [apply Hdom; exists y | split; [apply Hran; exists e|]]; auto.

    rewrite <- Hceqey; auto.

    (*x -co;rfe-> e -si- y*)
    destruct Hcorfe as [x' [Hco Hrfe]]; 
    generalize (class_of_in_classes x' Hequivr HC); intro Hinx';
    generalize (class_of_refl x' Hequivr); intro Hreflx';
    apply Htrans with (class_of eqr x').

    generalize (Hcoincl x x' Hco); intro Hdco;
    generalize (Hdco (class_of eqr x) (class_of eqr x') Hinx Hinx' Hreflx Hreflx');
    intros Hoxx'; auto.

    generalize (Hrfeincl x' e Hrfe); intro Hdrfe;
    generalize (Hdrfe (class_of eqr x') (class_of eqr e) Hinx' Hine Hreflx' Hrefle);
    intros Hox'e.

    assert (ER E e) as HERe.
      exists x'; auto.
    assert (ER E y \/ IR E y) as Hory.
      apply read_is_ER_or_IR; auto; 
      apply ran_of_rfe_si_is_read with E si x' e; auto.
    inversion Hory as [HERy | HIRy]; clear Hory.

      assert (class_of eqr e = class_of eqr y) as Hceqey.
        clear Htrans; destruct_siwf Hsiwf.
        apply equiv_elts_have_equal_classes; auto; 
        rewrite <- Heqr; auto; split; auto; 
        [apply Hdom; exists y | split; [apply Hran; exists e|]]; auto.

      rewrite <- Hceqey; auto.

      apply Htrans with (class_of eqr e); auto; apply Hincl; auto;
      split; auto; split; auto; exists e; exists y; split; auto; split; auto;
      apply Hscaobincl; split; auto.

    (*x -fr;rfe-> e -si- y*)
    destruct Hfrrfe as [x' [Hfr Hrfe]]; 
    generalize (class_of_in_classes x' Hequivr HC); intro Hinx';
    generalize (class_of_refl x' Hequivr); intro Hreflx';
    apply Htrans with (class_of eqr x').

    generalize (Hfrincl x x' Hfr); intro Hdfr;
    generalize (Hdfr (class_of eqr x) (class_of eqr x') Hinx Hinx' Hreflx Hreflx');
    intros Hoxx'; auto.

    generalize (Hrfeincl x' e Hrfe); intro Hdrfe;
    generalize (Hdrfe (class_of eqr x') (class_of eqr e) Hinx' Hine Hreflx' Hrefle);
    intros Hox'e.

    assert (ER E e) as HERe.
      exists x'; auto.
    assert (ER E y \/ IR E y) as Hory.
      apply read_is_ER_or_IR; auto; 
      apply ran_of_rfe_si_is_read with E si x' e; auto.
    inversion Hory as [HERy | HIRy]; clear Hory.

      assert (class_of eqr e = class_of eqr y) as Hceqey.
        clear Htrans; destruct_siwf Hsiwf.
        apply equiv_elts_have_equal_classes; auto; 
        rewrite <- Heqr; auto; split; auto; 
        [apply Hdom; exists y | split; [apply Hran; exists e|]]; auto.

      rewrite <- Hceqey; auto.

      apply Htrans with (class_of eqr e); auto; apply Hincl; auto;
      split; auto; split; auto; exists e; exists y; split; auto; split; auto;
      apply Hscaobincl; split; auto.
Qed.

Lemma tc_obsp_si_in_order E co si r eqr C o x y :
  erln E si = eqr ->
  equiv_rel eqr ->
  C = classes eqr ->
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  clinearisations (lift C r) C o ->
  rel_incl (rfe E) (delift C o) ->
  rel_incl (co E) (delift C o) ->
  rel_incl (fr E co) (delift C o) ->
  rel_incl (scaob E si) r ->
  transitive_closure (rel_seq (obsplus E co) (si E)) x y ->
  o (class_of eqr x) (class_of eqr y).
Proof.
intros Heqr Hequivr HC Hsiwf Hrfwf Hcowf Hlin Hrfeincl Hcoincl Hfrincl Hscaobincl Htc; auto.
induction Htc as [x e Hb | x y e Hb]; 
generalize (obsp_si_in_order Heqr Hequivr HC Hsiwf Hrfwf Hcowf Hlin Hrfeincl Hcoincl Hfrincl Hscaobincl Hb); auto;
intros Hoxe; generalize (clin_ext_prop C (lift C r) o); intros [Himpl _];
generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; destruct_lin Hlso; destruct_part Hpart;
apply Htrans with (class_of eqr e); auto; rewrite <- Heqr; auto.
Qed.

Lemma mop_in_order E co si r eqr C o x y :
  erln E si = eqr ->
  equiv_rel eqr ->
  C = classes eqr ->
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  clinearisations (lift C r) C o ->
  rel_incl (rfe E) (delift C o) ->
  rel_incl (co E) (delift C o) ->
  rel_incl (fr E co) (delift C o) ->
  rel_incl (scaob E si) r ->
  maybe (transitive_closure (rel_seq (obsplus E co) (si E))) x y ->
  x = y \/ o (class_of eqr x) (class_of eqr y).
Proof.
intros Heqr Hequivr HC Hsiwf Hrfwf Hcowf Hlin Hrfeincl Hcoincl Hfrincl Hscaobincl [Heq | Htc]; auto.
right; apply tc_obsp_si_in_order with E co si r C; auto.
Qed.

Definition mobs_r_mobs E co si r := rel_seq (maybe (transitive_closure (rel_seq (obsplus E co) (si E)))) 
                                (rel_seq r (maybe (transitive_closure (rel_seq (obsplus E co) (si E))))).
Definition tc_mobs_r_mobs E co si r := transitive_closure (mobs_r_mobs E co si r).

Lemma r_to_lift (A:Type) (C:set (Class A)) (r:Rln A) eqr e1 e2 :
  equiv_rel eqr ->
  C (class_of eqr e1) ->
  C (class_of eqr e2) ->
  r e1 e2 ->
  lift C r (class_of eqr e1) (class_of eqr e2).
Proof.
intros Hequivr H1 H2 H12.
generalize (class_of_refl e1 Hequivr); intro Hre1;
generalize (class_of_refl e2 Hequivr); intro Hre2.
split; auto; split; auto; exists e1; exists e2; split; auto. 
Qed.

Lemma mobs_r_mobs_in_order E co si r eqr C o e1 e2 :
  erln E si = eqr ->
  equiv_rel eqr ->
  C = classes eqr ->
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  clinearisations (lift C r) C o ->
  rel_incl (rfe E) (delift C o) ->
  rel_incl (co E) (delift C o) ->
  rel_incl (fr E co) (delift C o) ->
  rel_incl (scaob E si) r ->
  (mobs_r_mobs E co si r) e1 e2 ->
  o (class_of eqr e1) (class_of eqr e2).
Proof.
intros Heqr Hequivr HC Hsiwf Hrfwf Hcowf Hlin Hrfincl Hcoincl Hfrincl Hscaobincl [e [H1e [e' [Hee' He'2]]]];
generalize (class_of_in_classes e Hequivr HC); intros HCe;
generalize (class_of_in_classes e' Hequivr HC); intros HCe';
generalize (clin_ext_prop C (lift C r) o); intros [Himpl _];
generalize (Himpl Hlin); intros [Hincl Hlso];
destruct_lin Hlso; destruct_part Hpart;
generalize (mop_in_order Heqr Hequivr HC Hsiwf Hrfwf Hcowf Hlin Hrfincl Hcoincl Hfrincl Hscaobincl H1e); 
generalize (mop_in_order Heqr Hequivr HC Hsiwf Hrfwf Hcowf Hlin Hrfincl Hcoincl Hfrincl Hscaobincl He'2);
intros [Heqe'2 | Hgcbe'2]; intros [Heqe1 | Hgcbe1]. 

  rewrite Heqe1; rewrite <- Heqe'2; auto;
  apply Hincl; apply r_to_lift; auto.

  apply Htrans with (class_of eqr e); auto;
  rewrite <- Heqe'2; auto;
  apply Hincl; apply r_to_lift; auto.

  apply Htrans with (class_of eqr e'); auto;
  rewrite Heqe1; auto;
  apply Hincl; apply r_to_lift; auto.

  apply Htrans with (class_of eqr e); auto; 
  apply Htrans with (class_of eqr e'); auto;
  apply Hincl; apply r_to_lift; auto.
Qed.

Lemma tc_mobs_r_mobs_in_order E co si r eqr C o x y :
  erln E si = eqr ->
  equiv_rel eqr ->
  C = classes eqr ->
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  clinearisations (lift C r) C o ->
  rel_incl (rfe E) (delift C o) ->
  rel_incl (co E) (delift C o) ->
  rel_incl (fr E co) (delift C o) ->
  rel_incl (scaob E si) r ->
  tc_mobs_r_mobs E co si r x y ->
  o (class_of eqr x) (class_of eqr y).
Proof.
intros Heqr Hequivr HC Hsiwf Hrfwf Hcowf Hlin Hrfincl Hcoincl Hfrincl Hscaobincl Hxy.
induction Hxy.
  apply mobs_r_mobs_in_order with E co si r C; auto.

  generalize (clin_ext_prop C (lift C r) o); intros [Himpl _];
  generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; destruct_lin Hlso; destruct_part Hpart;
  apply Htrans with (class_of eqr e); auto.
    apply mobs_r_mobs_in_order with E co si r C; auto.
Qed.

Lemma tc_mobs_r_mobs_irr E co si r eqr C o x :
  erln E si = eqr ->
  equiv_rel eqr ->
  C = classes eqr ->
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  clinearisations (lift C r) C o ->
  rel_incl (rfe E) (delift C o) ->
  rel_incl (co E) (delift C o) ->
  rel_incl (fr E co) (delift C o) ->
  rel_incl (scaob E si) r ->
  ~(tc_mobs_r_mobs E co si r x x).
Proof.
intros Heqr Hequivr HC Hsiwf Hrfwf Hcowf Hlin Hrfeincl Hcoincl Hfrincl Hscaobincl Hxx; 
generalize (clin_ext_prop C (lift C r) o); intros [Himpl _];
generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; destruct_lin Hlso; destruct_part Hpart.
apply Hirr with (class_of eqr x); apply tc_mobs_r_mobs_in_order with E co si r C; auto.
Qed.

(** * External Global Completion *)

(** ** External Global Completion: Definitions *)
Definition intervening_write (r : Rln Event) (e1 e2 : Event) : Prop :=
  exists w, is_write w /\ r e1 w /\ r w e2.

Definition gcb_rf (E : set Event) (gcb : Rln Event) (e1 e2 : Event) : Prop :=
  E e1 /\  E e2 /\ is_write e1 /\ is_read e2 /\ loc e1 = loc e2 /\ val e1 = val e2 /\
  gcb e1 e2 /\
  ~intervening_write (fun e1 e2 => gcb e1 e2 /\ loc e1 = loc e2) e1 e2.
Definition gcb_rf_wf (E:set Event) (gcb : Rln Event) : Prop :=
  forall r, is_read r -> exists w, gcb_rf E gcb w r.

Definition gcb_co (E : set Event) (gcb : Rln Event) (e1 e2 : Event) : Prop :=
  order_to_co E gcb e1 e2.

Definition does_not_locally_reads_from E r1 :=
  ~(exists w1, rfi E w1 r1).

Definition locally_reads_from_a_lob_write (E : set Event) (lob : set Event -> Rln Event) r1 e2 :=
  exists w1, rfi E w1 r1 /\ lob E w1 e2.

Definition read_requirements (E : set Event) (lob : set Event -> Rln Event) r1 e2 :=
  does_not_locally_reads_from E r1 \/ locally_reads_from_a_lob_write E lob r1 e2.

Definition lob' (E : set Event) (lob : set Event -> Rln Event) (e1 e2 : Event) :=
  lob E e1 e2 /\ (is_read e1 /\ ~(read_requirements E lob e1 e2)).

Definition preorder_gcb (E : set Event) (si lob : set Event -> Rln Event) (e1 e2 : Event) : Prop :=
  (lob E e1 e2 /\
  (is_write e1 \/
   (is_read e1 /\ read_requirements E lob e1 e2))) \/
  (scaob E si e1 e2).
Definition preorder_gcb_lift E si lob C :=
  lift C (preorder_gcb E si lob). 

Definition dgcb_loc E C gcb := 
  fun e1 => fun e2 => E e1 /\ E e2 /\ delift C gcb e1 e2 /\ loc e1 = loc e2. 
Definition external_global_completion (E : set Event) (co si lob: set Event -> Rln Event) (gcb : Rln (Class Event)): Prop :=
  (clinearisations (preorder_gcb_lift E si lob (MemC E si)) (MemC E si)) gcb /\
  rel_equal (rf E) (gcb_rf E (dgcb_loc E (MemC E si) gcb)) /\ rel_equal (co E) (gcb_co E (dgcb_loc E (MemC E si) gcb)).
  
(** ** External Global Completion: Auxiliary definitions, for convenience in the proofs below *)

Definition big_rel E co si lob := tc_mobs_r_mobs E co si (preorder_gcb E si lob).

(** ** External Global Completion: Lemmas that do _not_ need the existence of a External Global Completion order *)

Lemma lob'_irr E si lob x :
  lob_well_formed E si lob ->
  ~(lob' E lob x x).
Proof.
intros Hlobwf [Hxx ?]; destruct_lob_wf Hlobwf.
apply Hirr_lob; exists x; auto.
Qed. 

Lemma lob_implies_pgcb_or_lob' (E : set Event) (si lob : set Event -> Rln Event) (e1 e2 : Event) :
  lob_well_formed E si lob ->
  lob E e1 e2 ->
  preorder_gcb E si lob e1 e2 \/ lob' E lob e1 e2.
Proof.
intros Hlobwf H12; destruct_lob_wf Hlobwf; generalize (Hdom_lob  e1 e2 H12); intros [Hw1 | Hr1].
  left; left; split; auto.
  generalize (excluded_middle (read_requirements E lob e1 e2)); intros [Hrr | Hnrr];
  [left; left | right]; split; auto.
Qed.

Lemma pgcb_in_big_rel E co si lob e1 e2 :
  preorder_gcb E si lob e1 e2 ->
  big_rel E co si lob e1 e2.
Proof.
intros Hpgcb; apply _base; exists e1; split; [left; auto | exists e2; split; [|left]; auto].
Qed.

Lemma lob'_in_lob'_seq_big_rel E co si lob e1 e2 :
  lob' E lob e1 e2 ->
  rel_seq (lob' E lob) (maybe (big_rel E co si lob)) e1 e2.
Proof.
intros H12; exists e2; split; auto; left; auto. 
Qed.

Lemma front_seq (r1 r2 : Rln Event) e1 e2 e3 :
  transitive r1 ->
  r1 e1 e2 ->
  transitive_closure (rel_seq r1 (rel_seq r2 r1)) e2 e3 ->
  transitive_closure (rel_seq r1 (rel_seq r2 r1)) e1 e3.
Proof.
intros Htr1 H12 H23; induction H23 as [e e2 [e' [Hee' He'2]] | e e2 e' He2].
  apply _base; exists e'; split; auto.
    generalize Hee'; generalize H12; apply Htr1.
  apply _trans with e'; auto.
    destruct He2 as [e0 [He0 H0']]; exists e0; split; auto.
    generalize He0; generalize H12; apply Htr1.
Qed.

Lemma maybe_tc_trans r :
  transitive (maybe (transitive_closure r)).
Proof.
intros e1 e2 e3 Hor12 Hor23.
  inversion Hor12 as [Heq12 | H12]; clear Hor12.
    rewrite Heq12; auto.
    inversion Hor23 as [Heq23 | H23]; clear Hor23.
      rewrite <- Heq23; right; auto.
      right; apply tc_trans with e2; auto.
Qed.

Lemma mop_br_in_br E co si lob e1 e2 e3 :
  maybe (transitive_closure (rel_seq (obsplus E co) (si E))) e1 e2 ->
  big_rel E co si lob e2 e3 ->
  big_rel E co si lob e1 e3.
Proof.
unfold big_rel; unfold tc_mobs_r_mobs; unfold mobs_r_mobs.
apply front_seq; apply maybe_tc_trans.
Qed.

Lemma ER_lob'_contrad E lob e1 e2 :
  rf_well_formed E ->
  ER E e1 -> lob' E lob e1 e2 ->
  False.
Proof.
intros Hrfwf [w1 [Hrfw1e1 Hext]] [Hlob [Hr1 Hnrr]]; apply Hnrr; left; intros [w [Hrfw1 Hint]].
destruct Hrfwf as [? Hr]; generalize (Hr e1 Hr1); intros [? Huni]; generalize (Huni w1 w Hrfw1e1 Hrfw1); 
intro Heq; rewrite Heq in Hext; apply int_ext_contrad with E w e1; auto.
Qed.

Lemma W_lob'_contrad E lob e1 e2 :
  is_write e1 -> lob' E lob e1 e2 ->
  False.
Proof.
intros Hw1 [Hlob [Hr1 Hnrr]]; apply read_write_contrad with e1; auto.
Qed.

Lemma tc_dec r e1 e3 :
  transitive_closure r e1 e3 ->
  exists e2, maybe (transitive_closure r) e1 e2 /\ r e2 e3.
Proof.
intros H13; induction H13.
  exists e1; split; auto; left; auto.
  destruct IHtransitive_closure as [e' [Hee' He'2]]; exists e'; split; auto.
    inversion Hee' as [Heq | Htc].
      rewrite Heq in H; right; apply _base; auto.
      right; apply _trans with e; auto.
Qed.

Lemma obsp_si_lob' E co si lob e1 e2 e3 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  rel_seq (obsplus E co) (si E) e1 e2 ->
  lob' E lob e2 e3 -> 
  rel_seq (rel_seq (maybe (rel_union (co E) (fr E co))) (rfe E))
    (rel_seq (scaob E si) (lob' E lob)) e1 e3.
Proof.
intros Hsiwf Hrfwf Hcowf [e [H1e He2]] H23.
generalize (obsplus_dec Hrfwf Hcowf H1e); clear H1e; 
intros [Hrfe1e | [Hco1e | [Hfr1e | [Hcorfe1e | Hfrrfe1e]]]].

  assert (ER E e) as HERe.
    exists e1; auto.
  assert (is_read e2) as Hr2.
    destruct_siwf Hsiwf; apply Hr with e; auto; apply ran_rf_is_read with E e1; destruct Hrfe1e; auto.
  exists e; split; [exists e1; split; [left|] |]; auto; exists e2; split; auto; split; auto; split; auto.
    generalize (read_is_ER_or_IR e2 Hrfwf Hr2); intros [HER2 | HIR2]; auto.
    generalize (ER_lob'_contrad Hrfwf HER2 H23); intro Ht; inversion Ht.

  assert (is_write e2) as Hw2.
    destruct_siwf Hsiwf; apply Hw with e; auto; apply ran_co_is_write with E co e1; auto.
  generalize (W_lob'_contrad Hw2 H23); intro Ht; inversion Ht.

  assert (is_write e2) as Hw2.
    destruct_siwf Hsiwf; apply Hw with e; auto; apply ran_fr_is_write with E co e1; auto.
  generalize (W_lob'_contrad Hw2 H23); intro Ht; inversion Ht.

  destruct Hcorfe1e as [x [? Hrfe]]. 
  assert (ER E e) as HERe.
    exists x; auto.
  assert (is_read e2) as Hr2.
    destruct_siwf Hsiwf; apply Hr with e; auto; apply ran_rf_is_read with E x; destruct Hrfe; auto.
  exists e; split; [exists x; split; [right; left|] |]; auto; exists e2; split; auto; split; auto; split; auto.
    generalize (read_is_ER_or_IR e2 Hrfwf Hr2); intros [HER2 | HIR2]; auto.
  generalize (ER_lob'_contrad Hrfwf HER2 H23); auto; intro Ht; inversion Ht.

  destruct Hfrrfe1e as [x [? Hrfe]]. 
  assert (ER E e) as HERe.
    exists x; auto.
  assert (is_read e2) as Hr2.
    destruct_siwf Hsiwf; apply Hr with e; auto; apply ran_rf_is_read with E x; destruct Hrfe; auto.
  exists e; split; [exists x; split; [right; right|] |]; auto; exists e2; split; auto; split; auto; split; auto.
    generalize (read_is_ER_or_IR e2 Hrfwf Hr2); intros [HER2 | HIR2]; auto.
  generalize (ER_lob'_contrad Hrfwf HER2 H23); auto; intro Ht; inversion Ht.
Qed.

Lemma pre_mop_lob'_in_lob'_or_br E co si lob e1 e2 e3 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  maybe (transitive_closure (rel_seq (obsplus E co) (si E))) e1 e2 ->
  lob' E lob e2 e3 ->
  lob' E lob e1 e3 \/ 
  rel_seq (maybe (transitive_closure (rel_seq (obsplus E co) (si E)))) 
          (rel_seq (rel_seq (maybe (rel_union (co E) (fr E co))) (rfe E)) (rel_seq (scaob E si) (lob' E lob))) e1 e3.
Proof.
intros Hsiwf Hrfwf Hcowf [Heq12 | H12] H23; [rewrite Heq12|]; auto.
generalize (tc_dec H12); intros [e [H1e He2]]; right; exists e; split; auto.
  apply obsp_si_lob' with e2; auto.
Qed.

Lemma mscaob_lob'_is_lob_or_lob' E si lob e1 e2 :
  lob_well_formed E si lob ->
  rel_seq (maybe (scaob E si)) (lob' E lob) e1 e2 ->
  lob E e1 e2 \/ lob' E lob e1 e2.
Proof.
intros Hlobwf [e [Hm Hlob']]; inversion Hm as [Heq | Hsi].
  rewrite <- Heq in Hlob'; right; auto.
  destruct_lob_wf Hlobwf; destruct Hlob' as [Hlob ?].
    left; apply Hscaob_lob; exists e; auto.
Qed.

Lemma scaob_lob'_is_pgcb_or_lob' E si lob e1 e2 :
  rf_well_formed E ->
  lob_well_formed E si lob ->
  rel_seq (scaob E si) (lob' E lob) e1 e2 ->
  preorder_gcb E si lob e1 e2.
Proof.
intros Hrfwf Hlobwf [e [Hsi Hlob']]; 
  unfold preorder_gcb; left; split; auto.
    destruct_lob_wf Hlobwf; destruct Hlob' as [Hlob ?].
    apply Hscaob_lob; exists e; auto.

  right; destruct Hsi as [Hsi [[w1 [Hrf Hext]] HIR]].
    assert (is_read e1) as Hr1.
      apply ran_rf_is_read with E w1; auto.
    split; auto.
    left; intros [w2 [Hrf2 Hint]].
      destruct_rf_wf Hrfwf; generalize (Hex_uni e1 Hr1); intros [? Huni].
      generalize (Huni w1 w2 Hrf Hrf2); intro Heq.
      rewrite <- Heq in Hint; apply (int_ext_contrad Hint Hext).
Qed.

Lemma mscaob_lob'_is_pgcb_or_lob' E si lob e1 e2 :
  rf_well_formed E ->
  lob_well_formed E si lob ->
  rel_seq (maybe (scaob E si)) (lob' E lob) e1 e2 ->
  preorder_gcb E si lob e1 e2 \/ lob' E lob e1 e2.
Proof.
intros Hrfwf Hlobwf [e [Hm Hlob']]; inversion Hm as [Heq | Hsi].
  rewrite <- Heq in Hlob'; right; auto.
  left; apply scaob_lob'_is_pgcb_or_lob'; auto; exists e; auto.
Qed.

Lemma mop_lob'_in_lob'_or_br E co si lob e1 e2 e3 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  maybe (transitive_closure (rel_seq (obsplus E co) (si E))) e1 e2 ->
  lob' E lob e2 e3 ->
  lob' E lob e1 e3 \/ big_rel E co si lob e1 e3.
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf H12 H23; generalize (pre_mop_lob'_in_lob'_or_br si Hsiwf Hrfwf Hcowf H12 H23); intros [Hlob' | Hseq]; [left |]; auto.
destruct Hseq as [a [H1a [b [Hab Hb3]]]].
generalize (scaob_lob'_is_pgcb_or_lob' Hrfwf Hlobwf Hb3); intros Hpgcb.
right; unfold big_rel. unfold tc_mobs_r_mobs. unfold mobs_r_mobs.

  apply _base; exists b; split. 
  inversion H1a as [Heq1a | Hs1a].
    rewrite Heq1a; right; apply _base; exists b; split; auto.
      destruct Hab as [x [[Heqax | Hax] Hxb]].
        rewrite Heqax; apply _base; left; auto.
        apply _trans with x; auto. 
          inversion Hax as [Hco | Hfr]; right; [left | right]; auto.
          apply _base; left; auto.
      destruct_siwf Hsiwf; apply Hrefl; auto.

  right; apply tc_trans with a; auto.
    apply _base; exists b; split.
      destruct Hab as [x [[Heqax | Hax] Hxb]].
        rewrite Heqax; apply _base; left; auto.
        apply _trans with x; auto. 
          inversion Hax as [Hco | Hfr]; right; [left | right]; auto.
          apply _base; left; auto.
      destruct_siwf Hsiwf; apply Hrefl; auto.

  exists e3; split; [|left]; auto.
Qed.

Lemma br_trans E co si lob e1 e2 e3 :
  big_rel E co si lob e1 e2 ->
  big_rel E co si lob e2 e3 ->
  big_rel E co si lob e1 e3.
Proof.
intros H12 H23.
apply tc_trans with e2; auto.
Qed. 

Lemma br_mop_in_br E co si lob e1 e2 e3 :
  big_rel E co si lob e1 e2 ->
  maybe (transitive_closure (rel_seq (obsplus E co) (si E))) e2 e3 ->
  big_rel E co si lob e1 e3.
Proof.
intros Hbr12 Hmop23; induction Hbr12 as [e1 e2 Hb | e1 e2 e H1e].

destruct Hb as [e [He1e [e'[Hee' He'2]]]]; 
apply _base; exists e; split; auto; exists e'; split; auto. 
  generalize Hmop23; generalize He'2; apply maybe_tc_trans. 

apply br_trans with e; auto; apply _base; auto.
Qed.

Lemma lob'_seq_lob'_in_pgcb_or_lob' E si lob e1 e2 e3:
  lob_well_formed E si lob ->
  lob' E lob e1 e2 ->
  lob' E lob e2 e3 ->
  (preorder_gcb E si lob) e1 e3 \/ lob' E lob e1 e3.
Proof.
intros Hlobwf [Hlob12 [Hw1 Hrr1]] [Hlob23 [Hw2 Hrr2]]; destruct_lob_wf Hlobwf. 
assert (lob E e1 e3) as H13.
  apply (Htrans_lob e1 e2 e3 Hlob12 Hlob23).
generalize (excluded_middle (read_requirements E lob e1 e3)); intros [Hrr | Hnrr].
  left; left; split; auto.
  right; split; auto.
Qed.

Lemma pgcb_lob'_in_pgcb_or_lob' E si lob e1 e2 e3 :
  lob_well_formed E si lob ->
  (preorder_gcb E si lob) e1 e2 ->
  lob' E lob e2 e3 ->
  rel_seq (maybe (scaob E si)) (preorder_gcb E si lob) e1 e3 \/ 
  rel_seq (maybe (scaob E si)) (lob' E lob) e1 e3.
Proof.
intros Hlobwf Hpgcb H23; generalize Hlobwf; intro Hlobwf'; destruct_lob_wf Hlobwf'.
inversion Hpgcb as [Hpgcb12 | Hsirr].
  left; exists e1; split; [left | left]; auto; destruct Hpgcb12 as [H12 Hor12]; destruct H23 as [H23 [Hr2 Hnrr]].
  assert (lob E e1 e3) as Hlob13.
    apply (Htrans_lob e1 e2 e3 H12 H23).
  destruct Hor12 as [Hw1|[Hr1 Hrr]]; split; auto.
  right; split; auto.
  inversion Hrr as [Hext | Hint]; [left | right]; auto.
    destruct Hint as [w1 [Hrfi Hlob]]; exists w1; split; auto.
    apply (Htrans_lob w1 e2 e3 Hlob H23).
  right; exists e2; split; auto; right; auto.
Qed.

Lemma br_lob'_in_br_or_lob' E co si lob e1 e2 e3 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  big_rel E co si lob e1 e2 ->
  lob' E lob e2 e3 ->
  big_rel E co si lob e1 e3 \/ lob' E lob e1 e3.
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf H12 H23; induction H12 as [e1 e2 [e [H1e [e' [Hee' He'2]]]] | e1 e2 e H1e He2].

  generalize (mop_lob'_in_lob'_or_br Hsiwf Hrfwf Hcowf Hlobwf He'2 H23); clear He'2 H23; intros [He'3 | He'3].

  (*e',e3 in lob'*)
  generalize (pgcb_lob'_in_pgcb_or_lob' Hlobwf Hee' He'3); intros [Hpgcbe3 | Hlob'e3].
    destruct Hpgcbe3 as [e0 [Hee0 H03]]; inversion Hee0 as [Heqee0 | Hsee0]; clear Hee0; left. 
      rewrite Heqee0 in H1e; apply _base; exists e0; split; auto; exists e3; split; auto; left; auto.
      apply tc_trans with e0; apply _base.
        exists e; split; auto; exists e0; split; [|left]; auto; right; auto.
        exists e0; split; [left|exists e3; split; [|left]]; auto.

   generalize (mscaob_lob'_is_pgcb_or_lob' Hrfwf Hlobwf Hlob'e3); intros [Hpgcb | Hlob'].
     left; apply _base; exists e; split; auto; exists e3; split; auto; left; auto.
     generalize (mop_lob'_in_lob'_or_br Hsiwf Hrfwf Hcowf Hlobwf H1e Hlob'); intros [? |?]; auto.

  (*e',e3 in br*)
  left; apply _trans with e'; auto; exists e; split; auto; exists e'; split; [|left]; auto.

  (*inductive case*)
  generalize (IHHe2 H23); intros [Hbre3 | Hlob'e3].
    left; apply br_trans with e; auto; apply _base; auto.
    clear IHHe2 H23 He2 e2.
    destruct H1e as [x [H1x [y [Hxy Hye]]]].
    generalize (mop_lob'_in_lob'_or_br Hsiwf Hrfwf Hcowf Hlobwf Hye Hlob'e3); clear Hye Hlob'e3 e; intros [Hy3 | Hy3].

    (*y,e3 in lob'*)
    generalize (pgcb_lob'_in_pgcb_or_lob' Hlobwf Hxy Hy3); intros [Hpgcbx3 | Hlob'x3].
    destruct Hpgcbx3 as [e0 [Hee0 H03]]; inversion Hee0 as [Heqee0 | Hsee0]; clear Hee0; left. 
      rewrite Heqee0 in H1x; apply _base; exists e0; split; auto; exists e3; split; auto; left; auto.
      apply tc_trans with e0; apply _base.
        exists x; split; auto; exists e0; split; [|left]; auto; right; auto.
        exists e0; split; [left|exists e3; split; [|left]]; auto.

   generalize (mscaob_lob'_is_pgcb_or_lob' Hrfwf Hlobwf Hlob'x3); intros [Hpgcb | Hlob'].
     left; apply _base; exists x; split; auto; exists e3; split; auto; left; auto.
     generalize (mop_lob'_in_lob'_or_br Hsiwf Hrfwf Hcowf Hlobwf H1x Hlob'); intros [? |?]; auto.

    (*y,e3 in br*)
    left; apply _trans with y; auto; exists x; split; auto; exists y; split; [|left]; auto.
Qed. 
          
Lemma pgcb_mbr_in_br E co si lob e1 e2 e3 :
  (preorder_gcb E si lob) e1 e2 ->
  maybe (big_rel E co si lob) e2 e3 ->
  big_rel E co si lob e1 e3.
Proof.
intros H12 [Heq23 | H23].
  rewrite <- Heq23; apply _base; exists e1; split; [left | exists e2; split; [ | left]]; auto.
  induction H23.
    apply br_trans with e0; apply _base; auto.
      exists e1; split; [left | exists e0; split; [|left]]; auto.

    apply br_trans with e; auto; apply br_trans with e0; apply _base; auto.
      exists e1; split; [left | exists e0; split; [|left]]; auto.
Qed.

Lemma gcb_path_ob_dec (E : set Event) (co si lob : set Event -> Rln Event) (e1 e2 : Event) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  ob E co (si E) (lob E) e1 e2 ->
  transitive_closure (rel_seq (obsplus E co) (si E)) e1 e2 \/ 
  big_rel E co si lob e1 e2 \/ rel_seq (lob' E lob) (maybe (big_rel E co si lob)) e1 e2 
  \/ rel_seq (lob' E lob) (maybe (transitive_closure (rel_seq (obsplus E co) (si E)))) e1 e2.
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf H12.
induction H12 as [e1 e3 [e2 [Hobs Hsi]] | e1 e2 Hlob |]; auto.
  (*e1,e2 in obs*)
  left; apply _base; exists e2; split; auto; apply _base; auto.

  (*e1,e2 in lob*)
  generalize (lob_implies_pgcb_or_lob' e1 e2 Hlobwf Hlob); intros [Hpgcb | Hlob']; right;
    [left; apply pgcb_in_big_rel | right; left; apply lob'_in_lob'_seq_big_rel]; auto.

  (*inductive case*)
  clear H12_ H12_0; inversion IHob1 as [Hmop1e | [Hbr1e | [Hlob'1e | Hlob'1e]]]; clear IHob1.

    (*e1,e in obs+*)
    inversion IHob2 as [Hmop2 | [Hbre2 | Hlob'e2]]; clear IHob2.

      (*e,e2 in obs+*)
      left; apply tc_trans with e; auto.

      (*e,e2 in br*)
      right; left; apply mop_br_in_br with e; auto; right; auto. 

      (*e,e2 in lob';mbr U lob';mop*)
      assert (maybe (transitive_closure (rel_seq (obsplus E co) (si E))) e1 e) as Hmmop1e.
        right; auto.
      inversion Hlob'e2 as [Hee2 | Hee2]; clear Hlob'e2; destruct Hee2 as [e' [Hlob'ee' Hbre'2]];
      generalize (mop_lob'_in_lob'_or_br Hsiwf Hrfwf Hcowf Hlobwf Hmmop1e Hlob'ee'); intros [Hlob'1' | Hbr1']; right.
        right; left; exists e'; split; auto.
        left; inversion Hbre'2 as [Heq | Hbr]; [rewrite <- Heq | apply br_trans with e']; auto. 
        right; right; exists e'; split; auto.
        left; inversion Hbre'2 as [Heq | Hbr]; 
          [rewrite <- Heq | apply br_mop_in_br with e'; auto; right; apply _base; auto]; auto.

    (*e1,e in br*)
    inversion IHob2 as [Hmop2 | [Hbre2 | Hlob'e2]]; clear IHob2.

      (*e,e2 in op*)
      right; left; apply br_mop_in_br with e; auto; right; auto.

      (*e,e2 in br*)
      right; left; apply br_trans with e; auto.

      (*e,e2 in lob';mbr U lob';mop*)
      inversion Hlob'e2 as [Hee2 | Hee2]; clear Hlob'e2; destruct Hee2 as [e' [Hee' He'2]]; 
      generalize (br_lob'_in_br_or_lob' Hsiwf Hrfwf Hcowf Hlobwf Hbr1e Hee'); 
      clear Hbr1e Hee' e; intros [Hbr1' | Hlob1']; right. 

        left; inversion He'2 as [Heqe'2 | Hbre'2]; clear He'2; [rewrite Heqe'2 in Hbr1'|apply br_trans with e']; auto.
        right; left; exists e'; auto.
        left; apply br_mop_in_br with e'; auto. 
        right; right; exists e'; split; auto.

    (*e1,e in lob';mbr*)
    destruct Hlob'1e as [e' [H1e' He'e]]; inversion IHob2 as [Hmop2 | [Hbre2 | Hlob'e2]]; clear IHob2.

      (*e,e2 in op*)
      inversion He'e as [Heqe'e | Hbre'e]; clear He'e; right; right;
        [right; rewrite Heqe'e in H1e'; exists e; split |
        left; exists e'; split; auto; right; apply br_mop_in_br with e]; auto; right; auto.
      
      (*e,e2 in br*)
      right; right; left; exists e'; split; auto;
      inversion He'e as [Heqe'e | Hbre'e]; clear He'e; auto; right; [rewrite Heqe'e | apply br_trans with e]; auto.

      (*e,e2 in lob;mbr U lob;mop*)
      inversion Hlob'e2 as [He2 | He2]; clear Hlob'e2; destruct He2 as [e'' [Hee'' He''2]];
      inversion He'e as [Heqe'e | Hbre'e]; clear He'e.

         rewrite Heqe'e in H1e'; generalize (lob'_seq_lob'_in_pgcb_or_lob' Hlobwf H1e' Hee''); clear e Heqe'e H1e' Hee''; 
         intros [Hpgcb1'' | Hlob'1'']; right; [left; apply pgcb_mbr_in_br with e''; auto | right; left; exists e''; auto].

        generalize (br_lob'_in_br_or_lob' Hsiwf Hrfwf Hcowf Hlobwf Hbre'e Hee''); clear Hbre'e Hee'' e; intros [Hbre'e'' | Hlobe'e''].
          right; right; left; exists e'; split; auto.
        inversion He''2 as [Heq''2 | Hbre''2]; clear He''2.
          rewrite Heq''2 in Hbre'e''; right; auto.
        right; apply br_trans with e''; auto.

          generalize (lob'_seq_lob'_in_pgcb_or_lob' Hlobwf H1e' Hlobe'e''); clear e' H1e' Hlobe'e''; intros [Hmpgcb1'' | Hlob'1''].
            right; left; apply pgcb_mbr_in_br with e''; auto.
            right; right; left; exists e''; split; auto.

      (*e,e2 in lob;mop*)

         rewrite Heqe'e in H1e'; generalize (lob'_seq_lob'_in_pgcb_or_lob' Hlobwf H1e' Hee''); clear e Heqe'e H1e' Hee''; 
         intros [Hpgcb1'' | Hlob'1'']; right; [left; apply _base; exists e1; split; [left | exists e''; split]; auto |
                                             right; right; exists e''; split; auto].

         generalize (br_lob'_in_br_or_lob' Hsiwf Hrfwf Hcowf Hlobwf Hbre'e Hee''); clear Hbre'e Hee'' e; intros [Hbre'e'' | Hlob'e'e''].
           right; right; left; exists e'; split; auto; right; apply br_mop_in_br with e''; auto.

         generalize (lob'_seq_lob'_in_pgcb_or_lob' Hlobwf H1e' Hlob'e'e''); intros [Hpgcb | Hlob]; clear H1e' Hlob'e'e''.
           right; left; apply _base; exists e1; split; [left|exists e'']; auto.
           right; right; right; exists e''; split; auto.

    (*e,1e in lob';mop*)
    inversion IHob2 as [Hmop2 | [Hbre2 | Hlob'e2]]; clear IHob2; destruct Hlob'1e as [e' [H1' H'e]].

      (*e,e2 in obs+*)
      right; right; right; exists e'; split; auto.
        inversion H'e as [Heq'e | Hop'e]; clear H'e; [rewrite Heq'e; right|right; generalize Hmop2; generalize Hop'e; apply tc_trans]; auto.

      (*e,e2 in br*)
      right; right; left; exists e'; split; auto.
        right; apply mop_br_in_br with e; auto.
      
      (*e,e2 in lob';mbr U lob';mop*)
      inversion Hlob'e2 as [He2 | He2]; destruct He2 as [e'' [He'' H''2]];
      generalize (mop_lob'_in_lob'_or_br Hsiwf Hrfwf Hcowf Hlobwf H'e He''); clear H'e He''; intros [Hlob | Hbr].
    
       (*e',e'' in lob'*)
        generalize (lob'_seq_lob'_in_pgcb_or_lob' Hlobwf H1' Hlob); clear H1' Hlob; intros [Hpgcb | Hlob]; right.

            (*e1,e'' in pgcb*)
            left; apply pgcb_mbr_in_br with e''; auto.
            (*e1,e'' in lob'*)
            right; left; exists e''; auto.

       (*e',e'' in br*)
            right; right; left; exists e'; split; auto; inversion H''2 as [Heq | Hbr''2]; clear H''2;
              [rewrite Heq in Hbr; right | right; apply br_trans with e'']; auto.

            
       (*e',e'' in lob*)
        generalize (lob'_seq_lob'_in_pgcb_or_lob' Hlobwf H1' Hlob); clear H1' Hlob; intros [Hpgcb | Hlob]; right.

            (*e1,e'' in pgcb*)
            left; apply _base; exists e1; split; [left |]; auto; exists e''; split; auto.
            (*e1,e'' in lob'*)
            right; right; exists e''; auto.

       (*e',e'' in br*)
       right; right; left; exists e'; split; auto; right; apply br_mop_in_br with e''; auto.
Qed. 

(** * External Global Completion -> External Visibility lemmas *)

Lemma rf_in_dgcb E si gcb :
  rel_equal (rf E) (gcb_rf E (dgcb_loc E (MemC E si) gcb)) ->
  rel_incl (rf E) (delift (MemC E si) gcb).
Proof.
intros Hrfeq x y Hrf; destruct Hrfeq as [Hrfincl ?]; 
generalize (Hrfincl x y Hrf); intros [? [? [? [? [? [? [Hxy ?]]]]]]]; auto;
destruct Hxy as [? [? [? ?]]]; auto.
Qed.

Lemma co_in_dgcb E co si gcb :
  rel_equal (co E) (gcb_co E (dgcb_loc E (MemC E si) gcb)) ->
  rel_incl (co E) (delift (MemC E si) gcb).
Proof.
intros Hcoeq x y Hco; destruct Hcoeq as [Hcoincl ?]; 
generalize (Hcoincl x y Hco); intros [? [? [? [? [? [? [? [? ?]]]]]]]]; auto.
Qed.

Lemma erln_is_equiv E si :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  equiv_rel (erln E si).
Proof.
intros Hsiwf Hrfwf; destruct_siwf Hsiwf; unfold erln;
split; [|split].

  intro x; generalize (Hrefl x); intro Hsix.
  assert (M E x) as HMx.
    apply Hdom; exists x; auto.
  split; auto; split; auto; split; auto. 
  destruct HMx as [HEx [Hwx | Hrx]]; [left | right]; auto.
   destruct_rf_wf Hrfwf; generalize (Hex_uni x Hrx); intros [[wx Hrf] Huni];
   generalize (dom_rf_in_evts Hrf); intro HEwx.
   generalize (int_or_ext E wx x HEwx HEx); intros [Hintx | Hextx].
     right; exists wx; exists wx; split; auto; split; auto; split; auto.
     left; split; auto; exists wx; split; auto.

  intros x y [HMx [HMy [Hsi [[Hwx Hwy] | [[HERx HERy] | [wx [wy [Hrfix [Hrfiy Hsixy]]]] ]]]]]; 
    split; auto; split; auto; split; auto; right; right; exists wy; exists wx; split; auto.

  intros x y z [HMx [HMy [Hsixy Horxy]]] [HMy' [HMz [Hsiyz Horyz]]]; split; auto; split; auto; split; auto.
    apply Htrans with y; auto.
    inversion Horxy as [[Hwx Hwy] | [[HERx HERy] | [wx [wy [Hrfix [Hrfiy ?]]]]]]; clear Horxy;
    inversion Horyz as [[Hwy' Hwz] | [[HERy' HERz] | [wy' [wz [Hrfiy' [Hrfiz ?]]]]]]; clear Horyz.

      left; split; auto.

      destruct HERy' as [wy [Hrf ?]]; generalize (ran_rf_is_read Hrf); intro Hry;
      generalize (read_write_contrad y Hry Hwy); intro Ht; inversion Ht.

      destruct Hrfiy' as [Hrf ?]; generalize (ran_rf_is_read Hrf); intro Hry;
      generalize (read_write_contrad y Hry Hwy); intro Ht; inversion Ht.

      destruct HERy as [wy [Hrf ?]]; generalize (ran_rf_is_read Hrf); intro Hry;
      generalize (read_write_contrad y Hry Hwy'); intro Ht; inversion Ht.
      
      right; left; split; auto.

      destruct HERy as [wy [Hrf Hext]]; destruct Hrfiy' as [Hrf' Hint];
      generalize (ran_rf_is_read Hrf); intro Hry.
      destruct_rf_wf Hrfwf; generalize (Hex_uni y Hry); intros [? Huni];
      generalize (Huni wy wy' Hrf Hrf'); intro Heq; rewrite <- Heq in Hint;
      generalize (int_ext_contrad Hint Hext); intro Ht; inversion Ht.

      destruct Hrfiy as [Hrf ?]; generalize (ran_rf_is_read Hrf); intro Hry;
      generalize (read_write_contrad y Hry Hwy'); intro Ht; inversion Ht.

      destruct HERy' as [wy' [Hrf' Hext]]; destruct Hrfiy as [Hrf Hint];
      generalize (ran_rf_is_read Hrf); intro Hry.
      destruct_rf_wf Hrfwf; generalize (Hex_uni y Hry); intros [? Huni];
      generalize (Huni wy wy' Hrf Hrf'); intro Heq; rewrite Heq in Hint;
      generalize (int_ext_contrad Hint Hext); intro Ht; inversion Ht.   

      right; right; exists wx; exists wz; split; auto; split; auto.
      destruct Hrfiy as [Hrf Hint]; destruct Hrfiy' as [Hrf' Hint'];
      generalize (ran_rf_is_read Hrf); intro Hry;
      destruct_rf_wf Hrfwf; generalize (Hex_uni y Hry); intros [? Huni];
      generalize (Huni wy wy' Hrf Hrf'); intro Heq; 
      apply Htrans with wy; auto; rewrite Heq; auto.
Qed.

Lemma delift_trans E si gcb :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  partial_order gcb (MemC E si) ->
  transitive (delift (MemC E si) gcb).
Proof.
unfold delift;
intros Hsiwf Hrfwf Hpart x y z Hxy Hyz;
intros Cx Cz HinCx HinCz HCx HCz;
assert (MemC E si = classes (erln E si)) as HeqMemC.
  auto.
generalize (erln_is_equiv si Hsiwf Hrfwf); intro Herln;
generalize (class_of_in_classes y Herln HeqMemC); intro HCy;
generalize (class_of_refl y Herln); intro Hyy;
destruct_part Hpart; apply Htrans with (class_of (erln E si) y);
  [apply Hxy | apply Hyz]; auto.
Qed.

Lemma delift_irr E si gcb :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  partial_order gcb (MemC E si) -> 
  forall x : Event, ~ delift (MemC E si) gcb x x.
Proof.
unfold delift;
intros Hsiwf Hrfwf Hpart x Hx;
assert (MemC E si = classes (erln E si)) as HeqMemC.
  auto.
generalize (erln_is_equiv si Hsiwf Hrfwf); intro Herln;
generalize (class_of_in_classes x Herln HeqMemC); intro HCx;
generalize (class_of_refl x Herln); intro Hxx;
destruct_part Hpart; apply Hirr with (class_of (erln E si) x);
apply Hx; auto. 
Qed.

Lemma in_class_implies_class_of (A:Type) (eqr:Rln A) (Cx:Class A) (x:A) :
  equiv_rel eqr ->
  classes eqr Cx ->
  Cx x ->
  Cx = class_of eqr x.
Proof.
unfold classes; unfold class_of; 
intros Heqr [x' HeqCx] Hx; destruct_eqrln Heqr; 
apply Extensionality_Ensembles; split; intros e He; unfold In in *|-*.
  rewrite HeqCx in Hx; rewrite HeqCx in He; apply Htrans with x'; auto.
  rewrite HeqCx in Hx; rewrite HeqCx; apply Htrans with x; auto.
Qed.

Lemma not_si_implies_diff_class E si x y :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  ~(si E x y) ->
  class_of (erln E si) x <> class_of (erln E si) y.
Proof.
intros Hsiwf Hrfwf Hnsi; intro Heq; apply Hnsi.
assert ((class_of (erln E si) x) y) as Hxy.
  rewrite Heq; apply class_of_refl; apply erln_is_equiv; auto.
  destruct Hxy as [? [? [? ?]]]; auto.
Qed.

Lemma fr_implies_not_si E co si x y :
  co_well_formed E co ->
  si_well_formed E (si E) ->
  fr E co x y ->
  ~(si E x y).
Proof.
intros Hcowf Hsiwf Hfr; destruct_siwf Hsiwf.
generalize (excluded_middle (si E x y)); intros [Hsi |?]; auto.
generalize (dom_fr_is_read Hfr); intro Hrx;
generalize (ran_fr_is_write Hcowf Hfr); intro Hwy.
generalize (Hsym x y Hsi); intro Hsiyx; 
generalize (Hw y x Hsiyx Hwy); intro Hwx;
generalize (read_write_contrad x Hrx Hwx); 
intro Ht; inversion Ht.
Qed.

Lemma fr_in_dgcb E co si lob gcb :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  clinearisations (preorder_gcb_lift E si lob (MemC E si)) (MemC E si) gcb ->
  rel_equal (rf E) (gcb_rf E (dgcb_loc E (MemC E si) gcb)) ->
  rel_equal (co E) (gcb_co E (dgcb_loc E (MemC E si) gcb)) ->
  rel_incl (fr E co) (delift (MemC E si) gcb).
Proof.
intros Hsiwf Hcowf Hrfwf Hlin Hrfeq Hcoeq x y Hfr.
unfold delift; intros Cx Cy HinCx HinCy Hx Hy.

generalize (erln_is_equiv si Hsiwf Hrfwf); intro Heqr;
unfold MemC in HinCx; generalize (in_class_implies_class_of x Heqr HinCx Hx); intro Heqx1.
unfold MemC in HinCy; generalize (in_class_implies_class_of y Heqr HinCy Hy); intro Heqy1.

generalize (clin_ext_prop (MemC E si) (preorder_gcb_lift E si lob (MemC E si)) gcb); intros [Himpl ?];
generalize (Himpl Hlin); intros [Hincl Hlso]; destruct_lin Hlso.

assert (Cx <> Cy) as Hdiff.
  rewrite Heqx1; rewrite Heqy1.
  apply not_si_implies_diff_class; auto; apply fr_implies_not_si with co; auto.

generalize (Htot Cx Cy Hdiff HinCx HinCy); intros [? | Hyx]; auto.

generalize Hfr; intros [w [Hw [Hrf Hco]]]; 
generalize (rf_in_dgcb Hrfeq Hrf); intro Hgcbwx;
generalize (co_in_dgcb co Hcoeq w y Hco); intro Hgcbwy.
destruct Hrfeq as [Hinrf ?]; generalize (Hinrf w x Hrf); intros [? [? [? Hnointerv]]].
assert (loc w = loc y) as Hlocwy.
  apply co_implies_same_loc with E co; auto.
assert (loc x = loc y) as Hlocxy.
  apply fr_implies_same_loc with E co; auto.
assert False as Ht.
  apply Hnointerv; exists y; split; [|split; split]; auto.
    apply ran_co_is_write with E co w; auto.
    split; auto. 
    split; auto.
      apply ran_co_in_evts with co w; auto.
      split; auto.
      apply ran_co_in_evts with co w; auto.
      split; auto.
      split; auto.
      unfold delift; intros cy cx Hincy Hincx Hcy Hcx;
      unfold MemC in Hincx; generalize (in_class_implies_class_of x Heqr Hincx Hcx); intro Heqx2;
      unfold MemC in Hincy; generalize (in_class_implies_class_of y Heqr Hincy Hcy); intro Heqy2.
      rewrite Heqx2; rewrite Heqy2; rewrite <- Heqx1; rewrite <- Heqy1; auto.
inversion Ht.
Qed.

Lemma scaob_in_pgcb E si lob :
  rel_incl (scaob E si) (preorder_gcb E si lob).
Proof.
intros e1 e2 H12; right; auto.
Qed.

Lemma big_rel_irr E co si lob gcb x :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  clinearisations (preorder_gcb_lift E si lob (MemC E si)) (MemC E si) gcb ->
  rel_equal (rf E) (gcb_rf E (dgcb_loc E (MemC E si) gcb)) ->
  rel_equal (co E) (gcb_co E (dgcb_loc E (MemC E si) gcb)) ->
  ~(big_rel E co si lob x x).
Proof.
intros Hsiwf Hrfwf Hcowf Hlin Hrfeq Hcoeq; 
apply tc_mobs_r_mobs_irr with (erln E si) (MemC E si) gcb; auto. 
  apply erln_is_equiv; auto.
intros e1 e2 H12.
  apply rf_in_dgcb; destruct H12; auto.
  apply co_in_dgcb; auto.
  apply fr_in_dgcb with lob; auto.
  apply scaob_in_pgcb; auto.
Qed.

Lemma obsp_si_ac E co si lob gcb x :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  clinearisations (preorder_gcb_lift E si lob (MemC E si)) (MemC E si) gcb ->
  rel_equal (rf E) (gcb_rf E (dgcb_loc E (MemC E si) gcb)) ->
  rel_equal (co E) (gcb_co E (dgcb_loc E (MemC E si) gcb)) ->
  ~transitive_closure (rel_seq (obsplus E co) (si E)) x x.
Proof.
intros Hsiwf Hrfwf Hcowf Hlin Hrfeq Hcoeq Hx.

assert (erln E si = erln E si) as Heqt.
  auto.
assert ((MemC E si) = classes (erln E si)) as HeqC.
  unfold MemC; auto.
generalize (erln_is_equiv si Hsiwf Hrfwf); intro Hequivr.

assert (rel_incl (rfe E) (delift (MemC E si) gcb)) as Hrfeincl.
  intros e1 e2 [Hrf ?]; apply (rf_in_dgcb Hrfeq); auto.

generalize (co_in_dgcb co Hcoeq); intro Hcoincl.
generalize (fr_in_dgcb Hsiwf Hcowf Hrfwf Hlin Hrfeq Hcoeq); intro Hfrincl.
assert (rel_incl (scaob E si) (preorder_gcb E si lob)) as Hscaobincl.
  apply scaob_in_pgcb; auto.

generalize (tc_obsp_si_in_order Heqt Hequivr HeqC Hsiwf Hrfwf Hcowf Hlin Hrfeincl Hcoincl Hfrincl Hscaobincl Hx); 
generalize (clin_ext_prop (MemC E si) (preorder_gcb_lift E si lob (MemC E si)) gcb); intros [Himpl _]; generalize (Himpl Hlin); 
clear Himpl; intros [Hincl Hlso]; destruct_lin Hlso; destruct_part Hpart; apply Hirr.
Qed.

(** ** External global completion implies External Visibility *) 

Lemma external_global_completion_implies_external_visibility (E : set Event) (co si lob : set Event -> Rln Event) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  (exists gcb, external_global_completion E co si lob gcb) -> external_visibility E co si lob.

(** We show here that:
    - given a well-formed execution (E, lob, rf, co) 
    - if the [internal_visibility] axiom holds over E,
    - and there exists an [external_global_completion] External Global Completion order gcb,
    - then the [external_visibility] axiom holds over E *)

Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hscpv [gcb [Hlin [Hrfeq Hcoeq]]] [x Hx].
(** Reason by contradiction: suppose that the [external_visibility] axiom does not hold over E, viz, 
    there exists x s.t. (x,x) in ob.*)
generalize (gcb_path_ob_dec Hsiwf Hrfwf Hcowf Hlobwf Hx); clear Hx; intros [Hmop | [Hbr | [Hlbr | Hlop]]].

(** Now observe (c.f. [gcb_path_ob_dec]) that (x,x) in ob means either: 
 - (x,x) obsplus
 - (x,x) in big_rel 
 - (x,x) in lob';big_rel? 
 - (x,x) in lob';obs *)

(** *** Case 1: (x,x) obsplus*)
  apply (obsp_si_ac Hsiwf Hrfwf Hcowf Hlin Hrfeq Hcoeq Hmop).

(** This is a contradiction of the [internal_visibility] axiom. *)

(** *** Case 2: (x,x) in big_rel *)
  apply (big_rel_irr Hsiwf Hrfwf Hcowf Hlin Hrfeq Hcoeq Hbr); auto.

(** This is impossible as big_rel is irreflexive (c.f. [big_rel_irr]). *)

(** *** Case 3: (x,x) in lob';big_rel?

       We reason by case disjunction:
       - 3a. (x,x) in lob'
       - 3b. (x,x) in lob';big_rel *)

  destruct Hlbr as [y [Hxy Hyx]].
    inversion Hyx as [Heqyx | Hbryx]; clear Hyx.
      (** **** Case 3a: (x,x) in lob' *)
      rewrite Heqyx in Hxy; apply (lob'_irr Hlobwf Hxy).

      (** This is impossible as lob' is irreflexive (c.f. [lob'_irr]). *)

      (** **** Case 3b: (x,x) in lob';big_rel 

          In this case by definition there exists y s.t. (x,y) in lob' and (y,x) in big_rel. *)
      generalize (br_lob'_in_br_or_lob' Hsiwf Hrfwf Hcowf Hlobwf Hbryx Hxy); clear Hbryx Hxy; intros [Hbr | Hlob'].
      (** Using [br_lob'_in_br_or_lob'] we can reason by case disjunction:
          - 3bi. (x,x) in big_rel 
          - 3bii. (x,x) in lob'*) 

          (** Case 3bi: (x,x) in big_rel *)
          apply (big_rel_irr Hsiwf Hrfwf Hcowf Hlin Hrfeq Hcoeq Hbr); auto.

          (** This is impossible because big_rel is irreflexive (c.f. [big_rel_irr]). *)

          (** Case 3bii: (x,x) in lob' *)
          apply (lob'_irr Hlobwf Hlob').

          (** This is impossible because lob' is irreflexive (c.f. [lob'_irr]). *)          

(** *** Case 4: (x,x) in lob';obs* *)
  destruct Hlop as [y [Hxy Hyx]].

(** By definition this means that there exists y s.t. (x,y) in lob' and (y,x) in obs*. *)
  generalize (mop_lob'_in_lob'_or_br Hsiwf Hrfwf Hcowf Hlobwf Hyx Hxy); intros [Hlob' | Hbr].

(** Therefore (y,y) in obs*;(lob'|br). *)
  (** case where (y,y) in obs*;lob'.*)
  (** Now observe that (c.f. [mop_lob'_in_lob']) this entails (y,y) in lob'. *)
  apply (lob'_irr Hlobwf Hlob').

  (** case where (y,y) in br*)
  apply (big_rel_irr Hsiwf Hrfwf Hcowf Hlin Hrfeq Hcoeq Hbr); auto.

(** This is impossible since lob' is irreflexive (c.f. [lob'_irr]). *)
Qed.

(** * External Visibility -> External Global Completion lemmas *)
(** We show here that: 
     - given a well-formed execution (E, lob, rf, co) 
     - if the internal visibility axiom holds 
     - if the external visibility axiom holds
     - then there exists a External Global Completion external global completion order gcb.*)
(** To do so we need to exhibit an order gcb that satisfies the requirements given in [external_global_completion]. 
    Observe that (c.f. [pre_egc_partial_order]) the relation [pre_egc] is a partial order.
    Using the order extension principle (c.f. [order_ext]) we can extend pre_egc to a total order that we call gcb.
    Using [external_global_completion_gcb] we then show that gcb satisfies the [external_global_completion] requirement.*)
Definition pre_egc E co si lob := 
  transitive_closure (rel_union (rel_seq (rf E) (erln E si)) (rel_union (rel_seq (co E) (erln E si)) 
    (rel_union (rel_seq (fr E co) (erln E si)) (preorder_gcb E si lob)))).
Definition big_rel2 E co si lob := 
  transitive_closure 
    (rel_seq (maybe (transitive_closure (rel_seq (erln E si) (rel_seq (obsplus E co) (erln E si))))) 
             (rel_seq (rel_seq (erln E si) (rel_seq (preorder_gcb E si lob) (erln E si))) 
                      (maybe (transitive_closure (rel_seq (erln E si) (rel_seq (obsplus E co) (erln E si))))))).

Lemma transitive_maybe_tc r :
  transitive (maybe (transitive_closure r)).
Proof.
intros e1 e2 e3 [Heq12 | H12] [Heq23 | H23].
  left; rewrite Heq12; auto.

  right; rewrite Heq12; auto.

  right; rewrite <- Heq23; auto.

  right; apply tc_trans with e2; auto.
Qed.

Lemma rfi_erln_is_erln_rfi E si w1 e1 e2 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  rfi E w1 e1 ->
  erln E si e1 e2 ->
  (exists w2, erln E si w1 w2 /\ rfi E w2 e2).
Proof.
intros Hsiwf Hrfwf Hrfiwe1 [HM1 [HM2 [Hsi12 Hor12]]].
inversion Hor12 as [[Hw1 Hw2] | [[HER1 HER2] | Hrfisw12]]; clear Hor12.
  destruct Hrfiwe1 as [Hrf ?]; generalize (ran_rf_is_read Hrf); intro Hr1;
  generalize (read_write_contrad e1 Hr1 Hw1); intro Ht; inversion Ht.
  
  destruct Hrfiwe1 as [Hrf Hint]; destruct HER1 as [w' [Hrf' Hext]];
  generalize (ran_rf_is_read Hrf); intro Hr1;
  destruct_rf_wf Hrfwf; generalize (Hex_uni e1 Hr1); intros [? Huni];
  generalize (Huni w1 w' Hrf Hrf'); intro Heq; rewrite <- Heq in Hext;
  generalize (int_ext_contrad Hint Hext); intro Ht; inversion Ht.

  destruct Hrfisw12 as [w' [w2 [Hrfi1 [Hrfi2 Hsiw12]]]]; exists w2; split; auto.
  destruct Hrfiwe1 as [Hrf ?]; destruct Hrfi1 as [Hrf' ?];
  generalize (ran_rf_is_read Hrf); intro Hr1;
  destruct_rf_wf Hrfwf; generalize (Hex_uni e1 Hr1); intros [? Huni];
  generalize (Huni w1 w' Hrf Hrf'); intro Heq; rewrite <- Heq in Hsiw12;
  split; [| split; [|split]]; auto.  
    split; [apply dom_rf_in_evts with e1 | left; apply dom_rf_is_write with E e1]; auto. 
    destruct Hrfi2; split; [apply dom_rf_in_evts with e2 | left; apply dom_rf_is_write with E e2]; auto. 
    destruct Hrfi2; left; split; [apply dom_rf_is_write with E e1 | apply dom_rf_is_write with E e2]; auto.
Qed.

Lemma rfi_obs_in_obsp E co si w r e :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  rfi E w r ->
  obs E co r e ->
  obsplus E co w e.
Proof.
intros Hsiwf Hcowf Hrfwf Hwr Hre.
  inversion Hre as [Hrfe | [Hco | Hfr]]; clear Hre.
  
    assert (is_read r) as Hr.
      apply ran_rf_is_read with E w; destruct Hwr; auto.
    assert (is_write r) as Hw.
      apply dom_rf_is_write with E e; destruct Hrfe; auto.
    generalize (read_write_contrad r Hr Hw); intro Ht; inversion Ht.

    assert (is_read r) as Hr.
      apply ran_rf_is_read with E w; destruct Hwr; auto.
    assert (is_write r) as Hw.
      apply dom_co_is_write with E co e; auto.
    generalize (read_write_contrad r Hr Hw); intro Ht; inversion Ht.

    left; right; left; apply rf_fr_is_co with r; auto; destruct Hwr; auto.
Qed.

Lemma rfi_obsp_in_obsp E co si w r e :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  rfi E w r ->
  obsplus E co r e ->
  obsplus E co w e.
Proof.
intros Hsiwf Hcowf Hrfwf Hwr Hre.
induction Hre as [r e Hre|].
  apply rfi_obs_in_obsp with si r; auto.

  apply tc_trans with e; auto;
  apply rfi_obs_in_obsp with si e1; auto.
Qed.

Lemma rfi_erln_obsp_in_erln_obsp E (co si : set Event -> Rln Event) e1 e e0 e2 :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  rfi E e1 e0 ->
  erln E si e0 e ->
  obsplus E co e e2 ->
  rel_seq (erln E si) (obsplus E co) e1 e2. 
Proof.
intros Hsiwf Hcowf Hrfwf H10 H0e He2.
generalize (rfi_erln_is_erln_rfi Hsiwf Hrfwf H10 H0e); clear H10 H0e;
intros [we [H0we Hrfi]]; exists we; split; auto;  
apply rfi_obsp_in_obsp with si e; auto.
Qed.

Lemma rf_erln_obsp_erln_in_tc E (co si : set Event -> Rln Event) e1 e e0 e2 :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  rf E e1 e0 ->
  erln E si e0 e ->
  (rel_seq (erln E si) (rel_seq (obsplus E co) (erln E si))) e e2 ->
  transitive_closure (rel_seq (erln E si) (rel_seq (obsplus E co) (erln E si))) e1 e2. 
Proof.
intros Hsiwf Hcowf Hrfwf H10 H0e He2; 
generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [? Htrans]].

  assert (E e1) as HE1.
    apply dom_rf_in_evts with e0; auto.
  assert (E e0) as HE0.
    apply ran_rf_in_evts with e1; auto.

generalize (int_or_ext E e1 e0 HE1 HE0); intros [Hint | Hext].

  assert (rfi E e1 e0) as Hrfi.
    split; auto.
  destruct He2 as [x [Hex [y [Hxy Hy2]]]].
  assert (erln E si e0 x) as He0x.
    apply Htrans with e; auto.

    generalize (rfi_erln_obsp_in_erln_obsp Hsiwf Hcowf Hrfwf Hrfi He0x Hxy); intros [r1 [H0r1 Hr1y]].
      apply _base; exists r1; split; auto; exists y; auto.

  apply tc_trans with e; apply _base; auto.
  exists e1; split; auto; exists e0; split; auto; left; left; auto; split; auto.
Qed.

Lemma rf_tc_erln_obsp_erln_in_tc E (co si : set Event -> Rln Event) e1 e e0 e2 :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  rf E e1 e0 ->
  erln E si e0 e ->
  transitive_closure (rel_seq (erln E si) (rel_seq (obsplus E co) (erln E si))) e e2 ->
  transitive_closure (rel_seq (erln E si) (rel_seq (obsplus E co) (erln E si))) e1 e2. 
Proof.
intros Hsiwf Hcowf Hrfwf H10 H0e He2; 
generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [? Htrans]];
induction He2.

  apply rf_erln_obsp_erln_in_tc with e2 e0; auto.

  generalize (rf_erln_obsp_erln_in_tc Hsiwf Hcowf Hrfwf H10 H0e H0); intros H1e;
  apply tc_trans with e; auto.
Qed.

Lemma tc_dec2 r e1 e3 :
  transitive_closure r e1 e3 ->
  exists e2, r e1 e2 /\ maybe (transitive_closure r) e2 e3.
Proof.
intros H13; induction H13.
  exists e2; split; auto; left; auto.
  exists e; split; auto; right; auto.
Qed.

Lemma rfi_pgcb_is_pgcb E si lob e1 e2 e3 :
  rf_well_formed E ->
  rfi E e1 e2 ->
  preorder_gcb E si lob e2 e3 ->
  preorder_gcb E si lob e1 e3.
Proof.
intros Hrfwf [Hrf12 Hint12];
generalize (ran_rf_is_read Hrf12); intros Hr2 [[Hlob23 Hreqs] | Hscaob].  
  inversion Hreqs as [Hw2 | [_ Hrr23]];
  [generalize (read_write_contrad e2 Hr2 Hw2); intro Ht; inversion Ht | inversion Hrr23 as [Hext | Hint]; clear Hrr23].
  assert False as Ht.
    apply Hext; exists e1; split; auto. 
  inversion Ht.
  destruct Hint as [w' [[Hrf' ?] Hlob]]; destruct_rf_wf Hrfwf;
  generalize (Hex_uni e2 Hr2); intros [? Huni]; generalize (Huni e1 w' Hrf12 Hrf'); intro Heq.
  rewrite Heq; left; split; auto; left; apply dom_rf_is_write with E e2; auto.

  destruct Hscaob as [? [[w2 [Hrf2 Hext2]] He3]]. 

  destruct_rf_wf Hrfwf; generalize (Hex_uni e2 Hr2); intros [Hex Huni];
  generalize (Huni e1 w2 Hrf12 Hrf2); intro Heq12; rewrite Heq12 in Hint12;
  generalize (int_ext_contrad Hint12 Hext2); intro Ht; inversion Ht.
Qed.

Lemma rf_base_br2_in_br2 E co si lob e1 e0 e x e2 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  rf E e1 e0 ->
  erln E si e0 e ->
  rel_seq
        (rel_seq (erln E si)
           (rel_seq (preorder_gcb E si lob) (erln E si)))
        (maybe
           (transitive_closure
              (rel_seq (erln E si)
                 (rel_seq (obsplus E co) (erln E si))))) e x ->
   maybe
        (transitive_closure
           (rel_seq
              (maybe
                 (transitive_closure
                    (rel_seq (erln E si)
                       (rel_seq (obsplus E co) (erln E si)))))
              (rel_seq
                 (rel_seq (erln E si)
                    (rel_seq (preorder_gcb E si lob)
                       (erln E si)))
                 (maybe
                    (transitive_closure
                       (rel_seq (erln E si)
                          (rel_seq (obsplus E co) (erln E si)))))))) x e2 ->
  big_rel2 E co si lob e1 e2.
Proof.
intros Hsiwf Hrfwf H1e0 He0e Hex Hx2; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [Hsym Htrans]];

    assert (E e1) as HE1.
      apply dom_rf_in_evts with e0; auto.

    assert (E e0) as HE0.
      apply ran_rf_in_evts with e1; auto.

    generalize (int_or_ext E e1 e0 HE1 HE0); intros [Hint | Hext].
      assert (rfi E e1 e0) as Hrfi.
        split; auto.
      generalize (rfi_erln_is_erln_rfi Hsiwf Hrfwf Hrfi He0e); 
      clear Hrfi He0e; intros [w [H1w Hwe]].
      destruct Hex as [e' [Hee' He'x]].
      destruct Hee' as [a [Hea [b [Hab Hbe']]]].
      generalize (rfi_erln_is_erln_rfi Hsiwf Hrfwf Hwe Hea); clear Hwe Hea; intros [wa [Herlnwa Hrfia]].
      assert (erln E si e1 wa) as Herlne1wa.
        apply Htrans with w; auto.
      clear H1w Herlnwa.
      generalize (rfi_pgcb_is_pgcb Hrfwf Hrfia Hab); clear Hrfia Hab; intro Hpgcbwab.  
      inversion Hx2 as [Heqx2 | Htcx2]; clear Hx2.
        rewrite Heqx2 in He'x; apply _base; 
        exists e1; split; [left|]; auto. 
        exists e'; split; auto; exists wa; split; auto; exists b; split; auto.

        apply _trans with x; auto.
        exists e1; split; [left|]; auto. 
        exists e'; split; auto; exists wa; split; auto; exists b; split; auto.

      assert (rfe E e1 e0) as Hrfe.
        split; auto.
      inversion Hx2 as [Heqx2 | Htcx2]; clear Hx2.
        rewrite Heqx2 in Hex; clear Heqx2; apply _base; exists e; split; auto;
        right; apply _base; exists e1; split; auto; exists e0; split; auto; left; left; auto. 

        apply _trans with x; auto; exists e; split; auto;
        right; apply _base; exists e1; split; auto; exists e0; split; auto; left; left; auto. 
Qed.

Lemma rf_br2_in_br2 E (co si lob : set Event -> Rln Event) e1 e e0 e2 :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  rf E e1 e0 ->
  erln E si e0 e ->
  big_rel2 E co si lob e e2 ->
  (big_rel2 E co si lob) e1 e2. 
Proof.
intros Hsiwf Hcowf Hrfwf H1e0 He0e He2; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [Hsym Htrans]];
generalize (tc_dec2 He2); clear He2; intros [x [[y [Hey Hex]] He2]].

  inversion Hey as [Heqey | Htcey]; clear Hey.
    rewrite <- Heqey in Hex; clear Heqey. 
    apply rf_base_br2_in_br2 with e0 e x; auto.

    inversion He2 as [Heqxe2 | Htcxe2]; clear He2. 
      rewrite Heqxe2 in Hex; clear Heqxe2.
      apply _base; exists y; split; auto; right; apply rf_tc_erln_obsp_erln_in_tc with e e0; auto.

    apply tc_trans with x; auto.
    apply _base; exists y; split; auto; right; apply rf_tc_erln_obsp_erln_in_tc with e e0; auto.
Qed.

Lemma co_br2_in_br2 E (co si lob : set Event -> Rln Event) e1 e e0 e2 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  co E e1 e0 ->
  erln E si e0 e ->
  big_rel2 E co si lob e e2 ->
  big_rel2 E co si lob e1 e2.
Proof.
intros Hsiwf Hrfwf Hcowf H1e0 He0e He2; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?].
  apply tc_seq_left with e; auto.

    apply transitive_maybe_tc.

    right; apply _base; exists e1; split; auto;
    exists e0; split; auto; left; right; left; auto.
Qed.

Lemma fr_br2_in_br2 E (co si lob : set Event -> Rln Event) e1 e e0 e2 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  fr E co e1 e0 ->
  erln E si e0 e ->
  big_rel2 E co si lob e e2 ->
  big_rel2 E co si lob e1 e2. 
Proof.
intros Hsiwf Hrfwf Hcowf H1e0 He0e He2; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?].
  apply tc_seq_left with e; auto.

    apply transitive_maybe_tc.
    
    right; apply _base; exists e1; split; auto;
    exists e0; split; auto; left; right; right; auto.
Qed.

Definition nrel E co si lob :=
  maybe (transitive_closure 
    (rel_union (rel_seq (erln E si) (rel_seq (obsplus E co) (erln E si)))
               (rel_seq (erln E si)(rel_seq (preorder_gcb E si lob) (erln E si))))).

Lemma rfi_erln_base_nrel_in_nrel_irr E co si lob e1 e2 e3 e4 :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  rfi E e1 e2 ->
  erln E si e2 e3 ->
  rel_union (rel_seq (erln E si) (rel_seq (obsplus E co) (erln E si)))
            (rel_seq (erln E si) (rel_seq (preorder_gcb E si lob) (erln E si))) e3 e4 ->
  (transitive_closure
     (rel_union
        (rel_seq (erln E si) (rel_seq (obsplus E co) (erln E si)))
        (rel_seq (erln E si)
           (rel_seq (preorder_gcb E si lob) (erln E si))))) e1 e4.
Proof.
intros Hsiwf Hcowf Hrfwf Hrfi Herln [Heoe | Hpgcbe].
  destruct Hrfi as [Hrf ?];
  generalize (rf_erln_obsp_erln_in_tc Hsiwf Hcowf Hrfwf Hrf Herln Heoe); intro H14.
  apply tc_incl with (rel_seq (erln E si) (rel_seq (obsplus E co) (erln E si))); auto.
    intros x y Hxy; left; auto.
  
  destruct Hpgcbe as [x [H3x [y [Hxy Hy4]]]].
  generalize (erln_is_equiv si Hsiwf Hrfwf); intros [? [? Htrans]].
  assert (erln E si e2 x) as H2x.
    apply Htrans with e3; auto.
  clear Herln H3x.
  generalize (rfi_erln_is_erln_rfi Hsiwf Hrfwf Hrfi H2x); clear Hrfi H2x; intros [wx [Hrfi Hwxx]].
  apply _base; right; exists wx; split; auto; exists y; split; auto.
    apply rfi_pgcb_is_pgcb with x; auto.
Qed.

Lemma rfi_erln_base_nrel_in_nrel E co si lob e1 e2 e3 e4 :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  rfi E e1 e2 ->
  erln E si e2 e3 ->
  rel_union (rel_seq (erln E si) (rel_seq (obsplus E co) (erln E si)))
            (rel_seq (erln E si) (rel_seq (preorder_gcb E si lob) (erln E si))) e3 e4 ->
  nrel E co si lob e1 e4.
Proof.
intros Hsiwf Hcowf Hrfwf Hrfi Herln [Heoe | Hpgcbe].
  destruct Hrfi as [Hrf ?];
  generalize (rf_erln_obsp_erln_in_tc Hsiwf Hcowf Hrfwf Hrf Herln Heoe); intro H14.
  right; apply tc_incl with (rel_seq (erln E si) (rel_seq (obsplus E co) (erln E si))); auto.
    intros x y Hxy; left; auto.
  
  destruct Hpgcbe as [x [H3x [y [Hxy Hy4]]]].
  generalize (erln_is_equiv si Hsiwf Hrfwf); intros [? [? Htrans]].
  assert (erln E si e2 x) as H2x.
    apply Htrans with e3; auto.
  clear Herln H3x.
  generalize (rfi_erln_is_erln_rfi Hsiwf Hrfwf Hrfi H2x); clear Hrfi H2x; intros [wx [Hrfi Hwxx]].
  right; apply _base; right; exists wx; split; auto; exists y; split; auto.
    apply rfi_pgcb_is_pgcb with x; auto.
Qed. 

Lemma rf_erln_nrel_in_nrel_irr E co si lob e1 e2 e3 e4 :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  rf E e1 e2 ->
  erln E si e2 e3 ->
  nrel E co si lob e3 e4 ->
  (transitive_closure
     (rel_union
        (rel_seq (erln E si) (rel_seq (obsplus E co) (erln E si)))
        (rel_seq (erln E si)
           (rel_seq (preorder_gcb E si lob) (erln E si))))) e1 e4 \/ 
  (rel_seq (rfi E) (erln E si)) e1 e4.
Proof.
intros Hsiwf Hcowf Hrfwf H12 H23 H34.
assert (E e1) as HE1.
  apply dom_rf_in_evts with e2; auto.
assert (E e2) as HE2.
  apply ran_rf_in_evts with e1; auto.
generalize (int_or_ext E e1 e2 HE1 HE2); intros [Hint | Hext].

(*rfi*)
  assert (rfi E e1 e2) as Hrfi12.
    split; auto.
  clear H12 Hint.
  inversion H34 as [Heq | Htc]; clear H34.
    right; rewrite <- Heq; exists e2; split; auto.
    generalize (tc_dec2 Htc); clear Htc; intros [e [He3e Hee4]].
      inversion Hee4 as [Heqee4 | Htcee4]; clear Hee4.
        rewrite Heqee4 in He3e; clear Heqee4; left. 
          apply rfi_erln_base_nrel_in_nrel_irr with e2 e3; auto.
        left; 
        generalize (rfi_erln_base_nrel_in_nrel_irr Hsiwf Hcowf Hrfwf Hrfi12 H23 He3e); clear Hrfi12 H23 He3e; intro H1e.
          apply tc_trans with e; auto.

(*rfe*)
  assert (rfe E e1 e2) as Hrfe12.
    split; auto.
  clear H12 Hext.
  generalize (erln_is_equiv si Hsiwf Hrfwf); intros [? ?]; auto.
  left; inversion H34 as [Heq34 | Htc34]; clear H34.
    rewrite Heq34 in H23; clear Heq34.
    apply _base; left; exists e1; split; auto.
      exists e2; split; auto; left; left; auto.
    apply tc_trans with e3; auto.
      apply _base; left; exists e1; split; auto;
      exists e2; split; auto; left; left; auto.
Qed.

Lemma rf_erln_nrel_in_nrel E co si lob e1 e2 e3 e4 :
  si_well_formed E (si E) ->
  co_well_formed E co -> 
  rf_well_formed E ->
  rf E e1 e2 ->
  erln E si e2 e3 ->
  nrel E co si lob e3 e4 ->
  nrel E co si lob e1 e4 \/ (rel_seq (rfi E) (erln E si)) e1 e4.
Proof.
intros Hsiwf Hcowf Hrfwf H12 H23 H34.
assert (E e1) as HE1.
  apply dom_rf_in_evts with e2; auto.
assert (E e2) as HE2.
  apply ran_rf_in_evts with e1; auto.
generalize (int_or_ext E e1 e2 HE1 HE2); intros [Hint | Hext].

(*rfi*)
  assert (rfi E e1 e2) as Hrfi12.
    split; auto.
  clear H12 Hint.
  inversion H34 as [Heq | Htc]; clear H34.
    right; rewrite <- Heq; exists e2; split; auto.
    generalize (tc_dec2 Htc); clear Htc; intros [e [He3e Hee4]].
      inversion Hee4 as [Heqee4 | Htcee4]; clear Hee4.
        rewrite Heqee4 in He3e; clear Heqee4; left. 
          apply rfi_erln_base_nrel_in_nrel with e2 e3; auto.

        left; right;
        generalize (rfi_erln_base_nrel_in_nrel Hsiwf Hcowf Hrfwf Hrfi12 H23 He3e); clear Hrfi12 H23 He3e; intro H1e.
        inversion H1e as [Heq1e | Htc1e]; clear H1e.
          rewrite Heq1e; auto.
          apply tc_trans with e; auto.

(*rfe*)
  assert (rfe E e1 e2) as Hrfe12.
    split; auto.
  clear H12 Hext.
  generalize (erln_is_equiv si Hsiwf Hrfwf); intros [? ?]; auto.
  left; inversion H34 as [Heq34 | Htc34]; clear H34.
    rewrite Heq34 in H23; clear Heq34.
    right; apply _base; left; exists e1; split; auto.
      exists e2; split; auto; left; left; auto.
    right; apply tc_trans with e3; auto.
      apply _base; left; exists e1; split; auto;
      exists e2; split; auto; left; left; auto.
Qed.

Lemma co_erln_seq_nrel_in_nrel E (co: set Event -> Rln Event) si lob e1 e' e a :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co E e1 e' ->
  erln E si e' e ->
  nrel E co si lob e a ->
  nrel E co si lob e1 a.
Proof.
intros Hsiwf Hrfwf H1e' He'e Hor;
generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?];
inversion Hor as [Hea | Hea]; clear Hor; right.

  rewrite <- Hea; clear Hea; apply _base; left; exists e1; split; auto; 
  exists e'; split; auto; left; right; left; auto.

  apply _trans with e; auto; left; exists e1; split; auto; 
  exists e'; split; auto; left; right; left; auto.
Qed.

Lemma fr_erln_seq_nrel_in_nrel E co si lob e1 e' e a :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  fr E co e1 e' ->
  erln E si e' e ->
  nrel E co si lob e a ->
  nrel E co si lob e1 a.
Proof.
intros Hsiwf Hrfwf H1e' He'e Hor;
generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?];
inversion Hor as [Hea | Hea]; clear Hor; right.

  rewrite <- Hea; clear Hea; apply _base; left; exists e1; split; auto; 
  exists e'; split; auto; left; right; right; auto.
  apply _trans with e; auto; left; exists e1; split; auto; 
  exists e'; split; auto; left; right; right; auto.
Qed.

Lemma pgcb_seq_nrel_in_nrel E co si lob e1 e a :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  (preorder_gcb E si lob) e1 e ->
  nrel E co si lob e a ->
  nrel E co si lob e1 a.
Proof.
intros Hsiwf Hrfwf H1e Hor;
generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?];
inversion Hor as [Hea | Hea]; clear Hor; right. 

  apply _base; right; exists e1; split; auto; exists a; split; auto; rewrite <- Hea; auto.

  apply _trans with e; auto; right; exists e1; split; auto; exists e; split; auto.
Qed.

Lemma pre_egc_dec (E : set Event) (co si lob : set Event -> Rln Event) (e1 e2 : Event) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  pre_egc E co si lob e1 e2 ->
  (rel_seq (nrel E co si lob) (rel_seq (rfi E) (erln E si))) e1 e2 \/
  transitive_closure (rel_seq (erln E si) (rel_seq (obsplus E co) (erln E si))) e1 e2 \/ 
  (big_rel2 E co si lob) e1 e2.
Proof.
intros Hsiwf Hrfwf Hcowf H12; 
generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [Hsym Htrans]].

induction H12 as [e1 e2 H12 | e1 e2 e H1e He2].

  inversion H12 as [[e' [Hrf1e' Hsie'2]] | [[e' [Hco1e' Hsie'2]]  | [[e' [Hfr1e' Hsie'2]] | Hpgcb12]]]; clear H12.

    assert (E e1) as HE1.
      apply dom_rf_in_evts with e'; auto.
    assert (E e') as HE'.
      apply ran_rf_in_evts with e1; auto.
    generalize (int_or_ext E e1 e' HE1 HE'); intros [Hint | Hext].
      left; exists e1; split; auto; [left|exists e'; split]; auto; split; auto.
      right; left; apply _base; exists e1; split; auto; 
        exists e'; split; auto; left; auto; left; auto; split; auto. 
   right; left; apply _base; exists e1; split; auto;
     exists e'; split; auto; left; auto; right; left; auto. 
   right; left; apply _base; exists e1; split; auto;
     exists e'; split; auto; left; auto; right; right; auto.
   right; right; apply _base; exists e1; split; auto; [left | exists e2; split; auto]; auto;
      [exists e1; split; auto; exists e2; split; auto | left; auto]. 

  inversion H1e as [[e' [Hrf1e' Hsie'e]] | [[e' [Hco1e' Hsie'e]] | [[e' [Hfr1e' Hsie'e]] | Hpgcb1e]]]; clear H1e; 
  inversion IHHe2 as [Hnrfie2 | [Hcompluse2 | Hbre2]]; clear IHHe2.

    destruct Hnrfie2 as [e'' [Hnrel Hrfie]].
    generalize (rf_erln_nrel_in_nrel Hsiwf Hcowf Hrfwf Hrf1e' Hsie'e Hnrel); intros He1e''.
    inversion He1e'' as [Hnrel1e'' | Hrfiee1e'']; clear He1e''.
      left; exists e''; split; auto.
      assert (is_write e'') as Hwe''.
        destruct Hrfie as [r [[He''r ?] ?]];
        apply dom_rf_is_write with E r; auto.
      assert (is_read e'') as Hre''.
        destruct Hrfiee1e'' as [x [[He1x ?] Hxe'']];
        destruct Hxe'' as [? [? [Hsi ?]]]. 
        clear Hrefl Hsym Htrans; destruct_siwf Hsiwf; apply Hr with x; auto;
        apply ran_rf_is_read with E e1; auto.
      generalize (read_write_contrad e'' Hre'' Hwe''); intro Ht; inversion Ht.
        right; left; apply rf_tc_erln_obsp_erln_in_tc with e e'; auto.

    right; right; apply rf_br2_in_br2 with e e'; auto. 

    destruct Hnrfie2 as [a [Hea Ha2]]; left; exists a; split; auto.
    apply co_erln_seq_nrel_in_nrel with e' e; auto.

    right; left; apply tc_trans with e; auto; apply _base; exists e1; split; auto; exists e'; split; auto. 
      left; auto.  right; left; auto.
    right; right; apply co_br2_in_br2 with e e'; auto. 

    destruct Hnrfie2 as [a [Hea Ha2]]; left; exists a; split; auto.
    apply fr_erln_seq_nrel_in_nrel with e' e; auto.

    right; left; apply tc_trans with e; auto; apply _base; exists e1; split; auto; exists e'; split; auto. 
      left; right; right; auto.
    right; right; apply fr_br2_in_br2 with e e'; auto. 

    destruct Hnrfie2 as [a [Hea Ha2]]; left; exists a; split; auto.
    apply pgcb_seq_nrel_in_nrel with e; auto.

    right; right; apply _base; exists e1; split; auto; [left | exists e; split; [| right]]; auto.
      exists e1; split; auto; exists e; split; auto. 
    right; right; 
    apply _trans with e; auto. 
      exists e1; split; auto; [left |]; auto; exists e; split; [|left]; auto.
      exists e1; split; auto; exists e; split; auto.      
Qed.

Definition obs_extra E co := rel_union (rfe E) (rel_union (co E) (rel_union (fr E co) (rel_union (corfe E co) (frrfe E co)))).

Lemma seq_tc_reorg3 r0 r1 r2 r3 e1 e2 e3 :
  transitive r0 ->
  rel_incl (rel_seq (rel_seq r0 r2) (rel_seq r0 r3))  
    (rel_seq (maybe (transitive_closure (rel_seq r0 (rel_seq r1 r0)))) (rel_seq r0 r3)) ->
  rel_seq r0 (rel_seq (rel_union r1 r2) r0) e1 e2 ->
  (rel_seq r0 r3) e2 e3 ->
  rel_seq (maybe (transitive_closure (rel_seq r0 (rel_seq r1 r0)))) (rel_seq r0 r3) e1 e3.
Proof.
intros Htr0 Hincl [x [H1x [y [Hxy Hy2]]]] H23.
inversion Hxy as [Hr1xy | Hr2xy]; clear Hxy.

  exists e2; split; auto; right; apply _base; exists x; split; auto; exists y; split; auto.
  
  apply Hincl; exists y; split; auto.
    exists x; split; auto.
    destruct H23 as [e [H2e He3]]; exists e; split; auto; apply Htr0 with e2; auto. 
Qed.

Lemma complus_obs_extra_rln E co :
  complus E co = rel_union (obs_extra E co) (rel_union (rfi E) (rel_union (rel_seq (co E) (rfi E)) (rel_seq (fr E co) (rfi E)))). 
Proof.
apply Extensionality_Rlns; split; intros e1 e2 H12.
  inversion H12 as [Hrf | [Hco | [Hfr | [Hcorf | Hfrrf]]]]; clear H12.

    generalize (dom_rf_in_evts Hrf); intro HEe1; generalize (ran_rf_in_evts Hrf); intro HEe2;
    generalize (int_or_ext E e1 e2 HEe1 HEe2); intros [Hint | Hext];
      [right; left; split; auto | left; left; split; auto].

    left; right; left; auto.

    left; right; right; left; auto.

    destruct Hcorf as [e [Hco Hrf]]; generalize (dom_rf_in_evts Hrf); intro HEe; generalize (ran_rf_in_evts Hrf); intro HEe2;
    generalize (int_or_ext E e e2 HEe HEe2); intros [Hint | Hext].
      right; right; left; exists e; split; auto; split; auto.
      left; right; right; right; left; exists e; split; auto; split; auto.

    destruct Hfrrf as [e [Hco Hrf]]; generalize (dom_rf_in_evts Hrf); intro HEe; generalize (ran_rf_in_evts Hrf); intro HEe2;
    generalize (int_or_ext E e e2 HEe HEe2); intros [Hint | Hext].
      right; right; right; exists e; split; auto; split; auto.
      left; right; right; right; right; exists e; split; auto; split; auto.

  inversion H12 as [Hpc | [[Hrf ?] | [[e [Hco [Hrf ?]]] | [e [Hfr [Hrf ?]]]]]]; clear H12.

    inversion Hpc as [[Hrf ?] | [Hco | [Hfr | [[e [Hco [Hrf ?]]] | [e [Hfr [Hrf ?]]]]]]].
      left; auto.
      right; left; auto.
      right; right; left; auto.
      right; right; right; left; exists e; split; auto.
      right; right; right; right; exists e; split; auto.

    left; auto.
    right; right; right; left; exists e; split; auto.
    right; right; right; right; exists e; split; auto.
Qed.

Lemma complus_seq_pgcb E co si lob e1 e2 e3 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  (rel_seq (erln E si) (rel_seq (complus E co) (erln E si))) e1 e2 ->
  (rel_seq (erln E si) (preorder_gcb E si lob)) e2 e3 ->
  rel_seq (maybe (transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))))) 
    (rel_seq (erln E si) (preorder_gcb E si lob)) e1 e3. 
Proof.
intros Hsiwf Hrfwf H12 [e'2 [H22' H2'3]]; 
generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [? Htrans]].
assert (complus E co = rel_union (obs_extra E co) (rel_union (rfi E) (rel_union (rel_seq (co E) (rfi E)) (rel_seq (fr E co) (rfi E)))) ) as Heq.
  apply complus_obs_extra_rln; auto.
rewrite Heq in H12.
apply seq_tc_reorg3 with (rel_union (rfi E) (rel_union (rel_seq (co E) (rfi E)) (rel_seq (fr E co) (rfi E)))) e2; auto.
Focus 2.
  exists e'2; split; auto.

  intros x z [y [[a [Hxa Hay]] [e' [Hye' He'z]]]];
  inversion Hay as [Hrfi | [[e [Hco Hrfi]] | [e [Hfr Hrfi]]]];
  generalize (rfi_erln_is_erln_rfi Hsiwf Hrfwf Hrfi Hye'); intros [w' [Hae' Hrfi']].

    exists x; split; [left | exists w'; split; [apply Htrans with a|]]; auto; apply rfi_pgcb_is_pgcb with e'; auto.
    exists e; split; 
      [right; apply _base; exists a; split; auto; exists e; split; auto; right; left | 
      exists w'; split; auto; apply rfi_pgcb_is_pgcb with e']; auto.
    exists e; split; 
      [right; apply _base; exists a; split; auto; exists e; split; auto; right; right; left |
      exists w'; split; auto; apply rfi_pgcb_is_pgcb with e']; auto.      
Qed.    

Lemma rf_obs_extra_is_obs_extra E co e1 e2 e3 :
  co_well_formed E co ->
  rf_well_formed E ->
  rf E e1 e2 ->
  obs_extra E co e2 e3 ->
  obs_extra E co e1 e3.
Proof.
intros Hcowf Hrfwf H12 [Hrfe | [Hco | [Hfr | [Hcorfe | Hfrrfe]]]].

  destruct Hrfe as [Hrf23 ?]; 
  generalize (ran_rf_is_read H12); intro Hr2; 
  generalize (dom_rf_is_write Hrf23); intro Hw2;
  generalize (read_write_contrad e2 Hr2 Hw2); intro Ht; inversion Ht.

  generalize (ran_rf_is_read H12); intro Hr2; 
  generalize (dom_co_is_write e2 e3 Hcowf Hco); intro Hw2;
  generalize (read_write_contrad e2 Hr2 Hw2); intro Ht; inversion Ht.

  right; left; apply rf_fr_is_co with e2; auto.

  destruct Hcorfe as [e [Hco Hrfe]];
  generalize (ran_rf_is_read H12); intro Hr2; 
  generalize (dom_co_is_write e2 e Hcowf Hco); intro Hw2;
  generalize (read_write_contrad e2 Hr2 Hw2); intro Ht; inversion Ht.

  destruct Hfrrfe as [e [Hfr Hrfe]]; right; right; right; left; exists e; split; auto;
  apply rf_fr_is_co with e2; auto.
Qed.

Lemma seq_incl r0 r1 r2 x y :
  rel_incl r1 r2 ->
  rel_seq r0 (rel_seq r1 r0) x y ->
  rel_seq r0 (rel_seq r2 r0) x y.
Proof.
intros Hincl [e1 [Hx1 [e2 [H12 H2y]]]].
exists e1; split; auto; exists e2; split; auto.
Qed.

Lemma seq_tc_reorg4 r0 r1 r2 x y z :
  transitive r0 ->
  rel_incl (rel_seq (rel_seq r2 r0) (rel_seq r0 r1)) (rel_seq r0 r1) -> 
  rel_seq r0 (rel_seq (rel_union r1 r2) r0) x y ->
  rel_seq r0 (rel_seq r1 r0) y z ->
  transitive_closure (rel_seq r0 (rel_seq r1 r0)) x z.
Proof.
intros Htr0 Hincl [e1 [Hx1 [e2 [H12 H2y]]]] Hyz;
inversion H12 as [Hr112 | Hr212]; clear H12.

  apply _trans with y; [| apply _base]; auto.
    exists e1; split; auto; exists e2; split; auto.

  assert ((rel_seq r2 r0) e1 y) as H1y.
    exists e2; split; auto.
  clear Hr212 H2y e2.

  assert ((rel_seq (rel_seq r0 r1) r0) y z) as Hyz'.
    destruct Hyz as [a [Hya [b [Hab Hz]]]]. 
    exists b; split; auto; exists a; split; auto.
  clear Hyz.

  destruct Hyz' as [e [Hye Hez]].

  assert ((rel_seq r0 r1) e1 e) as He1e.
    apply Hincl; exists y; split; auto.
  clear H1y Hye y.

  destruct He1e as [e2 [H12 H2e]].
  apply _base; exists e2; split.
    apply Htr0 with e1; auto.
    exists e; split; auto.
Qed.

Lemma seq_tc_reorg5 r0 r1 r2 x y z :
  transitive r0 ->
  rel_incl (rel_seq (rel_seq r0 (rel_seq r2 r0)) (rel_seq r0 (rel_seq r1 r0))) (transitive_closure (rel_seq r0 (rel_seq r1 r0))) -> 
  rel_seq r0 (rel_seq (rel_union r1 r2) r0) x y ->
  rel_seq r0 (rel_seq r1 r0) y z ->
  transitive_closure (rel_seq r0 (rel_seq r1 r0)) x z.
Proof.
intros Htr0 Hincl [e1 [Hx1 [e2 [H12 H2y]]]] Hyz.
inversion H12 as [Hr112 | Hr212]; clear H12.

  apply _trans with y; [| apply _base]; auto.
    exists e1; split; auto; exists e2; split; auto.

  assert ((rel_seq r0 (rel_seq r2 r0)) x y) as Hxy.
    exists e1; split; auto; exists e2; auto.
  clear Hx1 Hr212 H2y.

  assert ((rel_seq (rel_seq r0 (rel_seq r2 r0)) (rel_seq r0 (rel_seq r1 r0))) x z) as Hxz.
    exists y; split; auto.
  clear Hxy Hyz.

apply Hincl; auto.
Qed.

Lemma rfi_erln_obs_extra_erln_red E co si e2 e3 e5 e6 :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  rfi E e2 e3 ->
  erln E si e3 e5 ->
  rel_seq (obs_extra E co) (erln E si) e5 e6 ->
  (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))) e2 e6.
Proof.
intros Hsiwf Hcowf Hrfwf Hrfi H35 H56.
generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [? Htrans]].
  generalize (rfi_erln_is_erln_rfi Hsiwf Hrfwf Hrfi H35); clear Hrfi H35; intros [w5 [H25 Hrfi]].

  exists w5; split; auto; clear H25.
  destruct H56 as [e [H5e He6]]; exists e; split; auto; clear He6.
  apply rf_obs_extra_is_obs_extra with e5; auto; destruct Hrfi; auto.
Qed.

Lemma obs_extra_red E co si :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
rel_incl
  (rel_seq
     (rel_seq (erln E si)
        (rel_seq
           (rel_union (rfi E)
              (rel_union (rel_seq (co E) (rfi E))
                 (rel_seq (fr E co) (rfi E)))) 
           (erln E si)))
     (rel_seq (erln E si)
        (rel_seq (obs_extra E co) (erln E si))))
((transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))))).
Proof.
intros Hsiwf Hcowf Hrfwf e1 e6 [e4 [[e2 [H12 [e3 [H23 H34]]]] [e5 [H45 H56]]]];
generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [? Htrans]].
assert (erln E si e3 e5) as H35.
  apply Htrans with e4; auto.
clear H34 H45 e4.
inversion H23 as [Hrfi | [Hcorfi | Hfrrfi]]; clear H23.

  generalize (rfi_erln_obs_extra_erln_red Hsiwf Hcowf Hrfwf Hrfi H35 H56); intro H26.
  apply tc_seq_left with e2; auto; apply _base; auto.

  destruct Hcorfi as [e [H2e Hrfi]]; apply tc_trans with e; [|clear H12 H2e]. 
    apply _base; exists e2; split; auto; exists e; split; auto; right; left; auto.
  apply _base; apply rfi_erln_obs_extra_erln_red with e3 e5; auto.

  destruct Hfrrfi as [e [H2e Hrfi]]; apply tc_trans with e; [|clear H12 H2e]. 
    apply _base; exists e2; split; auto; exists e; split; auto; right; right; left; auto.
  apply _base; apply rfi_erln_obs_extra_erln_red with e3 e5; auto.
Qed.

Lemma complus_erln_obs_extra_erln E co si e1 e2 e3 :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  (rel_seq (erln E si) (rel_seq (complus E co) (erln E si))) e1 e2 ->
  rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si)) e2 e3 ->
  transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))) e1 e3.
Proof.
intros Hsiwf Hcowf Hrfwf H12 H23;
generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [? Htrans]].


apply seq_tc_reorg5 with (rel_union (rfi E) (rel_union (rel_seq (co E) (rfi E)) (rel_seq (fr E co) (rfi E)))) e2; auto.
Focus 2.
  rewrite <- complus_obs_extra_rln; auto.

  apply obs_extra_red; auto.
Qed.

Lemma erln_complus_erln_obs_extra_erln E co si e1 e2 e3 :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  rel_seq (erln E si) (rel_seq (complus E co) (erln E si)) e1 e2 ->
  rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si)) e2 e3 ->
  transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))) e1 e3.
Proof.
intros Hsiwf Hcowf Hrfwf H12 H23.
generalize (complus_erln_obs_extra_erln Hsiwf Hcowf Hrfwf H12 H23); intro H13; auto.
Qed.

Lemma erln_complus_erln_tc_obs_extra_si E co si e1 e2 e3 :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  rel_seq (erln E si) (rel_seq (complus E co) (erln E si)) e1 e2 ->
  transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))) e2 e3 ->
  transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))) e1 e3.
Proof.
intros Hsiwf Hcowf Hrfwf H12 H23; induction H23;
generalize (erln_complus_erln_obs_extra_erln Hsiwf Hcowf Hrfwf H12 H); intro H1e; auto.
  apply tc_trans with e; auto.
Qed.

Lemma tc_erln_complus_erln_obs_extra_si E co si e1 e2 e3 :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  transitive_closure (rel_seq (erln E si) (rel_seq (complus E co) (erln E si))) e1 e2 ->
  rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si)) e2 e3 ->
  transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))) e1 e3.
Proof.
intros Hsiwf Hcowf Hrfwf H12 H23; induction H12.
Focus 2.
  apply erln_complus_erln_tc_obs_extra_si with e; auto.
  apply erln_complus_erln_tc_obs_extra_si with e2; auto; apply _base; auto.
Qed.

Lemma obs_extra_in_complus E co x y :
  (obs_extra E co) x y ->
  (complus E co) x y.
Proof.
intros Hobs_extraxy; auto.
  inversion Hobs_extraxy as [Hrf | [Hco | [Hfr | [Hcorf | Hfrrf]]]]; clear Hobs_extraxy.
    left; apply rfe_in_rf; auto.
    right; left; auto.
    right; right; auto.
    right; right; right; left; apply corfe_in_corf; auto.
    right; right; right; right; apply frrfe_in_frrf; auto.
Qed.

Lemma tc_erln_complus_erln_tc_obs_extra_si E co si e1 e2 e3 :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  transitive_closure (rel_seq (erln E si) (rel_seq (complus E co) (erln E si))) e1 e2 ->
  transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))) e2 e3 ->
  transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))) e1 e3.
Proof.
intros Hsiwf Hcowf Hrfwf H12 H23; induction H23.
Focus 2.
  apply IHtransitive_closure.
  generalize (tc_erln_complus_erln_obs_extra_si Hsiwf Hcowf Hrfwf H12 H); apply tc_incl;
  intros x z [y [Hxz [a [Hza Hay]]]]; exists y; split; auto; 
  exists a; split; auto; apply obs_extra_in_complus; auto. 

  apply tc_erln_complus_erln_obs_extra_si with e0; auto.
Qed.

Lemma mcomplus_seq_obs_extra E co si lob e1 e2 e3 :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  maybe (transitive_closure (rel_seq (erln E si) (rel_seq (complus E co) (erln E si)))) e1 e2 ->
  (rel_seq (erln E si) (preorder_gcb E si lob)) e2 e3 ->
  rel_seq (maybe (transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))))) 
    (rel_seq (erln E si) (preorder_gcb E si lob)) e1 e3. 
Proof.
intros Hsiwf Hcowf Hrfwf H12 H23; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?];
  inversion H12 as [Heq12 | Htc12]; clear H12. 
  rewrite Heq12; exists e2; split; [left|]; auto.

  induction Htc12 as [e1 e2 H12|]. 

  generalize (complus_seq_pgcb Hsiwf Hrfwf H12 H23); intros [e0 [H10 H03]];
    exists e0; split; auto.

  generalize (IHHtc12 H23); intros [e0 [[Heqee0 | Hee0] He0e3]].
    rewrite <- Heqee0 in He0e3; 
    generalize (complus_seq_pgcb Hsiwf Hrfwf H0 He0e3); intros [e' [H1' H'3]]. 
    exists e'; split; auto.

  exists e0; split; auto;
  right; apply erln_complus_erln_tc_obs_extra_si with e; auto.
Qed.

Lemma mcomplus_seq_obs_extra_incl E co si lob :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  rel_incl (rel_seq (maybe (transitive_closure (rel_seq (erln E si) (rel_seq (complus E co) (erln E si))))) 
    (rel_seq (erln E si) (preorder_gcb E si lob))) 
    (rel_seq (maybe (transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))))) 
      (rel_seq (erln E si) (preorder_gcb E si lob))).
Proof.
intros Hsiwf Hcowf Hrfwf e1 e3 [e2 [H12 H23]]; apply mcomplus_seq_obs_extra with e2; auto.
Qed.

Lemma erln_scaob_is_scaob E si e1 e2 e3 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  erln E si e1 e2 ->
  scaob E si e2 e3 ->
  scaob E si e1 e3.
Proof.
intros Hsiwf Hrfwf [? [? [Hsi12 H12]]] [H23 [HE2 HI3]]; 
destruct_siwf Hsiwf;
split; auto.
  apply Htrans with e2; auto.

  assert (is_read e2) as Hr2.
    destruct HE2 as [w2 [Hrf ?]]; apply ran_rf_is_read with E w2; auto.

  inversion H12 as [[Hw1 Hw2] | [[HER1 HER2] | Hrfisw]].

    assert False as Ht.
      apply read_write_contrad with e2; auto.
    inversion Ht.

    split; auto.

    destruct HE2 as [w2 [Hrf2 Hext2]].
    destruct Hrfisw as [? [w2' [? [[Hrf2' Hint2] ?]]]].
    destruct_rf_wf Hrfwf; generalize (Hex_uni e2 Hr2); intros [? Huni];
    generalize (Huni w2 w2' Hrf2 Hrf2'); intro Heq; rewrite <- Heq in Hint2.
    generalize (int_ext_contrad Hint2 Hext2); intro Ht; inversion Ht. 
Qed.

Lemma erln_pgcb_in_dec E (co si lob : set Event -> Rln Event) e1 e2 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  rel_seq (erln E si) (preorder_gcb E si lob) e1 e2 ->
  rel_union (scaob E si) (rel_seq (rel_union (erln E si) (scaob E si)) (ob E co (si E) (lob E))) e1 e2.
Proof.
intros Hsiwf Hrfwf Hcowf H12.
destruct H12 as [e [H1e He3]];
  generalize H1e; intros [? [? [Hsi1e ?]]]; 
  generalize Hsiwf; intro Hsiwf'; destruct_siwf Hsiwf'; 
  inversion He3 as [[Hlob ?] | Hscaob]; clear He3.

    right; exists e; split; auto; [left | apply _lob]; auto. 
    left; apply erln_scaob_is_scaob with e; auto.
Qed.

Lemma ob_erln_in_ob E (co si lob : set Event -> Rln Event) x y z :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  ob E co (si E) (lob E) x y ->
  erln E si y z ->
  ob E co (si E) (lob E) x z.
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hxy Hyz;
destruct_siwf Hsiwf.
induction Hxy as [e1 e3 [e2 [H12 H23]] | |].
  apply _obs; exists e2; split; auto; apply Htrans with e3; destruct Hyz as [? [? [Hsi ?]]]; auto.

  destruct_lob_wf Hlobwf; apply _lob; apply Hlob_erln; exists e2; split; auto.

  apply _ob with e; auto.
Qed.

Lemma ob_scaob_in_ob E (co si lob : set Event -> Rln Event) x y z :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  ob E co (si E) (lob E) x y ->
  (scaob E si) y z ->
  ob E co (si E) (lob E) x z.
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hxy Hyz;
destruct_siwf Hsiwf.
induction Hxy as [e1 e3 [e2 [H12 H23]] | |].
  apply _obs; exists e2; split; auto; apply Htrans with e3; destruct Hyz as [Hsi ?]; auto.

  destruct_lob_wf Hlobwf; apply _lob; apply Hlob_scaob; exists e2; split; auto.

  apply _ob with e; auto.
Qed.

Lemma ob_erln_union_scaob_in_ob E (co si lob : set Event -> Rln Event) x y z :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  ob E co (si E) (lob E) x y ->
  rel_union (erln E si) (scaob E si) y z ->
  ob E co (si E) (lob E) x z.
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hxy [Hyz | Hyz];
  [apply ob_erln_in_ob with y |
   apply ob_scaob_in_ob with y]; auto.
Qed.

Lemma obs_extra_in_ob E (co si lob : set Event -> Rln Event) e1 e2 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  (obs_extra E co) e1 e2 ->
  ob E co (si E) (lob E) e1 e2.
Proof.
intros Hsiwf Hrfwf Hcowf; destruct_siwf Hsiwf; intros [Hrfe | [Hco | [Hfr | [[e [Hco Hrfe]] | [e [Hfr Hrfe]]]]]].
  apply _obs; exists e2; split; auto; left; auto.
  apply _obs; exists e2; split; auto; right; left; auto.   
  apply _obs; exists e2; split; auto; right; right; auto.

  apply _ob with e; auto; apply _obs.
    exists e; split; auto; right; left; auto.   
    exists e2; split; auto; left; auto.

  apply _ob with e; auto; apply _obs.
    exists e; split; auto; right; right; auto.   
    exists e2; split; auto; left; auto.
Qed.

Lemma erln_obs_extra_erln_in_dec E (co si lob : set Event -> Rln Event) e1 e4 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si)) e1 e4 ->
  rel_union (scaob E si) (rel_seq (rel_union (erln E si) (scaob E si)) (ob E co (si E) (lob E))) e1 e4.
Proof.
intros Hsifwf Hrfwf Hcowf Hlobwf [e2 [H12 [e3 [H23 H34]]]].
right; exists e2; split; [left |]; auto.
  destruct H12 as [? [? [Hsi ?]]]; auto.
  apply ob_erln_in_ob with e3; auto; apply obs_extra_in_ob; auto.
Qed.

Lemma scaob_erln_is_scaob E si e1 e2 e3 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  scaob E si e1 e2 ->
  erln E si e2 e3 ->
  scaob E si e1 e3.
Proof.
intros Hsiwf Hrfwf [Hsi12 [HER1 HIR2]] [? [? [Hsi23 H23]]];
destruct_siwf Hsiwf; split. 
  apply Htrans with e2; auto.

  assert (is_read e2) as Hr2.
    destruct HIR2 as [w2 [Hrf2 Hint2]]; apply ran_rf_is_read with E w2; auto.
  split; auto.
    inversion H23 as [[Hw2 Hw3] | [[HER2 HER3] | Hrfisw]].
    
      generalize (read_write_contrad e2 Hr2 Hw2); intro Ht; inversion Ht. 

      destruct HIR2 as [w2 [Hrf2 Hint2]]; destruct HER2 as [w2' [Hrf2' Hext2]];
      destruct_rf_wf Hrfwf; generalize (Hex_uni e2 Hr2); intros [? Huni]; generalize (Huni w2 w2' Hrf2 Hrf2'); 
      intro Heq; rewrite <- Heq in Hext2; generalize (int_ext_contrad Hint2 Hext2); intro Ht; inversion Ht.

      destruct Hrfisw as [? [w3 [? [Hrfi3 ?]]]]; exists w3; auto.
Qed.

Lemma scaob_scaob_contrad E si e1 e2 e3 :
  rf_well_formed E ->
  scaob E si e1 e2 ->
  scaob E si e2 e3 ->
  False.
Proof.
intros Hrfwf [? [? [w2 [Hrf2 Hint]]]] [? [[w2' [Hrf2' Hext]] ?]];
  assert (is_read e2) as Hr2.
    apply ran_rf_is_read with E w2; auto.
  destruct_rf_wf Hrfwf; generalize (Hex_uni e2 Hr2); intros [? Huni]; generalize (Huni w2 w2' Hrf2 Hrf2'); 
  intro Heq; rewrite <- Heq in Hext; generalize (int_ext_contrad Hint Hext); intro Ht; inversion Ht.
Qed.

Lemma dec_trans E (co si lob : set Event -> Rln Event) e1 e2 e3 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  rel_union (scaob E si) (rel_seq (rel_union (erln E si) (scaob E si)) (ob E co (si E) (lob E))) e1 e2 ->
  rel_union (scaob E si) (rel_seq (rel_union (erln E si) (scaob E si)) (ob E co (si E) (lob E))) e2 e3 ->
  rel_union (scaob E si) (rel_seq (rel_union (erln E si) (scaob E si)) (ob E co (si E) (lob E))) e1 e3.
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf [Hsi12 | Hs12] [Hsi23 | Hs23].

  destruct Hsi12 as [? [? [w2 [Hrf2 Hint]]]]; destruct Hsi23 as [? [[w2' [Hrf2' Hext]] ?]];
  assert (is_read e2) as Hr2.
    apply ran_rf_is_read with E w2; auto.
  destruct_rf_wf Hrfwf; generalize (Hex_uni e2 Hr2); intros [? Huni]; generalize (Huni w2 w2' Hrf2 Hrf2'); 
  intro Heq; rewrite <- Heq in Hext; generalize (int_ext_contrad Hint Hext); intro Ht; inversion Ht.

  destruct Hs23 as [e [[H2e | H2e] He3]].
    generalize (scaob_erln_is_scaob Hsiwf Hrfwf Hsi12 H2e); intro H1e; right; exists e; split; auto; right; auto.
    generalize (scaob_scaob_contrad Hrfwf Hsi12 H2e); intro Ht; inversion Ht.

  destruct Hs12 as [e [H1e He2]]; generalize (ob_scaob_in_ob Hsiwf Hrfwf Hcowf Hlobwf He2 Hsi23); clear He2 Hsi23; intro He3;
  right; exists e; split; auto.

  destruct Hs12 as [e [H1e He2]]; destruct Hs23 as [e' [H2e' He'3]];
  generalize (ob_erln_union_scaob_in_ob Hsiwf Hrfwf Hcowf Hlobwf He2 H2e'); clear He2 H2e'; intro Hee';
  right; exists e; split; auto; apply _ob with e'; auto.
Qed.

Lemma dec_is_trans E (co si lob : set Event -> Rln Event) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  transitive (rel_union (scaob E si) (rel_seq (rel_union (erln E si) (scaob E si)) (ob E co (si E) (lob E)))).
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf e1 e2 e3 H12 H23; apply dec_trans with e2; auto.
Qed.

Lemma trans_equals_tc r :
  transitive r ->
  r = transitive_closure r.
Proof.
intro Htr; apply Extensionality_Rlns; split; intros e1 e2 H12.
  apply _base; auto. 
  induction H12; auto.
    apply Htr with e; auto.
Qed.

Lemma pgcb_mobs_extra_seq_dec E (co si lob : set Event -> Rln Event) e1 e3 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  rel_seq (rel_seq (erln E si) (preorder_gcb E si lob)) 
    (maybe (transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))))) e1 e3 ->
  rel_union (scaob E si) (rel_seq (rel_union (erln E si) (scaob E si)) (ob E co (si E) (lob E))) e1 e3.
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf [e2 [H12 H23]];
inversion H23 as [Heq23 | Htc23]; clear H23.

  rewrite Heq23 in H12; clear Heq23; apply erln_pgcb_in_dec; auto.
  
  generalize (dec_is_trans Hsiwf Hrfwf Hcowf Hlobwf); intro Hdt;
  rewrite (trans_equals_tc Hdt).

  induction Htc23 as [x y Hxy |].
  Focus 2.
    generalize (erln_pgcb_in_dec Hsiwf Hrfwf Hcowf H12); intro H10.
    generalize (erln_obs_extra_erln_in_dec Hsiwf Hrfwf Hcowf Hlobwf H); intro H0e.

    assert (rel_incl 
             (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))) 
             (rel_union (scaob E si) (rel_seq (rel_union (erln E si) (scaob E si)) (ob E co (si E) (lob E))))) as Hincl.
      intros a b Hab; apply erln_obs_extra_erln_in_dec; auto.
    generalize (tc_incl Hincl Htc23); intros He2.
    
    apply tc_trans with e0; auto. 
      apply _base; auto.
      apply tc_trans with e; auto; apply _base; auto. 

    apply tc_trans with x; auto; apply _base. 
      apply erln_pgcb_in_dec; auto.
      apply erln_obs_extra_erln_in_dec; auto.
Qed.

Lemma tc_pgcb_mobs_extra_dec E (co si lob aob : set Event -> Rln Event) x y :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  transitive_closure (rel_seq (rel_seq (erln E si) (preorder_gcb E si lob)) 
    (maybe (transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si)))))) x y ->
  rel_union (scaob E si) (rel_seq (rel_union (erln E si) (scaob E si)) (ob E co (si E) (lob E))) x y.
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hxy.
  generalize (dec_is_trans Hsiwf Hrfwf Hcowf Hlobwf); intro Hdt;
  rewrite (trans_equals_tc Hdt).
generalize Hxy; apply tc_incl.
intros a b Hab; apply pgcb_mobs_extra_seq_dec; auto.
Qed.

Lemma mcomplus_seq_obs_extra_seq_pgcb E co si lob e' e2 e'' e3 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  maybe (transitive_closure (rel_seq (erln E si) (rel_seq (complus E co) (erln E si)))) e' e2 ->
  transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))) e2 e'' ->
  (rel_seq (erln E si)(preorder_gcb E si lob)) e'' e3 ->
  rel_seq (maybe (transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))))) 
    (rel_seq (erln E si)(preorder_gcb E si lob)) e' e3.
Proof.
intros Hsiwf Hrfwf Hcowf H'2 H2'' H''3.
apply mcomplus_seq_obs_extra_incl; auto. 
inversion H'2 as [Heq'2 | Htc'2]; clear H'2.

  rewrite Heq'2; exists e''; split; auto.
  right; generalize H2''; apply tc_incl; intros x y [e [Hxe [e0 [Hee0 He0y]]]]; 
  exists e; split; auto; exists e0; split; auto.
    apply obs_extra_in_complus; auto.  

  generalize (tc_erln_complus_erln_tc_obs_extra_si Hsiwf Hcowf Hrfwf Htc'2 H2''); intro He'e''; exists e''; split; auto.
  right; generalize He'e''; apply tc_incl; intros x y [e [Hxe [e0 [Hee0 He0y]]]]; 
  exists e; split; auto; exists e0; split; auto.
    apply obs_extra_in_complus; auto.
Qed.

Lemma complus_seq_tc E (co si lob aob : set Event -> Rln Event) x y z :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  external_visibility E co si lob ->
  (maybe (transitive_closure (rel_seq (erln E si) (rel_seq (complus E co) (erln E si))))) x y ->
  transitive_closure (rel_seq (maybe (transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))))) (rel_seq (erln E si) (preorder_gcb E si lob))) y z ->
  transitive_closure (rel_seq (maybe (transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))))) (rel_seq (erln E si) (preorder_gcb E si lob))) x z.
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hintv Hextv Hxy Hyz.
induction Hyz. Focus 2.
  apply _trans with e; auto.
  destruct H as [e' [H1' H'e]]; inversion H1' as [Heq1' | Hobs_extra1']; clear H1'.
    rewrite Heq1' in Hxy; apply mcomplus_seq_obs_extra with e'; auto.
    apply mcomplus_seq_obs_extra_seq_pgcb with e1 e'; auto.

  destruct H as [e' [H1' H'e]]; inversion H1' as [Heq1' | Hobs_extra1']; clear H1'.
    apply _base; rewrite Heq1' in Hxy; apply mcomplus_seq_obs_extra with e'; auto. 
    apply _base; apply mcomplus_seq_obs_extra_seq_pgcb with e1 e'; auto.  
Qed. 

(** ** The relation pre_egc is a partial order over events
        viz,
        - it is defined over events
        - it is transitive
        - it is irreflexive

*)

Lemma ran_si_in_evts E si x y :
  si_well_formed E (si E) ->
  si E x y ->
  E y.
Proof.
intros Hsiwf Hxy; destruct_siwf Hsiwf.
assert (ran (si E) y) as Hrany.
  exists x; auto.
generalize (Hran y Hrany); intros [? ?]; auto.
Qed. 

Lemma pre_egc_in_evts (E : set Event) (co si lob : set Event -> Rln Event) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  Included Event (Union Event (dom (pre_egc E co si lob)) (ran (pre_egc E co si lob))) E.
Proof.
(** The relation pre_egc is defined over events and transitive:
    since pre_egc is defined as the transitive closure of a union of relations over events,
    it is trivially defined over events and trivially transitive. 
*)
intros Hsiwf Hrfwf Hcowf Hlobwf; apply r_in_evts_implies_tc_in_evts; intros _x [x Hdom | y Hran].

  destruct Hdom as [e Hxe]; inversion Hxe as [[y [Hrf ?]] | [[y [Hco ?]] | [[y [Hfr ?]] | [[Hlob ?] | Hscaob]]]].
    apply dom_rf_in_evts with y; auto.
    apply dom_co_in_evts with co y; auto.
    apply dom_fr_in_evts with co y; auto.
    destruct_lob_wf Hlobwf; apply dom_po_in_evts with e; auto.
    destruct Hscaob as [? [[wx [Hrfx ?]] ?]]; apply ran_rf_in_evts with wx; auto.  
  destruct Hran as [e Hey]; inversion Hey as [[x [? Hsi]] | [[x [? Hsi]] | [[x [? Hsi]] | [[Hlob ?] | Hscaob]]]].
    destruct Hsi as [? [? [? ?]]]; apply ran_si_in_evts with si x; auto.
    destruct Hsi as [? [? [? ?]]]; apply ran_si_in_evts with si x; auto.
    destruct Hsi as [? [? [? ?]]]; apply ran_si_in_evts with si x; auto.
    destruct_lob_wf Hlobwf; apply ran_po_in_evts with e; auto.
    destruct Hscaob as [? [? [wy [Hrfy ?]]]]; apply ran_rf_in_evts with wy; auto.
Qed.

Lemma tc_seq_split r1 r2 x y :
  transitive r1 ->
  transitive_closure (rel_seq r1 (rel_seq r2 r1)) x y -> 
  exists e, r1 x e /\ transitive_closure (rel_seq r2 r1) e y. 
Proof.
intros Htr1 Hxy; induction Hxy; [|clear Hxy].
  destruct H as [e [Hxe Hey]]; exists e; split; auto; apply _base; auto.

  destruct H as [a [H1a Hae]]; destruct IHHxy as [b [Heb Hb2]]; exists a; split; auto.
  apply _trans with b; auto. 
    destruct Hae as [c [Hac Hce]]; exists c; split; auto; apply Htr1 with e; auto.
Qed.

Lemma lrflobw_erln_is_lrflobw E si lob e1 e2 e3 :
  si_well_formed E (si E) ->
  lob_well_formed E si lob ->
  rf_well_formed E ->
  locally_reads_from_a_lob_write E lob e1 e2 ->
  erln E si e2 e3 ->
  locally_reads_from_a_lob_write E lob e1 e3.
Proof.
intros Hsiwf Hlobwf Hrfwf [w1 [Hrfi1 Hlob2]] H23;
destruct_lob_wf Hlobwf;
exists w1; split; auto;
apply Hlob_erln; exists e2; auto. 
Qed.

Lemma pc_erln_dec (E : set Event) (si lob : set Event -> Rln Event) (e1 e3 : Event) :
  si_well_formed E (si E) ->
  lob_well_formed E si lob ->
  rf_well_formed E ->
  rel_seq (preorder_gcb E si lob) (erln E si) e1 e3 ->
  (preorder_gcb E si lob) e1 e3.
Proof.
intros Hsiwf Hlobwf Hrfwf [e2 [[Hlob12 | Hscaob12] H23]]; 
generalize Hlobwf; intros Hlobwf'; destruct_lob_wf Hlobwf'.
  destruct Hlob12 as [Hlob [Hw1 | [Hr1 Hrr12]]].
    left; split; auto.
      apply Hlob_erln; exists e2; split; auto.

    left; split; auto.
      apply Hlob_erln; exists e2; split; auto.
      right; split; auto; inversion Hrr12 as [He | Hi]; clear Hrr12.
        left; auto. 
        right; auto. 
          apply lrflobw_erln_is_lrflobw with si e2; auto.
 
    right; auto.
    apply scaob_erln_is_scaob with e2; auto.
Qed.

Lemma nrel_irr_dec E co si lob x z :
  si_well_formed E (si E) ->
  lob_well_formed E si lob ->
  rf_well_formed E ->
  transitive_closure
          (rel_union
             (rel_seq (erln E si)
                (rel_seq (obsplus E co) (erln E si)))
             (rel_seq (erln E si)
                (rel_seq (preorder_gcb E si lob)
                   (erln E si)))) x z ->
 rel_seq (erln E si) 
    (transitive_closure (rel_union (rel_seq (obsplus E co) (erln E si)) (preorder_gcb E si lob))) x z.
Proof.
intros Hsiwf Hlobwf Hrfwf Hxz; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [Hsym Htrans]];
induction Hxz as [x z Hxz |].
  inversion Hxz as [[y [Ho Hy]] | [y [Hpc Hy]]]; clear Hxz;
  exists y; split; auto; apply _base; [left | right; apply pc_erln_dec]; auto.

  inversion H as [[x [Hx Ho]] | [x [Hx Hpc]]]; clear H Hxz;
  destruct IHHxz as [y [Hy Htc]]; exists x; split; auto; apply _trans with y; auto.
    destruct Ho as [z [Ho Hz]]; left; exists z; split; auto; apply Htrans with z; auto; apply Htrans with e; auto.
    destruct Hpc as [z [Hpc Hz]].
    assert (rel_seq (preorder_gcb E si lob) (erln E si) x y) as Hobs_extra.
      exists z; split; auto; apply Htrans with z; auto; apply Htrans with e; auto.
    right; apply pc_erln_dec; auto.
Qed.

Lemma tc_eoe_is_e_seq_tc_oe E co si x z :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  transitive_closure
          (rel_seq (erln E si)
             (rel_seq (obsplus E co) (erln E si))) x z ->
  exists y, erln E si x y /\ transitive_closure (rel_seq (obsplus E co) (erln E si)) y z.
Proof.
intros Hsiwf Hrfwf Hxz; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [Hsym Htrans]]; induction Hxz.
  destruct H as [y [H1y Hy2]]; exists y; split; auto; apply _base; auto.
  clear Hxz; destruct H as [a [H1a Hae]]; destruct IHHxz as [b [Heb Hb2]]; 
  exists a; split; auto; apply _trans with b; auto.
  destruct Hae as [x [Hax Hxe]]; exists x; split; auto; 
  apply Htrans with e; auto.
Qed.

Lemma oe_mtc_eoe_in_tc E co si a b e2 r:
  si_well_formed E (si E) ->
  rf_well_formed E ->
  rel_seq (obsplus E co) (erln E si) a b ->
  maybe
        (transitive_closure
           (rel_seq (erln E si)
              (rel_seq (obsplus E co) (erln E si)))) b e2 ->
  transitive_closure
  (rel_union (rel_seq (obsplus E co) (erln E si)) r) a e2.
Proof.
intros Hsiwf Hrfwf Hab [Heq | Htcb2]; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [Hsym Htrans]].
  rewrite Heq in Hab; apply _base; left; auto.
  generalize (tc_eoe_is_e_seq_tc_oe Hsiwf Hrfwf Htcb2); clear Htcb2; intros [y [Hby Hy2]].
  apply _trans with y; auto.
    left; destruct Hab as [x [Hax Hxb]]; exists x; split; auto; apply Htrans with b; auto.
    apply tc_incl with (rel_seq (obsplus E co) (erln E si)); auto.
      intros c d Hcd; left; auto.
Qed.

Lemma mtc_eoe_e_is_mtc_eoe E co si b x e :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  maybe
        (transitive_closure
           (rel_seq (erln E si)
              (rel_seq (obsplus E co) (erln E si)))) b x ->
  erln E si x e ->
  maybe
        (transitive_closure
           (rel_seq (erln E si)
              (rel_seq (obsplus E co) (erln E si)))) b e \/ erln E si b e. 
Proof.
intros Hsiwf Hrfwf; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [Hsym Htrans]];
intros [Heqbx | Hbx] Hxe.
  rewrite Heqbx; clear Heqbx; right; auto.

  induction Hbx.

    left; destruct H as [a [H1a [b [Hab Hb2]]]]; 
      right; apply _base; exists a; split; auto;
      exists b; split; auto; apply Htrans with e2; auto.

    clear Hbx; generalize (IHHbx Hxe); clear Hxe IHHbx; intros [[Heq0e | Htc] | Herln].   
      rewrite Heq0e in H; clear Heq0e; left; right; apply _base; auto.
      left; right; apply _trans with e0; auto.
      left; right; apply _base; destruct H as [a [H1a [b [Hab Hb0]]]]; 
      exists a; split; auto; exists b; split; auto; apply Htrans with e0; auto.
Qed.

Lemma br2_base_dec (E : set Event) (co si lob : set Event -> Rln Event) (e1 e2 : Event) :
  si_well_formed E (si E) ->
  lob_well_formed E si lob ->
  rf_well_formed E ->
  co_well_formed E co ->
  rel_seq
      (maybe
         (transitive_closure
            (rel_seq (erln E si)
               (rel_seq (obsplus E co) (erln E si)))))
      (rel_seq
         (rel_seq (erln E si)
            (rel_seq (preorder_gcb E si lob) (erln E si)))
         (maybe
            (transitive_closure
               (rel_seq (erln E si)
                  (rel_seq (obsplus E co) (erln E si)))))) e1 e2 ->
  rel_seq (erln E si) 
    (transitive_closure (rel_union (rel_seq (obsplus E co) (erln E si)) (preorder_gcb E si lob))) e1 e2.
Proof.
intros Hsiwf Hlobwf Hrfwf Hcowf H12; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [Hsym Htrans]].

  destruct H12 as [x [H1x [y [Hxy Hy2]]]];
  inversion H1x as [Heq1x | Htc1x]; clear H1x;
  inversion Hy2 as [Heqy2 | Htcy2]; clear Hy2.

    rewrite Heq1x; rewrite <- Heqy2; clear Heq1x Heqy2;
    destruct Hxy as [z [Hxz Hzy]]; 
    exists z; split; auto; apply _base; right; 
    apply (pc_erln_dec Hsiwf Hlobwf Hrfwf Hzy). 

    rewrite Heq1x; clear Heq1x;
    destruct Hxy as [z [Hxz Hzy]]; 
    exists z; split; auto; clear Hxz.
    generalize (tc_dec2 Htcy2); clear Htcy2; intros [e [[d [Hyd Hde]] He2]].
    assert (rel_seq (preorder_gcb E si lob) (erln E si) z d) as Hzd.
      destruct Hzy as [z' [Hzz' Hz'y]]; exists z'; split; auto; apply Htrans with y; auto.
        apply _trans with d; [right; apply pc_erln_dec|apply oe_mtc_eoe_in_tc with e]; auto.

    rewrite Heqy2 in Hxy; clear Heqy2;
    destruct Hxy as [z [Hxz Hz2]];
    generalize (tc_dec2 Htc1x); clear Htc1x; intros [b [[a [H1a Hab]] Hbx]];
    exists a; split; auto;
    apply tc_trans with z; auto.
      generalize (mtc_eoe_e_is_mtc_eoe Hsiwf Hrfwf Hbx Hxz); intros [Hmbz | Heqbz].

      apply oe_mtc_eoe_in_tc with b; auto.
      apply _base; left; destruct Hab as [e [Hae Heb]]; exists e; split; auto; apply Htrans with b; auto.
      apply _base; right; apply pc_erln_dec; auto.

    destruct Hxy as [z [Hxz Hz2]];
    generalize (tc_dec2 Htc1x); clear Htc1x; intros [b [[a [H1a Hab]] Hbx]];
    generalize (tc_dec2 Htcy2); clear Htcy2; intros [e [[d [Hyd Hde]] He2]].
    exists a; split; auto; clear H1a;
    generalize (mtc_eoe_e_is_mtc_eoe Hsiwf Hrfwf Hbx Hxz); clear Hbx Hxz; intros [Hmbz | Heqbz].

      generalize (oe_mtc_eoe_in_tc (preorder_gcb E si lob) Hsiwf Hrfwf Hab Hmbz); clear Hab Hmbz; intro Haz;
      apply tc_trans with z; auto; clear Haz.
      apply _trans with d; auto; [right; apply pc_erln_dec|]; auto.
        destruct Hz2 as [f [Hzf Hfy]]; exists f; split; auto; apply Htrans with y; auto.
        apply oe_mtc_eoe_in_tc with e; auto.

      apply tc_trans with z; auto.
      apply _base; destruct Hab as [x' [Hax' Hx'b]]; left; exists x'; split; auto; apply Htrans with b; auto.
      apply _trans with d; auto; [right; apply pc_erln_dec|]; auto.
        destruct Hz2 as [f [Hzf Hfy]]; exists f; split; auto; apply Htrans with y; auto.
        apply oe_mtc_eoe_in_tc with e; auto.
Qed.

Lemma tcu_e_is_tcu E co si lob a e b :
  si_well_formed E (si E) ->
  lob_well_formed E si lob ->
  rf_well_formed E ->
  transitive_closure
        (rel_union (rel_seq (obsplus E co) (erln E si))
           (preorder_gcb E si lob)) a e ->
  erln E si e b ->
  transitive_closure
        (rel_union (rel_seq (obsplus E co) (erln E si))
           (preorder_gcb E si lob)) a b.
Proof.
intros Hsiwf Hlobwf Hrfwf Hae Heb; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [Hsym Htrans]].
induction Hae.
  inversion H as [Ho | Hpc]; clear H.
    destruct Ho as [a [H1a Ha2]]; apply _base; left; exists a; split; auto.
      apply Htrans with e2; auto.
    apply _base; right; apply pc_erln_dec; auto; exists e2; split; auto.
  apply _trans with e; auto.
Qed.

Lemma br2_dec (E : set Event) (co si lob : set Event -> Rln Event) (e1 e2 : Event) :
  si_well_formed E (si E) ->
  lob_well_formed E si lob ->
  rf_well_formed E ->
  co_well_formed E co ->
  (big_rel2 E co si lob) e1 e2 -> 
  rel_seq (erln E si) 
    (transitive_closure (rel_union (rel_seq (obsplus E co) (erln E si)) (preorder_gcb E si lob))) e1 e2.
Proof.
intros Hsiwf Hlobwf Hrfwf Hcowf H12; induction H12.

  apply br2_base_dec; auto.
  
  generalize (br2_base_dec Hsiwf Hlobwf Hrfwf Hcowf H); clear H H12; intros [a [H1a Hae]];
  exists a; split; auto; clear H1a;
  destruct IHtransitive_closure as [b [Heb Hb2]]; apply tc_trans with b; auto; clear Hb2.
  apply tcu_e_is_tcu with e; auto.
Qed.

Lemma op_si_in_ob (E : set Event) (co si lob : set Event -> Rln Event) x y z :
  si_well_formed E (si E) ->
  (obsplus E co) x y ->
  si E y z -> 
  ob E co (si E) (lob E) x z.
Proof.
intros Hsiwf Hxy Hyz; induction Hxy.
Focus 2.
  destruct_siwf Hsiwf; 
  apply _ob with e; auto; apply _obs; exists e; split; auto.

apply _obs; exists e2; split; auto.
Qed.

Lemma op_e_in_ob (E : set Event) (co si lob : set Event -> Rln Event) x y :
  si_well_formed E (si E) ->
  rel_seq (obsplus E co) (erln E si) x y -> 
  ob E co (si E) (lob E) x y.
Proof.
intros Hsiwf [e [Hxe [? [? [Hsi ?]]]]]; 
apply op_si_in_ob with e; auto.
Qed.

Lemma pc_in_dec (E : set Event) (co si lob : set Event -> Rln Event) (x y : Event) :
  preorder_gcb E si lob x y ->
  rel_seq (maybe (scaob E si)) (ob E co (si E) (lob E)) x y \/ rel_seq (scaob E si) (maybe (lob' E lob)) x y.
Proof.
intros [[Hlob ?] | Hsi]; [left; exists x; split | right]; auto.
  left; auto. apply _lob; auto.
  exists y; split; [|left]; auto.
Qed.  

Lemma pc_in_dec2 (E : set Event) (co si lob : set Event -> Rln Event) (x y : Event) :
  preorder_gcb E si lob x y ->
  ob E co (si E) (lob E) x y \/ rel_seq (scaob E si) (maybe (lob' E lob)) x y.
Proof.
intros [[Hlob ?] | Hsi]; [left | right]; auto.
  apply _lob; auto.
  exists y; split; [|left]; auto.
Qed.  

Lemma nrr_implies_IR E lob e1 e2 :
  ~read_requirements E lob e1 e2 ->
  IR E e1.
Proof.
unfold read_requirements;
intros Hnrr;
generalize (not_or_and (does_not_locally_reads_from E e1) (locally_reads_from_a_lob_write E lob e1 e2) Hnrr);
clear Hnrr; unfold does_not_locally_reads_from; intros [H1 H2].
generalize (NNPP (IR E e1) H1); auto.
Qed.

Lemma tc_e_in_ob_or_scaob (E : set Event) (co si lob : set Event -> Rln Event) (x y z : Event) :
  si_well_formed E (si E) ->
  lob_well_formed E si lob ->
  rf_well_formed E ->
  transitive_closure
        (rel_union (rel_seq (obsplus E co) (erln E si))
           (preorder_gcb E si lob)) x y ->
  erln E si y z -> 
  rel_seq (maybe (scaob E si)) (ob E co (si E) (lob E)) x z \/ rel_seq (scaob E si) (maybe (lob' E lob)) x z.
Proof.
intros Hsiwf Hlobwf Hrfwf Hxy Hyz; generalize (tcu_e_is_tcu Hsiwf Hlobwf Hrfwf Hxy Hyz); clear Hxy Hyz; intros Hxz.
induction Hxz; [| clear Hxz]. 
  inversion H as [Hobs | Hpc]; clear H; [left; exists e1; split; auto; [left | apply op_e_in_ob]; auto | apply pc_in_dec]; auto.
  
  inversion H as [Hobs1e | Hpc1e]; clear H; inversion IHHxz as [Hobe2 | Hsie2]; clear IHHxz.
    destruct Hobe2 as [b [[Heqeb | [Hsieb ?]] Hb2]].
      rewrite Heqeb in Hobs1e; clear Heqeb;
      left; exists e1; split; auto; 
        [left|apply _ob with b; auto; apply op_e_in_ob]; auto.
      destruct Hobs1e as [a [H1a [? [? [Hae ?]]]]]; 
      left; exists e1; split; auto; 
        [left|apply _ob with b; auto; apply op_si_in_ob with a; auto; destruct_siwf Hsiwf; apply Htrans with e]; auto.

    destruct Hsie2 as [b [[Hsi ?] [Heq | [Hlob ?]]]]; destruct Hobs1e as [a [H1a [? [? [Hae ?]]]]].
      rewrite Heq in Hsi; clear Heq; left; exists e1; split; auto; 
        [left | apply op_si_in_ob with a; auto; destruct_siwf Hsiwf; apply Htrans with e]; auto.
      left; exists e1; split; auto; 
        [left | apply _ob with b; auto;
                  [apply op_si_in_ob with a; auto; destruct_siwf Hsiwf; apply Htrans with e |
                   apply _lob]]; auto.

    inversion Hpc1e as [[Hlob1e ?] | Hscaob]; clear Hpc1e; 
      [left; exists e1; split; auto; [left |]|]; auto.
        destruct Hobe2 as [b [[Heqeb | Hscaobeb] Hb2]]; apply _ob with b; auto. 
          rewrite Heqeb in Hlob1e; clear Heqeb; apply _lob; auto.
          destruct_lob_wf Hlobwf; apply _lob; apply Hlob_scaob; exists e; split; auto.
      destruct Hobe2 as [b [[Heqeb | Hscaobeb] Hb2]]. 
        rewrite Heqeb in Hscaob; clear Heqeb; left; exists b; split; auto; right; auto. 
        generalize (scaob_scaob_contrad Hrfwf Hscaob Hscaobeb); intro Ht; inversion Ht.

    inversion Hpc1e as [[Hlob1e ?] | Hscaob1e]; clear Hpc1e; 
    destruct Hsie2 as [b [Hscaob [Heq | Hlob']]].
      rewrite Heq in Hscaob; clear Heq; left; exists e1; split; 
        [left| apply _lob; destruct_lob_wf Hlobwf; apply Hlob_scaob; exists e; auto]; auto.
      left; exists e1; split; destruct Hlob' as [Hlob ?];
        [left| apply _ob with b; apply _lob; auto; destruct_lob_wf Hlobwf; apply Hlob_scaob; exists e; auto]; auto.
      generalize (scaob_scaob_contrad Hrfwf Hscaob1e Hscaob); intro Ht; inversion Ht.
      generalize (scaob_scaob_contrad Hrfwf Hscaob1e Hscaob); intro Ht; inversion Ht.
Qed.

Lemma ob_scaob E co si lob x y z :
  si_well_formed E (si E) ->
  lob_well_formed E si lob ->
  rf_well_formed E ->
  ob E co (si E) (lob E) x y ->
  scaob E si y z ->
  ob E co (si E) (lob E) x z.
Proof.
intros Hsiwf Hlobwf Hrfwf Hxy Hyz; induction Hxy.

  destruct H as [a [Hxa Hay]]; destruct Hyz as [? [? [Hsi ?]]]; apply _obs;
  exists a; split; auto; destruct_siwf Hsiwf; apply Htrans with e2; auto.
  
  destruct_lob_wf Hlobwf; apply _lob; apply Hlob_scaob; exists e2; auto.
  
  apply _ob with e; auto.
Qed.

Lemma scaob_irr E si x :
  rf_well_formed E ->
  scaob E si x x -> False.
Proof.
intros Hrfwf [Hsi [[wx [Hrfx HEx]] [wx' [Hrfx' HIx]]]].
assert (is_read x) as Hrx.
  apply ran_rf_is_read with E wx; auto.
destruct_rf_wf Hrfwf; generalize (Hex_uni x Hrx); intros [? Huni];
generalize (Huni wx wx' Hrfx Hrfx'); intro Heq;
rewrite <- Heq in HIx; apply int_ext_contrad with E wx x; auto.
Qed.

Lemma e_tc_union_irr (E : set Event) (co si lob : set Event -> Rln Event) (x : Event) :
  si_well_formed E (si E) ->
  lob_well_formed E si lob ->
  rf_well_formed E ->
  external_visibility E co si lob ->
  rel_seq (erln E si) 
      (transitive_closure (rel_union (rel_seq (obsplus E co) (erln E si)) (preorder_gcb E si lob))) x x -> 
  False.
Proof.
intros Hsiwf Hlobwf Hrfwf Hev [y [Hxy Hyx]].
generalize (tc_e_in_ob_or_scaob Hsiwf Hlobwf Hrfwf Hyx Hxy); clear Hxy Hyx x; 
  intros [[x [[Heq | Hsi] Hob]] | [x [Hscaob [Heq | Hlob']]]].

  rewrite Heq in Hob; clear Heq; apply Hev; exists x; auto.
  apply Hev; exists x; apply ob_scaob with y; auto.

  rewrite Heq in Hscaob; clear Heq. 
    apply scaob_irr with E si y; auto.

  destruct Hlob' as [Hlob ?]; apply Hev; exists x; apply _lob;
  destruct_lob_wf Hlobwf; apply Hlob_scaob; exists y; auto. 
Qed.
  
(** ** The relation pre_egc is irreflexive:
*)
Lemma pre_egc_irr (E : set Event) (co si lob aob : set Event -> Rln Event) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  external_visibility E co si lob ->
  (forall x : Event, ~ pre_egc E co si lob x x).
Proof.
(** Reason by contradiction and suppose that there exists x s.t. (x,x) in pre_egc.*)
intros Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis x Hx.
generalize (pre_egc_dec Hsiwf Hrfwf Hcowf Hx); clear Hx; intros [Hx|Hx].

(** Observe (c.f. [pre_egc_dec]) that this entails:
    - either (x,x) in complus
    - or (x,x) in big_rel2.

    We then reason by case disjunction. *)

(** ** Case 1: (x,x) in nrel;rfi;erln *)
  destruct Hx as [y [Hxy [z [[Hyz Hint] Hzx]]]].
  generalize (rf_erln_nrel_in_nrel_irr Hsiwf Hcowf Hrfwf Hyz Hzx Hxy); clear Hyz Hzx Hxy Hint x z; intros [Hnrel | Hrfie].

    generalize (nrel_irr_dec Hsiwf Hlobwf Hrfwf Hnrel); clear Hnrel. 
      apply e_tc_union_irr; auto.
    destruct Hrfie as [x [[Hrf ?] Herln]];
    apply read_write_contrad with y; auto.
      destruct_siwf Hsiwf; apply Hr with x; auto.
        destruct Herln as [? [? [Hsi ?]]]; auto.
        apply ran_rf_is_read with E y; auto.
      apply dom_rf_is_write with E x; auto.

(** This is impossible as complus must be (c.f. [complus_irr]) irreflexive under the [internal_visibility] requirement.*)

(** ** Case 2: (x,x) in (e;obs+;e)+ or big_rel2 *)

  inversion Hx as [Heoe | Hbr2]; clear Hx. 
   generalize (tc_eoe_is_e_seq_tc_oe Hsiwf Hrfwf Heoe); intros [y [Hxy Hyx]];
    apply e_tc_union_irr with E co si lob x; auto; exists y; split; auto; 
    apply tc_incl with (rel_seq (obsplus E co) (erln E si)); auto; 
    intros e1 e2 H12; left; auto.

    apply e_tc_union_irr with E co si lob x; auto; apply br2_dec; auto.
Qed.

Lemma pre_egc_partial_order (E : set Event) (co si lob aob : set Event -> Rln Event) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  external_visibility E co si lob ->
  partial_order (pre_egc E co si lob) E.
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis; split; [|split].
apply pre_egc_in_evts; auto.
intros e1 e2 e3 H12 H23; apply tc_trans with e2; auto.
apply pre_egc_irr; auto.
Qed.

Lemma rel_incl_lift (A:Type) (C:set (Class A)) (r1 r2 : Rln A) :
  rel_incl r1 r2 ->
  rel_incl (lift C r1) (lift C r2).
Proof.
unfold lift; 
intros Hincl C1 C2 [HC1 [HC2 [x [y [Hx1 [Hy2 Hrxy]]]]]]; 
split; auto; split; auto; exists x; exists y; split; auto.
Qed.

(** ** A linear extension of the relation pre_egc satisfies the [external_global_completion] requirement
        viz,
        - gcb is a linear extension of the preorder_gcb relation
        - the read-froms extracted from gcb [gcb_rf] are the same as the axiomatic ones [rf]
        - the coherence extracted from gcb [gcb_co] are the same as the axiomatic ones [co] 

*)

Lemma read_reqs_erln_is_read_reqs E si lob x y z :
  lob_well_formed E si lob ->
  read_requirements E lob x y ->
  erln E si y z ->
  read_requirements E lob x z.
Proof.
intros Hlobwf [Hext | [wx [Hrfi Hlob]]] Hyz; [left | right]; auto.
  exists wx; split; auto.
  destruct_lob_wf Hlobwf; apply Hlob_erln; exists y; auto.
Qed.

Lemma lob'_erln_is_lob' E si lob x y z :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  lob_well_formed E si lob ->
  lob' E lob x y ->
  erln E si y z ->
  lob' E lob x z.
Proof.
intros Hsiwf Hrfwf Hlobwf [Hxy [? Hrr]] Hyz; 
generalize Hlobwf; intros Hlobwf'; destruct_lob_wf Hlobwf'; split; [|split]; auto.
  apply Hlob_erln; exists y; auto.
  generalize (erln_is_equiv si Hsiwf Hrfwf); intros [? [Hsym ?]];
  generalize (Hsym y z Hyz); intro Hzy; intro Hreqs.
  generalize (read_reqs_erln_is_read_reqs Hlobwf Hreqs Hzy); intro Hc; apply Hrr; auto.
Qed.

Lemma pre_egc_erln_is_pre_egc E co si lob x y z :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  lob_well_formed E si lob ->
  pre_egc E co si lob x y ->
  erln E si y z ->
  pre_egc E co si lob x z.
Proof.
intros Hsiwf Hrfwf Hlobwf Hxy Hyz; induction Hxy as [x y Hb | x e3 y Htc].
Focus 2.
  apply _trans with y; auto; unfold pre_egc in IHHxy; generalize (IHHxy Hyz); intro Ht; auto.
  
  assert (si E y z) as Hsi.
    destruct Hyz as [? [? [? ?]]]; auto.
  generalize (erln_is_equiv si Hsiwf Hrfwf); intros [? [? Htrans]];
  inversion Hb as [Hrfsi | [Hcosi | [Hfrsi | Hpc]]].
    
    destruct Hrfsi as [e [Hxe Hey]]; apply _base; left; exists e; split; auto.
    apply Htrans with y; auto.

    destruct Hcosi as [e [Hxe Hey]]; apply _base; right; left; exists e; split; auto;
    apply Htrans with y; auto.    

    destruct Hfrsi as [e [Hxe Hey]]; apply _base; right; right; left; exists e; split; auto;
    apply Htrans with y; auto.

    generalize Hlobwf; intro Hlobwf'; destruct_lob_wf Hlobwf'; 
    inversion Hpc as [[Hlob Hreqs] | Hscaob]; clear Hpc; apply _base; right; right; right. 
      left; split.
        apply Hlob_erln; exists y; split; auto.
        inversion Hreqs as [Hwx | [Hrx Hrr]]; [left | right; split]; auto.
        apply read_reqs_erln_is_read_reqs with si y; auto.

        right; apply scaob_erln_is_scaob with y; auto.
Qed.

Lemma udr_lift E co si lob :
  Included (Class Event)
    (Union (Class Event)
       (dom (lift (MemC E si) (pre_egc E co si lob)))
       (ran (lift (MemC E si) (pre_egc E co si lob))))
    (MemC E si).
Proof.
unfold lift;
intros ? [Cx [Cy [? ?]] | Cy [Cx [? [? ?]]]]; auto.
Qed.

Lemma trans_lift E co si lob :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  lob_well_formed E si lob ->
  partial_order (pre_egc E co si lob) E ->
  transitive (lift (MemC E si) (pre_egc E co si lob)).
Proof.
intros Hsiwf Hrfwf Hlobwf Hpart; destruct_part Hpart.
  unfold lift; intros Cx Cy Cz [HCx [HCy [x [y [Hx [Hy Hxy]]]]]] [? [HCz [y' [z [Hy' [Hz Hy'z]]]]]];
  split; auto; split; auto; exists x; exists z; split; auto; split; auto.
    apply Htrans with y'; auto.
    unfold MemC in HCy; unfold classes in HCy; unfold class_of in HCy; destruct HCy as [y0 HCy];
    rewrite HCy in Hy; rewrite HCy in Hy'.
    assert (erln E si y y') as Hyy'.
      generalize (erln_is_equiv si Hsiwf Hrfwf); intro Hequiv; clear Htrans; destruct_eqrln Hequiv;
      apply Htrans with y0; auto.
    apply pre_egc_erln_is_pre_egc with y; auto.
Qed.

Lemma irr_lift E co si lob Cx :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  lob_well_formed E si lob ->
  partial_order (pre_egc E co si lob) E ->
  ~ lift (MemC E si) (pre_egc E co si lob) Cx Cx.
Proof.
  intros Hsiwf Hrfwf Hlobwf Hpart; destruct_part Hpart.
  unfold lift; intros [HCx [? [e1 [e2 [H1 [H2 H12]]]]]].
  unfold MemC in HCx; unfold classes in HCx; unfold class_of in HCx; destruct HCx as [x0 HCx];
  rewrite HCx in H1; rewrite HCx in H2. 
    assert (erln E si e2 e1) as Hyy'.
      generalize (erln_is_equiv si Hsiwf Hrfwf); intro Hequiv; clear Htrans; destruct_eqrln Hequiv;
      apply Htrans with x0; auto.
  apply Hirr with e1; apply pre_egc_erln_is_pre_egc with e2; auto.
Qed.

Lemma lift_partial_order E co si lob :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  lob_well_formed E si lob ->
  partial_order (pre_egc E co si lob) E ->
  partial_order (lift (MemC E si) (pre_egc E co si lob)) (MemC E si). 
Proof.
intros Hsiwf Hrfwf Hlobwf Hpart; split; [|split].

  apply udr_lift; auto.

  apply trans_lift; auto.

  intros Cx; apply irr_lift; auto.
Qed.

(** *** The gcb relation is a linear extension of the preorder_gcb relation *)
Lemma gcb_is_lin (E : set Event) (co si lob : set Event -> Rln Event) (gcb : Rln (Class Event)) :
  clinearisations (lift (MemC E si) (pre_egc E co si lob)) (MemC E si) gcb ->
  clinearisations (preorder_gcb_lift E si lob (MemC E si)) (MemC E si) gcb.
Proof.
(** By definition gcb is a linearisation of the relation pre_egc. 

    Observe that preorder_gcb is included in pre_egc.

    Note that for all relation r2 included in another relation r1,
    a linearisation of the bigger relation r1   
    is a linearisation of the smaller relation r2. 

    Therefore a linearisation of the bigger relation pre_egc
    is also a linearisation of the smaller relation preorder_gcb. *)
intros Hlin; apply clin_of_big_is_clin_of_little with (lift (MemC E si) (pre_egc E co si lob)); auto.
apply rel_incl_lift; auto.
intros x y Hxy; apply _base; right; right; right; auto.
Qed.

Lemma co_in_pre_egc E co si lob :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  rel_incl (co E) (pre_egc E co si lob).
Proof.
intros Hsiwf Hrfwf x y Hxy; apply _base; right; left; auto.
generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?].
exists y; split; auto.
Qed.

Lemma MemC_in_evts E si Cx x :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  MemC E si Cx ->
  Cx x ->
  E x.
Proof.
unfold MemC; unfold classes; unfold class_of; unfold erln;
intros Hsiwf Hrfwf [e HCx] Hx; rewrite HCx in Hx; destruct Hx as [? [[? ?] ?]]; auto.
Qed.

Lemma co_gcb_incl (E : set Event) (co si lob : set Event -> Rln Event) gcb :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  clinearisations (lift (MemC E si) (pre_egc E co si lob)) (MemC E si) gcb ->
  rel_incl (co E) (gcb_co E (dgcb_loc E (MemC E si) gcb)).
Proof.
intros Hsiwf Hcowf Hrfwf Hlin; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?]. 

  intros x y Hxy; split; [| split; [|split; [|split; [|split]]]].
    apply dom_co_in_evts with co y; auto.
    apply ran_co_in_evts with co x; auto.
    apply dom_co_is_write with E co y; auto.
    apply ran_co_is_write with E co x; auto.
    apply co_implies_same_loc with E co; auto.
    split. 
      apply dom_co_in_evts with co y; auto.
      split. 
      apply ran_co_in_evts with co x; auto.
      split.
      generalize (co_in_pre_egc co si lob Hsiwf); intro Hcoincl.
      generalize (clin_ext_prop (MemC E si) (lift (MemC E si) (pre_egc E co si lob)) gcb); 
      intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso].
      intros Cx Cy HCx HCy Hinx Hiny; apply Hincl; unfold lift; split; auto; split; auto;
      exists x; exists y; split; auto; split; auto; apply _base; right; left; exists y; split; auto.

      apply co_implies_same_loc with E co; auto.
Qed.

Lemma gcb_co_incl (E : set Event) (co si lob : set Event -> Rln Event) gcb :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  clinearisations (lift (MemC E si) (pre_egc E co si lob)) (MemC E si) gcb ->
  rel_incl (gcb_co E (dgcb_loc E (MemC E si) gcb)) (co E).
Proof.
intros Hsiwf Hcowf Hrfwf Hlin x y Hxy.
  generalize Hcowf; intros [Hincl Hlin_co]; destruct_lin (Hlin_co (loc x));
  generalize (clin_ext_prop (MemC E si) (lift (MemC E si) (pre_egc E co si lob)) gcb); intros [Hd1 H2];
  generalize (Hd1 Hlin); intros [? Hlin_gcb]; destruct Hlin_gcb as [Hpart_gcb ?].

  assert (delift (MemC E si) gcb x y) as Hdxy.
    destruct Hxy as [? [? [? [? [? [? [? [Hdxy ?]]]]]]]]; auto.

  assert (x <> y) as Hdiff.
    intro Heq; rewrite <- Heq in Hdxy.
    assert False as Ht.
      apply (delift_irr Hsiwf Hrfwf Hpart_gcb Hdxy).
    inversion Ht.

  assert (Intersection Event E (is_write_same_loc (loc x)) x) as Hx.
    split; [|split]; destruct Hxy as [? [? [? ?]]]; auto.
  assert (Intersection Event E (is_write_same_loc (loc x)) y) as Hy.
    split; [|split]; destruct Hxy as [? [? [? [? [? ?]]]]]; auto.

  generalize (Htot x y Hdiff Hx Hy); intros [|Hco_yx]; auto.
  generalize (co_gcb_incl Hsiwf Hcowf Hrfwf Hlin y x Hco_yx); intro Hgcb_yx.

  assert (delift (MemC E si) gcb x x) as Hxx.
    apply delift_trans with y; auto.
      destruct Hgcb_yx as [? [? [? [? [? [? [? [Hdyx ?]]]]]]]]; auto.

  assert False as Ht.
    apply (delift_irr Hsiwf Hrfwf Hpart_gcb Hxx).
  inversion Ht.
Qed.

Lemma gcb_coeq (E : set Event) (co si lob : set Event -> Rln Event) gcb :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  clinearisations (lift (MemC E si) (pre_egc E co si lob)) (MemC E si) gcb ->
  rel_equal (co E) (gcb_co E (dgcb_loc E (MemC E si) gcb)).
Proof.
intros Hsiwf Hcowf Hrfwf Hlin; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?]; split. 

  apply co_gcb_incl with lob; auto.

  intros x y Hxy; generalize Hcowf; intros [Hincl Hlin_co]; destruct_lin (Hlin_co (loc x));
  generalize (clin_ext_prop (MemC E si) (lift (MemC E si) (pre_egc E co si lob)) gcb); intros [Hd1 H2];
  generalize (Hd1 Hlin); intros [? Hlin_gcb]; destruct Hlin_gcb as [Hpart_gcb ?].

  assert (delift (MemC E si) gcb x y) as Hdxy.
    destruct Hxy as [? [? [? [? [? [? [? [Hdxy ?]]]]]]]]; auto.

  assert (x <> y) as Hdiff.
    intro Heq; rewrite <- Heq in Hdxy.
    assert False as Ht.
      apply (delift_irr Hsiwf Hrfwf Hpart_gcb Hdxy).
    inversion Ht.

  assert (Intersection Event E (is_write_same_loc (loc x)) x) as Hx.
    split; [|split]; destruct Hxy as [? [? [? ?]]]; auto.
  assert (Intersection Event E (is_write_same_loc (loc x)) y) as Hy.
    split; [|split]; destruct Hxy as [? [? [? [? [? ?]]]]]; auto.

  generalize (Htot x y Hdiff Hx Hy); intros [|Hco_yx]; auto.
  generalize (co_gcb_incl Hsiwf Hcowf Hrfwf Hlin y x Hco_yx); intro Hgcb_yx.

  assert (delift (MemC E si) gcb x x) as Hxx.
    apply delift_trans with y; auto.
      destruct Hgcb_yx as [? [? [? [? [? [? [? [Hdyx ?]]]]]]]]; auto.

  assert False as Ht.
    apply (delift_irr Hsiwf Hrfwf Hpart_gcb Hxx).
  inversion Ht.
Qed.

Lemma gcb_rf_incl (E : set Event) (co si lob : set Event -> Rln Event) gcb :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  clinearisations (lift (MemC E si) (pre_egc E co si lob)) (MemC E si) gcb ->
  rel_incl (gcb_rf E (dgcb_loc E (MemC E si) gcb)) (rf E).
Proof.
intros Hswif Hrfwf Hlin; generalize (clin_ext_prop (MemC E si) (lift (MemC E si) (pre_egc E co si lob)) gcb); 
  intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; intros x y Hxy.

  destruct_lin Hlso; destruct Hxy as [HEx [HEy [Hwx [Hry [Hloc [Hval [Hgcb Hnointerv]]]]]]]; 
  split; auto; split; auto; split; auto; split; auto; split; auto.
Qed.

Lemma gcb_rf_is_wf (E : set Event) (co si lob : set Event -> Rln Event) gcb :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  external_visibility E co si lob ->
  clinearisations (lift (MemC E si) (pre_egc E co si lob)) (MemC E si) gcb ->
  gcb_rf_wf E (dgcb_loc E (MemC E si) gcb).
Proof. 
intros Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin r Hr.
generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?].
generalize Hrfwf; intro Hrfwf'; destruct_rf_wf Hrfwf'; generalize (Hex_uni r Hr); intros [[w Hrf] Huni]; 
  generalize (clin_ext_prop (MemC E si) (lift (MemC E si) (pre_egc E co si lob)) gcb); 
  intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso];
  exists w; split; [apply dom_rf_in_evts with r | split; [apply ran_rf_in_evts with w| 
    split; [apply dom_rf_is_write with E r | split; [apply ran_rf_is_read with E w |
            split; [apply rf_implies_same_loc with E | split; [apply rf_implies_same_val with E | split]]]]]]; auto.

  unfold dgcb_loc; unfold delift; split; auto.
    apply dom_rf_in_evts with r; auto.
    split.
    apply ran_rf_in_evts with w; auto.
    split.
    intros Cw Cr HCw HCr Hinw Hinr; apply Hincl; 
    unfold lift; split; auto; split; auto;
    exists w; exists r; split; auto; split; auto;
    apply _base; left; exists r; split; auto.
    apply rf_implies_same_loc with E; auto.

  intros [w' [Hw' [[Hgcbww' Hlocww']  [Hgcbw'r Hlocw'r]]]]. 

  generalize (gcb_co_incl Hsiwf Hcowf Hrfwf Hlin); intro Hcoincl.
  assert (E w) as HEw.
    apply dom_rf_in_evts with r; auto.
  assert (E w') as HEw'. 
    destruct Hgcbww' as [? [HEw' ?]]; auto.
  assert (is_write w) as Hw.
    apply dom_rf_is_write with E r; auto.
  assert (co E w w') as Hcoww'.
    apply Hcoincl; split; auto.
  assert (fr E co r w') as Hfrrw'.
    exists w; split; auto; split; auto.
  assert (dgcb_loc E (MemC E si) gcb r w') as Hgcbrw'.
    unfold dgcb_loc; unfold delift; split; auto.
    apply dom_fr_in_evts with co w'; auto.
    split; auto.
    split; auto.
    intros Cr Cw' HCr HCw' Hinr Hinw'; apply Hincl;
    unfold lift; split; auto; split; auto; 
    exists r; exists w'; split; auto; split; auto;
    apply _base; right; right; left; exists w'; split; auto.

  destruct_lin Hlso.
  assert ((delift (MemC E si) gcb) r r) as Hrr.
    destruct Hgcbw'r as [? [? [? ?]]]; destruct Hgcbrw' as [? [? [? ?]]];
    apply delift_trans with w'; auto.
    apply (delift_irr Hsiwf Hrfwf Hpart Hrr).
Qed.

Lemma rf_gcb_incl (E : set Event) (co si lob : set Event -> Rln Event) gcb :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  external_visibility E co si lob ->
  clinearisations (lift (MemC E si) (pre_egc E co si lob)) (MemC E si) gcb ->
  rel_incl (rf E) (gcb_rf E (dgcb_loc E (MemC E si) gcb)).
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin; intros x y Hxy.
assert (is_read y) as Hry.
  apply ran_rf_is_read with E x; auto.
generalize (gcb_rf_is_wf Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin y Hry);
intros [w Hgcbrfwy];
generalize (gcb_rf_incl Hsiwf Hrfwf Hlin Hgcbrfwy);
intro Hrfwy; destruct_rf_wf Hrfwf;
generalize (Hex_uni y Hry); intros [Hex Huni]; clear Hex_uni;
generalize (Huni x w Hxy Hrfwy); intro Heq; rewrite Heq; auto.
Qed.

Lemma gcb_rfeq (E : set Event) (co si lob : set Event -> Rln Event) gcb :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  external_visibility E co si lob ->
  clinearisations (lift (MemC E si) (pre_egc E co si lob)) (MemC E si) gcb ->
  rel_equal (rf E) (gcb_rf E (dgcb_loc E (MemC E si) gcb)).
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin; 
  generalize (clin_ext_prop (MemC E si) (lift (MemC E si) (pre_egc E co si lob)) gcb); 
  intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; split.
  apply rf_gcb_incl with co lob; auto.
  apply gcb_rf_incl with co lob; auto.
Qed.

Lemma external_global_completion_gcb (E : set Event) (co si lob : set Event -> Rln Event) gcb :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  external_visibility E co si lob ->
  clinearisations (lift (MemC E si) (pre_egc E co si lob)) (MemC E si) gcb ->
  external_global_completion E co si lob gcb.
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin; split; [|split]; auto. 
  apply gcb_is_lin with co; auto.
  apply gcb_rfeq with co lob; auto.
  apply gcb_coeq with lob; auto.
Qed.

(** ** All in all *)
Lemma external_visibility_implies_external_global_completion (E : set Event) (co si lob aob : set Event -> Rln Event) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  (external_visibility E co si lob -> exists gcb, external_global_completion E co si lob gcb).
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis.
generalize (pre_egc_partial_order lob Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis); intro Hpart.
generalize (lift_partial_order Hsiwf Hrfwf Hlobwf Hpart); clear Hpart; intro Hpart.
generalize (corder_ext Hpart); intros [gcb Hgcb]; exists gcb.

  apply external_global_completion_gcb; auto.
Qed. 

(** * External Visibility <-> External Global Completion *)
Theorem external_visibility_gcb_equivalence (E : set Event) (co si lob : set Event -> Rln Event) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  ((external_visibility E co si lob) <-> (exists gcb, external_global_completion E co si lob gcb)).
Proof.
intros Hsiwf Hrfwf Hcowf Hlob_wf Hint_vis.
split.
  apply external_visibility_implies_external_global_completion; auto.
  apply external_global_completion_implies_external_visibility; auto.
Qed.

(** * Global Completion *)

(** ** Global Completion: Definitions *)

Definition fwd (E : set Event) (cb : Rln Event) (w r : Event) := 
  po E w r /\ cb r w /\ ~intervening_write (fun e1 e2 => po E e1 e2 /\ loc e1 = loc e2) w r.
Definition nfwd (E : set Event) (cb : Rln Event) (w r : Event) := 
  cb w r /\ ~intervening_write (fun e1 e2 => cb e1 e2 /\ loc e1 = loc e2) w r.
Definition cb_rf (E : set Event) (cb : Rln Event) (e1 e2 : Event) : Prop :=
  is_write e1 /\ is_read e2 /\ loc e1 = loc e2 /\ val e1 = val e2 /\
  ((fwd E cb e1 e2) \/ (nfwd E cb e1 e2)).

Definition cb_co (E : set Event) (cb : Rln Event) (e1 e2 : Event) : Prop :=
  order_to_co E cb e1 e2.

Definition preorder_cb E si lob :=
  (rel_union (lob E) (scaob E si)).
Definition preorder_cb_lift E si lob C :=
  lift C (preorder_cb E si lob).

Definition global_completion (E : set Event) (co si lob : set Event -> Rln Event) (cb : Rln (Class Event)): Prop :=
  (clinearisations (preorder_cb_lift E si lob (MemC E si)) (MemC E si)) cb /\
  rel_equal (rf E) (cb_rf E (dgcb_loc E (MemC E si) cb)) /\ 
  rel_equal (co E) (cb_co E (dgcb_loc E (MemC E si) cb)).

Definition big_rel3 (E : set Event) (co si lob : set Event -> Rln Event) := 
  tc_mobs_r_mobs E co si (preorder_cb E si lob).

(** ** Global Completion: Lemmas that do _not_ need the existence of a External Global Completion order *)

Lemma rfe_in_cb E si cb :
  rel_equal (rf E) (cb_rf E (dgcb_loc E (MemC E si) cb)) ->
  rel_incl (rfe E) (delift (MemC E si) cb).
Proof.
intros Hrfeq x y [Hrf Hext]; destruct Hrfeq as [Hrfincl ?]; 
generalize (Hrfincl x y Hrf); intros [? [? [? [? Hor]]]].
inversion Hor as [[Hpo ?]| Hnfwd]; clear Hor.

  assert (internal E x y) as Hint.
    destruct Hpo; auto.
  assert False as Ht.
    apply int_ext_contrad with E x y; auto.
  inversion Ht; auto.
 
  destruct Hnfwd as [[? [? [Hxy ?]]] ?]; auto.
Qed.

Lemma co_in_cb E co si cb :
  rel_equal (co E) (cb_co E (dgcb_loc E (MemC E si) cb)) ->
  rel_incl (co E) (delift (MemC E si) cb).
Proof.
intros Hcoeq x y Hco; destruct Hcoeq as [Hcoincl ?]; 
generalize (Hcoincl x y Hco); intros [? [? [? [? [? [? [? [? ?]]]]]]]]; auto.
Qed. 

Lemma fr_in_cb E co si lob cb :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  (clinearisations (preorder_cb_lift E si lob (MemC E si)) (MemC E si)) cb ->
  rel_equal (rf E) (cb_rf E (dgcb_loc E (MemC E si) cb)) ->
  rel_equal (co E) (cb_co E (dgcb_loc E (MemC E si) cb)) ->
  rel_incl (fr E co) (delift (MemC E si) cb).
Proof.
intros Hsiwf Hcowf Hrfwf Hlin Hrfeq Hcoeq x y Hfr.
unfold delift; intros Cx Cy HinCx HinCy Hx Hy.

generalize (erln_is_equiv si Hsiwf Hrfwf); intro Heqr;
unfold MemC in HinCx; generalize (in_class_implies_class_of x Heqr HinCx Hx); intro Heqx1.
unfold MemC in HinCy; generalize (in_class_implies_class_of y Heqr HinCy Hy); intro Heqy1.

generalize (clin_ext_prop (MemC E si) (preorder_cb_lift E si lob (MemC E si)) cb); intros [Himpl ?];
generalize (Himpl Hlin); intros [Hincl Hlso]; destruct_lin Hlso.

assert (Cx <> Cy) as Hdiff.
  rewrite Heqx1; rewrite Heqy1.
  apply not_si_implies_diff_class; auto; apply fr_implies_not_si with co; auto.

generalize (Htot Cx Cy Hdiff HinCx HinCy); intros [? | Hyx]; auto.

generalize Hfr; intros [w [Hw [Hrf Hco]]]; 
generalize (co_in_dgcb co Hcoeq w y Hco); intro Hgcbwy.

generalize Hrfeq; intros Hrfeq';
destruct Hrfeq as [Hinrf ?]; generalize (Hinrf w x Hrf); intros [? [? [? [? [Hfwd | Hnfwd]]]]].

Focus 2.

destruct Hnfwd as [Hcbwx Hnointerv].
assert (loc w = loc y) as Hlocwy.
  apply co_implies_same_loc with E co; auto.
assert (loc x = loc y) as Hlocxy.
  apply fr_implies_same_loc with E co; auto.
assert False as Ht.
  apply Hnointerv; exists y; split; [|split; split]; auto.
    apply ran_co_is_write with E co w; auto.
    split; auto. 
      apply dom_co_in_evts with co y; auto.
      split; auto.
      apply ran_co_in_evts with co w; auto.
      split; auto.
      apply ran_co_in_evts with co w; auto.
      split; auto.
      apply ran_rf_in_evts with w; auto.
      split; auto.
      unfold delift; intros cy cx Hincy Hincx Hcy Hcx;
      unfold MemC in Hincx; generalize (in_class_implies_class_of x Heqr Hincx Hcx); intro Heqx2;
      unfold MemC in Hincy; generalize (in_class_implies_class_of y Heqr Hincy Hcy); intro Heqy2.
      rewrite Heqx2; rewrite Heqy2; rewrite <- Heqx1; rewrite <- Heqy1; auto.
inversion Ht.

destruct Hfwd as [? [[? [? [Hcbxw ?]]] ?]].
assert (MemC E si (class_of (erln E si) w)) as HinCw.
  apply class_of_in_classes; auto.
assert (class_of (erln E si) w w) as Hisw.
  unfold class_of; destruct Heqr as [Hrefl ?]; auto.
unfold delift in Hcbxw; unfold delift in Hgcbwy.
generalize (Hcbxw Cx (class_of (erln E si) w) HinCx HinCw Hx Hisw); intro Hxw.
generalize (Hgcbwy (class_of (erln E si) w) Cy HinCw HinCy Hisw Hy); intro Hwy.
destruct_part Hpart; apply (Htrans Cx (class_of (erln E si) w) Cy Hxw Hwy).
Qed.

Lemma cb_path_ob_dec (E : set Event) (co si lob : set Event -> Rln Event) (cb : Rln Event) (e1 e2 : Event) :
  rel_equal (rf E) (cb_rf E cb) ->
  rel_equal (co E) (cb_co E cb) ->
  ob E co (si E) (lob E) e1 e2 ->
  transitive_closure (rel_seq (obsplus E co) (si E)) e1 e2 \/ big_rel3 E co si lob e1 e2.
Proof.
intros Hrfeq Hcoeq H12.
induction H12 as [e1 e2 [e [Hobs Hsi]] | e1 e2 Hlob |]; auto.
  left; apply _base; exists e; split; auto; apply _base; auto.
  right; apply _base; exists e1; split; [left|exists e2; split; [|left]]; auto; left; auto.
  inversion IHob1 as [Hob1 | Hbr1]; clear IHob1; inversion IHob2 as [Hob2 | Hbr2]; clear IHob2.
    left; apply tc_trans with e; auto.
    right; apply tc_seq_left with e; auto; [intros x y z Hxy Hyz; apply maybe_tc_trans with y | right]; auto.
    right; apply tc_seq_reorg with e; auto; [intros x y z Hxy Hyz; apply maybe_tc_trans with y |right]; auto.
    right; apply tc_trans with e; auto.
Qed.

(** ** Global Completion ->  External Visibility lemmas *)
Lemma big_rel3_irr E co si lob cb x :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  clinearisations (preorder_cb_lift E si lob (MemC E si)) (MemC E si) cb ->
  rel_equal (rf E) (cb_rf E (dgcb_loc E (MemC E si) cb)) ->
  rel_equal (co E) (cb_co E (dgcb_loc E (MemC E si) cb)) ->
  ~(big_rel3 E co si lob x x).
Proof. 
intros Hsiwf Hrfwf Hcowf Hlin Hrfeq Hcoeq; generalize (erln_is_equiv si Hsiwf Hrfwf); intro Herln;
apply tc_mobs_r_mobs_irr with (erln E si) (MemC E si) cb; auto; intros e1 e2 H12.
  apply rfe_in_cb; auto.
  apply co_in_cb with co; auto. 
  apply fr_in_cb with co lob; auto.
  unfold preorder_cb. 
  right; auto.
Qed.

Lemma obsp_si_ac2 E co si lob cb x :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  clinearisations (preorder_cb_lift E si lob (MemC E si)) (MemC E si) cb ->
  rel_equal (rf E) (cb_rf E (dgcb_loc E (MemC E si) cb)) ->
  rel_equal (co E) (cb_co E (dgcb_loc E (MemC E si) cb)) ->
  ~transitive_closure (rel_seq (obsplus E co) (si E)) x x.
Proof.
intros Hsiwf Hrfwf Hcowf Hlin Hrfeq Hcoeq Hx.

assert (erln E si = erln E si) as Heqt.
  auto.
assert ((MemC E si) = classes (erln E si)) as HeqC.
  unfold MemC; auto.
generalize (erln_is_equiv si Hsiwf Hrfwf); intro Hequivr.

assert (rel_incl (rfe E) (delift (MemC E si) cb)) as Hrfeincl.
  intros e1 e2 Hrfe; apply (rfe_in_cb Hrfeq); auto.

generalize (co_in_cb co Hcoeq); intro Hcoincl.
generalize (fr_in_cb Hsiwf Hcowf Hrfwf Hlin Hrfeq Hcoeq); intro Hfrincl.

assert (rel_incl (scaob E si) (preorder_cb E si lob)) as Hscaobincl.
  intros e1 e2 H12; right; auto. 

generalize (tc_obsp_si_in_order Heqt Hequivr HeqC Hsiwf Hrfwf Hcowf Hlin Hrfeincl Hcoincl Hfrincl Hscaobincl Hx); 
generalize (clin_ext_prop (MemC E si) (preorder_cb_lift E si lob (MemC E si)) cb); intros [Himpl _]; generalize (Himpl Hlin); 
clear Himpl; intros [Hincl Hlso]; destruct_lin Hlso; destruct_part Hpart; apply Hirr.
Qed. 

Lemma global_completion_implies_external_visibility (E : set Event) (co si lob : set Event -> Rln Event) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  internal_visibility E co ->
  (exists cb : Rln (Class Event), global_completion E co si lob cb) -> 
  external_visibility E co si lob.
Proof.
intros Hsiwf Hrfwf Hcowf Hintv [cb [Hlin [Hrfeq Hcoeq]]] [x Hx].
generalize (cb_path_ob_dec si lob Hrfeq Hcoeq Hx); intros [Hobs | Hbr3].
  
  apply (obsp_si_ac2 Hsiwf Hrfwf Hcowf Hlin Hrfeq Hcoeq Hobs).

  apply (big_rel3_irr Hsiwf Hrfwf Hcowf Hlin Hrfeq Hcoeq Hbr3); auto.
Qed.

(** ** External Visibility -> Global Completion lemmas *)

Definition pre_gc (E : set Event) (co si lob : set Event -> Rln Event) := 
transitive_closure
  (rel_union (rel_seq (rfe E) (erln E si))
     (rel_union (rel_seq (co E) (erln E si))
        (rel_union (rel_seq (fr E co) (erln E si))
           (preorder_cb E si lob)))).

Definition big_rel4 E co si lob := 
transitive_closure
  (rel_seq
     (maybe
        (transitive_closure
           (rel_seq (erln E si)
              (rel_seq (obsplus E co) (erln E si)))))
     (rel_seq
        (rel_seq (erln E si)
           (rel_seq
              (preorder_cb E si lob)
              (erln E si)))
        (maybe
           (transitive_closure
              (rel_seq (erln E si)
                 (rel_seq (obsplus E co)
                    (erln E si))))))).

Lemma rfe_base_br4_in_br4 E co si lob e1 e0 e x e2 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  rfe E e1 e0 ->
  erln E si e0 e ->
  rel_seq
        (rel_seq (erln E si)
           (rel_seq (preorder_cb E si lob) (erln E si)))
        (maybe
           (transitive_closure
              (rel_seq (erln E si)
                 (rel_seq (obsplus E co) (erln E si))))) e x ->
   maybe
        (transitive_closure
           (rel_seq
              (maybe
                 (transitive_closure
                    (rel_seq (erln E si)
                       (rel_seq (obsplus E co) (erln E si)))))
              (rel_seq
                 (rel_seq (erln E si)
                    (rel_seq (preorder_cb E si lob)
                       (erln E si)))
                 (maybe
                    (transitive_closure
                       (rel_seq (erln E si)
                          (rel_seq (obsplus E co) (erln E si)))))))) x e2 ->
  big_rel4 E co si lob e1 e2.
Proof.
intros Hsiwf Hrfwf H1e0 He0e Hex Hx2; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [Hsym Htrans]];

      inversion Hx2 as [Heqx2 | Htcx2]; clear Hx2.

        rewrite Heqx2 in Hex; clear Heqx2; apply _base; exists e; split; auto;
        right; apply _base; exists e1; split; auto; exists e0; split; auto; left; left; auto. 

        apply _trans with x; auto; exists e; split; auto;
        right; apply _base; exists e1; split; auto; exists e0; split; auto; left; left; auto. 
Qed.

Lemma rf_br4_in_br4 E co si lob e1 e e0 e2 :
  si_well_formed E (si E) ->
  co_well_formed E co -> 
  rf_well_formed E ->
  rfe E e1 e0 ->
  erln E si e0 e ->
  big_rel4 E co si lob e e2 ->
  big_rel4 E co si lob e1 e2. 
Proof.
intros Hsiwf Hcowf Hrfwf H1e0 He0e He2; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [Hsym Htrans]];
generalize (tc_dec2 He2); clear He2; intros [x [[y [Hey Hex]] He2]].

  inversion Hey as [Heqey | Htcey]; clear Hey.
    rewrite <- Heqey in Hex; clear Heqey.

    apply rfe_base_br4_in_br4 with e0 e x; auto.

    inversion He2 as [Heqxe2 | Htcxe2]; clear He2. 
      rewrite Heqxe2 in Hex; clear Heqxe2.
      apply _base; exists y; split; auto; right; apply rf_tc_erln_obsp_erln_in_tc with e e0; auto;
      destruct H1e0; auto.

    apply tc_trans with x; auto.
    apply _base; exists y; split; auto; right; apply rf_tc_erln_obsp_erln_in_tc with e e0; auto;
    destruct H1e0; auto.
Qed.

Lemma co_br4_in_br4 E (co si lob : set Event -> Rln Event) e1 e e0 e2 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  co E e1 e0 ->
  erln E si e0 e ->
  big_rel4 E co si lob e e2 ->
  big_rel4 E co si lob e1 e2.
Proof.
intros Hsiwf Hrfwf Hcowf H1e0 He0e He2; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?].
  apply tc_seq_left with e; auto.

    apply transitive_maybe_tc.

    right; apply _base; exists e1; split; auto;
    exists e0; split; auto; left; right; left; auto.
Qed. 

Lemma fr_br4_in_br4 E (co si lob : set Event -> Rln Event) e1 e e0 e2 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  fr E co e1 e0 ->
  erln E si e0 e ->
  big_rel4 E co si lob e e2 ->
  big_rel4 E co si lob e1 e2. 
Proof.
intros Hsiwf Hrfwf Hcowf H1e0 He0e He2; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?].
  apply tc_seq_left with e; auto.

    apply transitive_maybe_tc.
    
    right; apply _base; exists e1; split; auto;
    exists e0; split; auto; left; right; right; auto.
Qed. 

Lemma preorder_cb_br4_in_br4 E (co si lob : set Event -> Rln Event) e1 e e2 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  preorder_cb E si lob e1 e ->
  big_rel4 E co si lob e e2 ->
  big_rel4 E co si lob e1 e2. 
Proof.
intros Hsiwf Hrfwf Hcowf H1e He2; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?];
induction He2 as [e e2 He2 | e e2 e' Hee'].

  apply _trans with e; [exists e1; split; [left | exists e; split; [|left]]|apply _base]; auto.
    exists e1; split; auto; exists e; split; auto.

  apply tc_trans with e'; auto.
    apply _trans with e; [exists e1; split; [left | exists e; split; [|left]]|apply _base]; auto.
      exists e1; split; auto; exists e; split; auto.
Qed. 

Lemma obs_extra_obsp_eq E co :
  rf_well_formed E ->
  co_well_formed E co ->
  obs_extra E co = obsplus E co.
Proof.
intros Hrfwf Hcowf; 
apply Extensionality_Rlns; split; intros e1 e2 H12.
  inversion H12 as [Hrfe | [Hco | [Hfr | [Hcorfe | Hfrrfe]]]].
    apply _base; left; auto.
    apply _base; right; left; auto.
    apply _base; right; right; auto.
    destruct Hcorfe as [e [Hco Hrfe]]; apply _trans with e; [right; left | apply _base; left]; auto.
    destruct Hfrrfe as [e [Hco Hrfe]]; apply _trans with e; [right; right | apply _base; left]; auto.

  induction H12.
    inversion H as [Hrfe | [Hco | Hfr]].
      left; auto.
      right; left; auto.
      right; right; left; auto.

    inversion H as [Hrfe1e | [Hco1e | Hfr1e]].  

    (*e1,e in rfe*) 
    inversion IHtransitive_closure as [Hrfee2 | [Hcoe2 | [Hfre2 | [Hcorfee2 | Hfrrfee2]]]].
      destruct Hrfe1e as [Hrfe1e ?]; destruct Hrfee2 as [Hrfee2 ?]; 
        generalize (ran_rf_is_read Hrfe1e); intro Hre; generalize (dom_rf_is_write Hrfee2); intro Hwe;
        generalize (read_write_contrad e Hre Hwe); intro Ht; inversion Ht.
      destruct Hrfe1e as [Hrfe1e ?]; 
        generalize (ran_rf_is_read Hrfe1e); intro Hre; generalize (dom_co_is_write e e2 Hcowf Hcoe2); intro Hwe;
        generalize (read_write_contrad e Hre Hwe); intro Ht; inversion Ht.
       destruct Hrfe1e as [Hrfe1e ?]; right; left; apply rf_fr_is_co with e; auto. 
      destruct Hrfe1e as [Hrfe1e ?]; destruct Hcorfee2 as [e' [Hco ?]];
        generalize (ran_rf_is_read Hrfe1e); intro Hre; generalize (dom_co_is_write e e' Hcowf Hco); intro Hwe;
        generalize (read_write_contrad e Hre Hwe); intro Ht; inversion Ht.
      destruct Hrfe1e as [Hrfe1e ?]; destruct Hfrrfee2 as [e' [Hfr ?]];
      generalize (rf_fr_is_co Hrfwf Hrfe1e Hfr); intro Hco; right; right; right; left; exists e'; split; auto.

    (*e1,e2 in co*)
    inversion IHtransitive_closure as [Hrfee2 | [Hcoe2 | [Hfre2 | [Hcorfee2 | Hfrrfee2]]]].
      right; right; right; left; exists e; split; auto.
      right; left; apply co_trans with e; auto.
      generalize (ran_co_is_write e1 e Hcowf Hco1e); intro Hwe; generalize (dom_fr_is_read Hfre2); intro Hre;
      generalize (read_write_contrad e Hre Hwe); intro Ht; inversion Ht.
      destruct Hcorfee2 as [e'[Hco ?]]; generalize (co_trans e1 e e' Hcowf Hco1e Hco); intro Hco1e';
      right; right; right; left; exists e'; split; auto.
      destruct Hfrrfee2 as [e' [Hfr ?]]; 
      generalize (ran_co_is_write e1 e Hcowf Hco1e); intro Hwe; generalize (dom_fr_is_read Hfr); intro Hre.
      generalize (read_write_contrad e Hre Hwe); intro Ht; inversion Ht.

    (*e1,e2 in fr*)
    inversion IHtransitive_closure as [Hrfee2 | [Hcoe2 | [Hfre2 | [Hcorfee2 | Hfrrfee2]]]].
      right; right; right; right; exists e; split; auto.
      right; right; left; apply fr_co_is_fr with e; auto.
      generalize (ran_fr_is_write Hcowf Hfr1e); intro Hwe; generalize (dom_fr_is_read Hfre2); intro Hre;
      generalize (read_write_contrad e Hre Hwe); intro Ht; inversion Ht.
      destruct Hcorfee2 as [e' [Hco Hrfe]]; generalize (fr_co_is_fr e' Hcowf Hfr1e Hco); intro Hfr1e';
      right; right; right; right; exists e'; split; auto.
      destruct Hfrrfee2 as [e' [Hfr Hrfe]]; 
      generalize (ran_fr_is_write Hcowf Hfr1e); intro Hwe; generalize (dom_fr_is_read Hfr); intro Hre;
      generalize (read_write_contrad e Hre Hwe); intro Ht; inversion Ht.
Qed.

Lemma pre_gc_dec (E : set Event) (co si lob : set Event -> Rln Event) (e1 e2 : Event) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  pre_gc E co si lob e1 e2 ->
  transitive_closure (rel_seq (erln E si) (rel_seq (obs_extra E co) (erln E si))) e1 e2 \/ (big_rel4 E co si lob) e1 e2.
Proof.
intros Hsiwf Hrfwf Hcowf H12; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?];
induction H12 as [e1 e2 H12 | e1 e2 b H1b Hb2].
  inversion H12 as [[a [Hrfe Hsi]]  | [[a [Hco Hsi]] | [[a [Hfr Hsi]] | Hlob]]]; clear H12.
    left; apply _base; exists e1; split; auto; exists a; split; auto; left; auto.
    left; apply _base; exists e1; split; auto; exists a; split; auto; right; left; auto.
    left; apply _base; exists e1; split; auto; exists a; split; auto; right; right; left; auto.
    right; apply _base; exists e1; split; [left | exists e2; split; [| left]]; auto.
      exists e1; split; auto. exists e2; split; auto.

  inversion H1b as [[a [Hrfe Hsi]]  | [[a [Hco Hsi]] | [[a [Hfr Hsi]] | Hlob]]]; clear H1b;
  inversion IHHb2 as [Hcpb2 | Hbr4b2]; clear IHHb2.
    left; apply _trans with b; auto; exists e1; split; auto; exists a; split; auto; left; auto. 
    right; apply rf_br4_in_br4 with b a; auto. 
    left; apply _trans with b; auto; exists e1; split; auto; exists a; split; auto; right; left; auto. 
    right; apply co_br4_in_br4 with b a; auto.
    left; apply _trans with b; auto; exists e1; split; auto; exists a; split; auto; right; right; left; auto. 
    right; apply fr_br4_in_br4 with b a; auto.
    right; apply _base; exists e1; split; [left |exists b; split; [|right]]; auto.
      exists e1; split; auto; exists b; split; auto. 
      rewrite <- obs_extra_obsp_eq; auto. 
    right; apply preorder_cb_br4_in_br4 with b; auto.
Qed.

Lemma mobs_extra_trans E (co si lob : set Event -> Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  external_visibility E co si lob ->
  transitive (maybe (obs_extra E co)).
Proof. 
intros Hrfwf Hcowf Hlobwf Hintv Hextv x y z Hxy Hyz; 
inversion Hxy as [Heqxy | Hpcxy]; clear Hxy; inversion Hyz as [Heqyz | Hpcyz]; clear Hyz.
  left; rewrite Heqxy; auto.
  right; rewrite Heqxy; auto.
  right; rewrite <- Heqyz; auto.
  right; inversion Hpcxy as [Hrfexy | [Hcoxy | [Hfrxy | [Hcorfexy | Hfrrfexy]]]]; clear Hpcxy;
         inversion Hpcyz as [Hrfeyz | [Hcoyz | [Hfryz | [Hcorfeyz | Hfrrfeyz]]]]; clear Hpcyz.

    assert False as Ht.
      apply (read_write_contrad y); [apply ran_rf_is_read with E x; destruct Hrfexy | apply dom_rf_is_write with E z; destruct Hrfeyz]; auto.
    inversion Ht.

    assert False as Ht.
      apply (read_write_contrad y); [apply ran_rf_is_read with E x; destruct Hrfexy | 
                                     apply dom_co_is_write with E co z]; auto.
    inversion Ht.

    destruct Hrfexy as [? ?]; right; left; apply rf_fr_is_co with y; auto.

    assert False as Ht.
      apply (read_write_contrad y); [apply ran_rf_is_read with E x; destruct Hrfexy | 
                                       destruct Hcorfeyz as [e [Hye Hez]]; apply dom_co_is_write with E co e]; auto.
    inversion Ht.

    destruct Hrfexy as [? ?]; destruct Hfrrfeyz as [e [Hfr Hrfe]]; right; right; right; left; exists e;
    split; auto; apply rf_fr_is_co with y; auto.

    right; right; right; left; exists y; split; auto.

    right; left; apply co_trans with y; auto.

    assert False as Ht.
      generalize (ran_co_is_write x y Hcowf Hcoxy); intro Hwy; generalize (dom_fr_is_read Hfryz); intro Hry;
      apply (read_write_contrad y Hry Hwy).
    inversion Ht.

    right; right; right; left; destruct Hcorfeyz as [e [Hco Hrfe]]; exists e; split; auto; apply co_trans with y; auto.

    destruct Hfrrfeyz as [e [Hfr Hrfe]]; assert False as Ht.
      generalize (ran_co_is_write x y Hcowf Hcoxy); intro Hwy; generalize (dom_fr_is_read Hfr); intro Hry;
      apply (read_write_contrad y Hry Hwy).
    inversion Ht.

    right; right; right; right; exists y; split; auto.

    right; right; left; apply fr_co_is_fr with y; auto.

    assert False as Ht.
      generalize (ran_fr_is_write Hcowf Hfrxy); intro Hwy; generalize (dom_fr_is_read Hfryz); intro Hry;
      apply (read_write_contrad y Hry Hwy).
    inversion Ht.

    destruct Hcorfeyz as [e [Hco Hrfe]]; right; right; right; right; exists e; split; auto; apply fr_co_is_fr with y; auto.

    destruct Hfrrfeyz as [e [Hfr Hrfe]]; assert False as Ht.
      generalize (ran_fr_is_write Hcowf Hfrxy); intro Hwy; generalize (dom_fr_is_read Hfr); intro Hry;
      apply (read_write_contrad y Hry Hwy).
    inversion Ht.    

    assert False as Ht.
      apply (read_write_contrad y); [destruct Hcorfexy as [e [Hxe [Hey ?]]]; apply ran_rf_is_read with E e | 
                                       apply dom_rf_is_write with E z; destruct Hrfeyz]; auto.
    inversion Ht.

    assert False as Ht.
      apply (read_write_contrad y); [destruct Hcorfexy as [e [Hxe [Hey ?]]]; apply ran_rf_is_read with E e | 
                                       apply dom_co_is_write with E co z]; auto.
    inversion Ht.

    destruct Hcorfexy as [e [Hco Hrfe]]; destruct Hrfe as [? ?]; right; left; apply co_trans with e; auto; apply rf_fr_is_co with y; auto.

    assert False as Ht.
      apply (read_write_contrad y); [destruct Hcorfexy as [e [Hxe [Hey ?]]]; apply ran_rf_is_read with E e | 
                                       destruct Hcorfeyz as [e [Hye Hez]]; apply dom_co_is_write with E co e]; auto.
    inversion Ht.

    destruct Hcorfexy as [e [Hco [Hrfe ?]]]; destruct Hfrrfeyz as [e' [Hfr Hrfe']]; right; right; right; left; exists e'; split; auto;
    apply co_trans with e; auto; apply rf_fr_is_co with y; auto.

    destruct Hfrrfexy as [e [Hfr [Hrf ?]]]; destruct Hrfeyz as [Hrf' ?]; assert False as Ht.
      generalize (dom_rf_is_write Hrf'); intro Hwy; generalize (ran_rf_is_read Hrf); intro Hry;
      apply (read_write_contrad y Hry Hwy).
    inversion Ht.

    destruct Hfrrfexy as [e [Hfr [Hrf ?]]]; assert False as Ht.
      generalize (ran_rf_is_read Hrf); intro Hry; generalize (dom_co_is_write y z Hcowf Hcoyz); intro Hwy; 
      apply (read_write_contrad y Hry Hwy).
    inversion Ht.

    destruct Hfrrfexy as [e [Hfr [Hrf ?]]]; right; right; left; apply fr_co_is_fr with e; auto; apply rf_fr_is_co with y; auto.

    destruct Hfrrfexy as [e [Hfr [Hrf ?]]]; destruct Hcorfeyz as [e' [Hco [Hrf' ?]]]; assert False as Ht. 
      generalize (ran_rf_is_read Hrf); intro Hry; generalize (dom_co_is_write y e' Hcowf Hco); intro Hwy; 
      apply (read_write_contrad y Hry Hwy).
    inversion Ht.

    destruct Hfrrfexy as [e [Hfr [Hrf ?]]]; destruct Hfrrfeyz as [e' [Hfr' Hrfe]]; right; right; right; right; 
    exists e'; split; auto; apply fr_co_is_fr with e; auto; apply rf_fr_is_co with y; auto.
Qed. 

Lemma preorder_cb_seq_mobs_extra_dec E co si lob x y :
  rf_well_formed E ->
  co_well_formed E co ->
  (rel_seq (preorder_cb E si lob) (maybe (obs_extra E co))) x y ->
  rel_seq (maybe (scaob E si))
      (transitive_closure (rel_union (rel_seq (obsplus E co) (maybe (scaob E si))) (lob E))) x y \/ scaob E si x y. 
Proof.
intros Hrfwf Hcowf [e [Hpre [Heq | Hobs_extra]]].
    rewrite Heq in Hpre; clear Heq.
    inversion Hpre as [Hlob | Hscaob]; clear Hpre.
      left; exists x; split; [left| apply _base; right]; auto.

      right; auto.

    inversion Hpre as [Hlob | Hscaob]; clear Hpre.
      left; exists x; split; [left|apply _trans with e; [right|apply _base; left]]; auto.
        exists y; split; [|left]; auto.
        rewrite <- obs_extra_obsp_eq; auto. 

    left; exists e; split.
      right; auto.
      apply _base; left.
      exists y; split; [|left]; auto.
      rewrite <- obs_extra_obsp_eq; auto.
Qed.

Lemma tc_u_scaob_is_tc E co si lob x y z :
  lob_well_formed E si lob ->
  rf_well_formed E ->
  transitive_closure
        (rel_union (rel_seq (obsplus E co) (maybe (scaob E si))) (lob E)) x y ->
  (maybe (scaob E si)) y z ->
  transitive_closure
        (rel_union (rel_seq (obsplus E co) (maybe (scaob E si))) (lob E)) x z.
Proof.
intros Hlobwf Hrfwf Hxy [Heq | Hyz]; [rewrite Heq in Hxy|]; auto.
  induction Hxy.
    inversion H as [[e [Hobsp [Heqe2 | Hscaobe2]]] | Hlob]; clear H.
      rewrite Heqe2 in Hobsp; clear Heqe2; apply _base; left; exists e2; split; auto; right; auto.
      generalize (scaob_scaob_contrad Hrfwf Hscaobe2 Hyz); intro Ht; inversion Ht.

      apply _base; right; destruct_lob_wf Hlobwf; apply Hlob_scaob; auto; exists e2; split; auto.

    clear Hxy; apply _trans with e; auto.
Qed.

Lemma tc_preorder_cb_seq_mobs_extra_dec E co si lob x y :
  lob_well_formed E si lob ->
  rf_well_formed E ->
  co_well_formed E co ->
  transitive_closure
        (rel_seq (preorder_cb E si lob) (maybe (obs_extra E co))) x y ->
  rel_seq (maybe (scaob E si))
      (transitive_closure (rel_union (rel_seq (obsplus E co) (maybe (scaob E si))) (lob E))) x y \/ scaob E si x y. 
Proof.
intros Hlobwf Hrfwf Hcowf Hxy; induction Hxy.

  apply preorder_cb_seq_mobs_extra_dec; auto.

  clear Hxy; generalize (preorder_cb_seq_mobs_extra_dec Hrfwf Hcowf H); clear H; intros [Hseq1e | Hscaob1e];
  inversion IHHxy as [Hseqe2 | Hscaobe2]; clear IHHxy.

    left; destruct Hseq1e as [a [H1a Hae]]; exists a; split; auto; clear H1a.
      destruct Hseqe2 as [b [Heb Hb2]]; apply tc_trans with b; auto.
      apply tc_u_scaob_is_tc with e; auto.

    left; destruct Hseq1e as [a [H1a Hae]]; exists a; split; auto; clear H1a.
    apply tc_u_scaob_is_tc with e; auto; right; auto.

    left; destruct Hseqe2 as [b [[Heqeb | Heb] Hb2]]; exists b; split; auto.
      right; rewrite Heqeb in Hscaob1e; auto.
      generalize (scaob_scaob_contrad Hrfwf Hscaob1e Heb); intro Ht; inversion Ht.

    generalize (scaob_scaob_contrad Hrfwf Hscaob1e Hscaobe2); intro Ht; inversion Ht.
Qed.

Lemma obsp_in_ob E co si lob e1 e2 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  obsplus E co e1 e2 ->
  ob E co (si E) (lob E) e1 e2.
Proof.
intros Hsiwf Hrfwf Hcowf Hobsp; destruct_siwf Hsiwf.
generalize (obsplus_dec Hrfwf Hcowf Hobsp); clear Hobsp;
intros [Hrfe | [Hco | [Hfr | [[e [Hco Hrfe]] | [e [Hfr Hrfe]]]]]].
  apply _obs; exists e2; split; auto; left; auto.
  apply _obs; exists e2; split; auto; right; left; auto.
  apply _obs; exists e2; split; auto; right; right; auto.
  apply _ob with e; apply _obs; [exists e | exists e2]; split; auto.
    right; left; auto. left; auto.
  apply _ob with e; apply _obs; [exists e | exists e2]; split; auto.
    right; right; auto. left; auto.
Qed.

Lemma obsp_scaob_in_ob E co si lob e1 e2 e3 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  obsplus E co e1 e2 ->
  scaob E si e2 e3 ->
  ob E co (si E) (lob E) e1 e3.
Proof.
intros Hsiwf Hrfwf Hcowf Hobsp [Hsi ?]; destruct_siwf Hsiwf.
generalize (obsplus_dec Hrfwf Hcowf Hobsp); clear Hobsp;
intros [Hrfe | [Hco | [Hfr | [[e [Hco Hrfe]] | [e [Hfr Hrfe]]]]]].
  apply _obs; exists e2; split; auto; left; auto.
  apply _obs; exists e2; split; auto; right; left; auto.
  apply _obs; exists e2; split; auto; right; right; auto.
  apply _ob with e; apply _obs; [exists e|exists e2]; split; auto.
    right; left; auto. left; auto.
  apply _ob with e; apply _obs; [exists e | exists e2]; split; auto.
    right; right; auto. left; auto.
Qed.

Lemma u_in_ob E co si lob e1 e2 :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  rel_union (rel_seq (obsplus E co) (maybe (scaob E si))) 
     (lob E) e1 e2 ->
  ob E co (si E) (lob E) e1 e2.
Proof.
intros Hsiwf Hrfwf Hcowf [[e [Hobsp [Heq | Hscaob]]] |Hlob].
    rewrite Heq in Hobsp; clear Heq. 
      apply obsp_in_ob; auto.
      apply obsp_scaob_in_ob with e; auto.
    apply _lob; auto.
Qed.

Lemma tc_in_ob E co si lob x y :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  transitive_closure
       (rel_union (rel_seq (obsplus E co) (maybe (scaob E si)))
          (lob E)) x y ->
  ob E co (si E) (lob E) x y.
Proof.
intros Hsiwf Hrfwf Hcowf Hxy; induction Hxy.
  apply u_in_ob; auto.
  clear Hxy; apply _ob with e; auto; apply u_in_ob; auto.
Qed.

Lemma tc_preorder_cb_seq_mobs_extra_irr E co si lob x:
  si_well_formed E (si E) ->
  lob_well_formed E si lob ->
  rf_well_formed E ->
  co_well_formed E co ->
  external_visibility E co si lob ->
  transitive_closure
        (rel_seq (preorder_cb E si lob) (maybe (obs_extra E co))) x x ->
  False.
Proof.
intros Hsiwf Hlobwf Hrfwf Hcowf Hext_vis Hx; generalize (tc_preorder_cb_seq_mobs_extra_dec Hlobwf Hrfwf Hcowf Hx); clear Hx.
intros [[y [Hxy Htc]] | Hscaob].
  generalize (tc_u_scaob_is_tc Hlobwf Hrfwf Htc Hxy); clear Htc Hxy x; intro Hy;
  apply Hext_vis; exists y. 
    apply tc_in_ob; auto.

  apply scaob_irr with E si x; auto.
Qed.

Lemma preorder_cb_erln_dec (E : set Event) (si lob : set Event -> Rln Event) (e1 e3 : Event) :
  si_well_formed E (si E) ->
  lob_well_formed E si lob ->
  rf_well_formed E ->
  rel_seq (preorder_cb E si lob) (erln E si) e1 e3 ->
  (preorder_cb E si lob) e1 e3.
Proof.
intros Hsiwf Hlobwf Hrfwf [e2 [[Hlob12 | Hscaob12] H23]]; 
generalize Hlobwf; intros Hlobwf'; destruct_lob_wf Hlobwf'.
    left; auto.
      apply Hlob_erln; exists e2; split; auto.
 
    right; auto.
    apply scaob_erln_is_scaob with e2; auto.
Qed.

Lemma tcu_e_is_tcu2 E co si lob a e b :
  si_well_formed E (si E) ->
  lob_well_formed E si lob ->
  rf_well_formed E ->
  transitive_closure
        (rel_union (rel_seq (obsplus E co) (erln E si))
           (preorder_cb E si lob)) a e ->
  erln E si e b ->
  transitive_closure
        (rel_union (rel_seq (obsplus E co) (erln E si))
           (preorder_cb E si lob)) a b.
Proof.
intros Hsiwf Hlobwf Hrfwf Hae Heb; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [Hsym Htrans]].
induction Hae.
  inversion H as [Ho | Hpc]; clear H.
    destruct Ho as [a [H1a Ha2]]; apply _base; left; exists a; split; auto.
      apply Htrans with e2; auto.

    apply _base; right; apply preorder_cb_erln_dec; auto; exists e2; split; auto.
  apply _trans with e; auto.
Qed.

Lemma preorder_cb_in_dec (E : set Event) (co si lob : set Event -> Rln Event) (x y : Event) :
  preorder_cb E si lob x y ->
  rel_seq (maybe (scaob E si)) (ob E co (si E) (lob E)) x y \/ (scaob E si) x y.
Proof.
intros [Hlob | Hsi]; [left; exists x; split | ]; auto.
  left; auto. apply _lob; auto.
Qed.  

Lemma tc_e_in_ob_or_scaob2 (E : set Event) (co si lob : set Event -> Rln Event) (x y z : Event) :
  si_well_formed E (si E) ->
  lob_well_formed E si lob ->
  rf_well_formed E ->
  transitive_closure
        (rel_union (rel_seq (obsplus E co) (erln E si))
           (preorder_cb E si lob)) x y ->
  erln E si y z -> 
  rel_seq (maybe (scaob E si)) (ob E co (si E) (lob E)) x z \/ (scaob E si) x z.
Proof.
intros Hsiwf Hlobwf Hrfwf Hxy Hyz; generalize (tcu_e_is_tcu2 Hsiwf Hlobwf Hrfwf Hxy Hyz); clear Hxy Hyz; intros Hxz.
induction Hxz; [| clear Hxz]. 
  inversion H as [Hobs | Hpc]; clear H; [left; exists e1; split; auto; [left | apply op_e_in_ob]; auto | apply preorder_cb_in_dec]; auto.
  
  inversion H as [Hobs1e | Hpc1e]; clear H; inversion IHHxz as [Hobe2 | Hsie2]; clear IHHxz.
    destruct Hobe2 as [b [[Heqeb | [Hsieb ?]] Hb2]].
      rewrite Heqeb in Hobs1e; clear Heqeb;
      left; exists e1; split; auto; 
        [left|apply _ob with b; auto; apply op_e_in_ob]; auto.
      destruct Hobs1e as [a [H1a [? [? [Hae ?]]]]]; 
      left; exists e1; split; auto; 
        [left|apply _ob with b; auto; apply op_si_in_ob with a; auto; destruct_siwf Hsiwf; apply Htrans with e]; auto.

    destruct Hsie2 as [Hsi ?]; destruct Hobs1e as [a [H1a [? [? [Hae ?]]]]].
      left; exists e1; split; auto; 
        [left | apply op_si_in_ob with a; auto; destruct_siwf Hsiwf; apply Htrans with e]; auto. 

    inversion Hpc1e as [Hlob1e | Hsi1e]; clear Hpc1e; 
      [left; exists e1; split; auto; [left |]|]; auto.
        destruct Hobe2 as [b [[Heqeb | Hscaobeb] Hb2]]; apply _ob with b; auto. 
          rewrite Heqeb in Hlob1e; clear Heqeb; apply _lob; auto.
          destruct_lob_wf Hlobwf; apply _lob; apply Hlob_scaob; exists e; split; auto.

      destruct Hobe2 as [b [[Heqeb | Hscaobeb] Hb2]]. 
        rewrite Heqeb in Hsi1e; clear Heqeb; left; exists b; split; auto; right; auto. 
        generalize (scaob_scaob_contrad Hrfwf Hsi1e Hscaobeb); intro Ht; inversion Ht.
      
      inversion Hpc1e as [Hlob1e | Hsi1e]; clear Hpc1e.
      left; exists e1; split; [left|apply _lob; destruct_lob_wf Hlobwf; apply Hlob_scaob; exists e]; auto.
      generalize (scaob_scaob_contrad Hrfwf Hsi1e Hsie2); intro Ht; inversion Ht.
Qed.

Lemma e_tc_union_irr2 (E : set Event) (co si lob : set Event -> Rln Event) (x : Event) :
  si_well_formed E (si E) ->
  lob_well_formed E si lob ->
  rf_well_formed E ->
  external_visibility E co si lob ->
  rel_seq (erln E si) 
      (transitive_closure (rel_union (rel_seq (obsplus E co) (erln E si)) (preorder_cb E si lob))) x x -> 
  False.
Proof.
intros Hsiwf Hlobwf Hrfwf Hev [y [Hxy Hyx]].
generalize (tc_e_in_ob_or_scaob2 Hsiwf Hlobwf Hrfwf Hyx Hxy); clear Hxy Hyx x; 
  intros [[x [[Heq | Hsi] Hob]] | Hscaob].

  rewrite Heq in Hob; clear Heq; apply Hev; exists x; auto.
  apply Hev; exists x; apply ob_scaob with y; auto.

  apply scaob_irr with E si y; auto.
Qed.

Lemma br4_base_dec (E : set Event) (co si lob : set Event -> Rln Event) (e1 e2 : Event) :
  si_well_formed E (si E) ->
  lob_well_formed E si lob ->
  rf_well_formed E ->
  co_well_formed E co ->
  rel_seq
      (maybe
         (transitive_closure
            (rel_seq (erln E si)
               (rel_seq (obsplus E co) (erln E si)))))
      (rel_seq
         (rel_seq (erln E si)
            (rel_seq (preorder_cb E si lob) (erln E si)))
         (maybe
            (transitive_closure
               (rel_seq (erln E si)
                  (rel_seq (obsplus E co) (erln E si)))))) e1 e2 ->
  rel_seq (erln E si) 
    (transitive_closure (rel_union (rel_seq (obsplus E co) (erln E si)) (preorder_cb E si lob))) e1 e2.
Proof.
intros Hsiwf Hlobwf Hrfwf Hcowf H12; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl [Hsym Htrans]].

  destruct H12 as [x [H1x [y [Hxy Hy2]]]];
  inversion H1x as [Heq1x | Htc1x]; clear H1x;
  inversion Hy2 as [Heqy2 | Htcy2]; clear Hy2.

    rewrite Heq1x; rewrite <- Heqy2; clear Heq1x Heqy2;
    destruct Hxy as [z [Hxz Hzy]]; 
    exists z; split; auto; apply _base; right; 
    apply (preorder_cb_erln_dec Hsiwf Hlobwf Hrfwf Hzy). 

    rewrite Heq1x; clear Heq1x;
    destruct Hxy as [z [Hxz Hzy]]; 
    exists z; split; auto; clear Hxz.
    generalize (tc_dec2 Htcy2); clear Htcy2; intros [e [[d [Hyd Hde]] He2]].
    assert (rel_seq (preorder_cb E si lob) (erln E si) z d) as Hzd.
      destruct Hzy as [z' [Hzz' Hz'y]]; exists z'; split; auto; apply Htrans with y; auto.
        apply _trans with d; [right; apply preorder_cb_erln_dec|apply oe_mtc_eoe_in_tc with e]; auto.

    rewrite Heqy2 in Hxy; clear Heqy2;
    destruct Hxy as [z [Hxz Hz2]];
    generalize (tc_dec2 Htc1x); clear Htc1x; intros [b [[a [H1a Hab]] Hbx]];
    exists a; split; auto;
    apply tc_trans with z; auto.
      generalize (mtc_eoe_e_is_mtc_eoe Hsiwf Hrfwf Hbx Hxz); intros [Hmbz | Heqbz].

      apply oe_mtc_eoe_in_tc with b; auto.
      apply _base; left; destruct Hab as [e [Hae Heb]]; exists e; split; auto; apply Htrans with b; auto.
      apply _base; right; apply preorder_cb_erln_dec; auto.

    destruct Hxy as [z [Hxz Hz2]];
    generalize (tc_dec2 Htc1x); clear Htc1x; intros [b [[a [H1a Hab]] Hbx]];
    generalize (tc_dec2 Htcy2); clear Htcy2; intros [e [[d [Hyd Hde]] He2]].
    exists a; split; auto; clear H1a;
    generalize (mtc_eoe_e_is_mtc_eoe Hsiwf Hrfwf Hbx Hxz); clear Hbx Hxz; intros [Hmbz | Heqbz].

      generalize (oe_mtc_eoe_in_tc (preorder_cb E si lob) Hsiwf Hrfwf Hab Hmbz); clear Hab Hmbz; intro Haz;
      apply tc_trans with z; auto; clear Haz.
      apply _trans with d; auto; [right; apply preorder_cb_erln_dec|]; auto.
        destruct Hz2 as [f [Hzf Hfy]]; exists f; split; auto; apply Htrans with y; auto.
        apply oe_mtc_eoe_in_tc with e; auto.

      apply tc_trans with z; auto.
      apply _base; destruct Hab as [x' [Hax' Hx'b]]; left; exists x'; split; auto; apply Htrans with b; auto.
      apply _trans with d; auto; [right; apply preorder_cb_erln_dec|]; auto.
        destruct Hz2 as [f [Hzf Hfy]]; exists f; split; auto; apply Htrans with y; auto.
        apply oe_mtc_eoe_in_tc with e; auto.
Qed. 

Lemma br4_dec (E : set Event) (co si lob : set Event -> Rln Event) (e1 e2 : Event) :
  si_well_formed E (si E) ->
  lob_well_formed E si lob ->
  rf_well_formed E ->
  co_well_formed E co ->
  (big_rel4 E co si lob) e1 e2 -> 
  rel_seq (erln E si) 
    (transitive_closure (rel_union (rel_seq (obsplus E co) (erln E si)) (preorder_cb E si lob))) e1 e2.
Proof.
intros Hsiwf Hlobwf Hrfwf Hcowf H12; induction H12.

  apply br4_base_dec; auto.
  
  generalize (br4_base_dec Hsiwf Hlobwf Hrfwf Hcowf H); clear H H12; intros [a [H1a Hae]];
  exists a; split; auto; clear H1a;
  destruct IHtransitive_closure as [b [Heb Hb2]]; apply tc_trans with b; auto; clear Hb2.
  apply tcu_e_is_tcu2 with e; auto.
Qed.

Lemma pre_gc_irr (E : set Event) (co si lob : set Event -> Rln Event) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  external_visibility E co si lob ->
  (forall x : Event, ~ pre_gc E co si lob x x).
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis x Hx.
generalize (pre_gc_dec Hsiwf Hrfwf Hcowf Hx); clear Hx; intros [Heoe|Hx].
   rewrite obs_extra_obsp_eq in Heoe; auto.
   generalize (tc_eoe_is_e_seq_tc_oe Hsiwf Hrfwf Heoe); intros [y [Hxy Hyx]];
    apply e_tc_union_irr2 with E co si lob x; auto; exists y; split; auto; 
    apply tc_incl with (rel_seq (obsplus E co) (erln E si)); auto; 
    intros e1 e2 H12; left; auto.

    apply e_tc_union_irr2 with E co si lob x; auto; apply br4_dec; auto.
Qed.

Lemma pre_gc_in_evts (E : set Event) (co si lob : set Event -> Rln Event) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  Included Event (Union Event (dom (pre_gc E co si lob)) (ran (pre_gc E co si lob))) E.
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf; apply r_in_evts_implies_tc_in_evts; intros _x [x Hdom | y Hran].

  destruct Hdom as [e' Hxe']; 
  inversion Hxe' as [[y [[Hrf ?] Hsi]] | [[y [Hco Hsi]]| [[y [Hfr Hsi]] | [Hlob | [Hsi ?]]]]]; clear Hxe'.
    apply dom_rf_in_evts with y; auto.
    apply dom_co_in_evts with co y; auto.
    apply dom_fr_in_evts with co y; auto.
    destruct_lob_wf Hlobwf; apply dom_po_in_evts with e'; auto.
    assert (dom (si E) x) as Hdx.
      exists e'; auto.
    destruct_siwf Hsiwf; generalize (Hdom x Hdx); intros [? ?]; auto.

  destruct Hran as [e' Hxe']; 
  inversion Hxe' as [[x [[Hrf ?] [? [? [Hsi ?]]]]] | [[x [Hco [? [? [Hsi ?]]]]]| [[x [Hfr [? [? [Hsi ?]]]]] | [Hlob | [Hsi ?]]]]]; clear Hxe'.
    assert (ran (si E) y) as Hry.
      exists x; auto.
    destruct_siwf Hsiwf; generalize (Hran y Hry); intros [? ?]; auto.

    assert (ran (si E) y) as Hry.
      exists x; auto.
    destruct_siwf Hsiwf; generalize (Hran y Hry); intros [? ?]; auto.

    assert (ran (si E) y) as Hry.
      exists x; auto.
    destruct_siwf Hsiwf; generalize (Hran y Hry); intros [? ?]; auto.

    destruct_lob_wf Hlobwf; apply ran_po_in_evts with e'; auto.

    assert (ran (si E) y) as Hry.
        exists e'; auto.
      destruct_siwf Hsiwf; generalize (Hran y Hry); intros [? ?]; auto.
Qed. 

Lemma pre_gc_partial_order (E : set Event) (co si lob : set Event -> Rln Event) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  external_visibility E co si lob ->
  partial_order (pre_gc E co si lob) E.
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis; split; [|split].
apply pre_gc_in_evts; auto.

intros e1 e2 e3 H12 H23; apply tc_trans with e2; auto.
apply pre_gc_irr; auto.
Qed.

Lemma cb_is_lin (E : set Event) (co si lob : set Event -> Rln Event) (cb : Rln (Class Event)) :
  clinearisations (lift (MemC E si) (pre_gc E co si lob)) (MemC E si) cb ->
  clinearisations (preorder_cb_lift E si lob (MemC E si)) (MemC E si) cb.
Proof.
intros Hlin; apply clin_of_big_is_clin_of_little with (lift (MemC E si) (pre_gc E co si lob)); auto.
apply rel_incl_lift; auto.
intros x y Hxy; apply _base; right; right; right; auto.
Qed. 

Lemma co_in_pre_gc E co si lob :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  rel_incl (co E) (pre_gc E co si lob).
Proof.
intros Hsiwf Hrfwf x y Hxy; apply _base; right; left; auto.
generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?].
exists y; split; auto.
Qed. 

Lemma co_cb_incl (E : set Event) (co si lob : set Event -> Rln Event) cb :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  clinearisations (lift (MemC E si) (pre_gc E co si lob)) (MemC E si) cb ->
  rel_incl (co E) (cb_co E (dgcb_loc E (MemC E si) cb)).
Proof.
intros Hsiwf Hcowf Hrfwf Hlin; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?]. 

  intros x y Hxy; split; [| split; [|split; [|split; [|split]]]].
    apply dom_co_in_evts with co y; auto.
    apply ran_co_in_evts with co x; auto.
    apply dom_co_is_write with E co y; auto.
    apply ran_co_is_write with E co x; auto.
    apply co_implies_same_loc with E co; auto.
    split. 
      apply dom_co_in_evts with co y; auto.
      split. 
      apply ran_co_in_evts with co x; auto.
      split.  
      generalize (co_in_pre_gc co si lob Hsiwf); intro Hcoincl.
      generalize (clin_ext_prop (MemC E si) (lift (MemC E si) (pre_gc E co si lob)) cb); 
      intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso].
      intros Cx Cy HCx HCy Hinx Hiny; apply Hincl; unfold lift; split; auto; split; auto;
      exists x; exists y; split; auto; split; auto; apply _base; right; left; auto; exists y; split; auto.

      apply co_implies_same_loc with E co; auto.
Qed.

Lemma cb_co_incl (E : set Event) (co si lob : set Event -> Rln Event) cb :
  si_well_formed E (si E) ->
  co_well_formed E co ->
  rf_well_formed E ->
  clinearisations (lift (MemC E si) (pre_gc E co si lob)) (MemC E si) cb ->
  rel_incl (gcb_co E (dgcb_loc E (MemC E si) cb)) (co E).
Proof.
intros Hsiwf Hcowf Hrfwf Hlin x y Hxy.
  generalize Hcowf; intros [Hincl Hlin_co]; destruct_lin (Hlin_co (loc x));
  generalize (clin_ext_prop (MemC E si) (lift (MemC E si) (pre_gc E co si lob)) cb); intros [Hd1 H2];
  generalize (Hd1 Hlin); intros [? Hlin_cb]; destruct Hlin_cb as [Hpart_cb ?].

  assert (delift (MemC E si) cb x y) as Hdxy.
    destruct Hxy as [? [? [? [? [? [? [? [Hdxy ?]]]]]]]]; auto.

  assert (x <> y) as Hdiff.
    intro Heq; rewrite <- Heq in Hdxy.
    assert False as Ht.
      apply (delift_irr Hsiwf Hrfwf Hpart_cb Hdxy).
    inversion Ht.

  assert (Intersection Event E (is_write_same_loc (loc x)) x) as Hx.
    split; [|split]; destruct Hxy as [? [? [? ?]]]; auto.
  assert (Intersection Event E (is_write_same_loc (loc x)) y) as Hy.
    split; [|split]; destruct Hxy as [? [? [? [? [? ?]]]]]; auto.

  generalize (Htot x y Hdiff Hx Hy); intros [|Hco_yx]; auto.
  generalize (co_cb_incl Hsiwf Hcowf Hrfwf Hlin y x Hco_yx); intro Hcb_yx.

  assert (delift (MemC E si) cb x x) as Hxx.
    apply delift_trans with y; auto.
      destruct Hcb_yx as [? [? [? [? [? [? [? [Hdyx ?]]]]]]]]; auto.

  assert False as Ht.
    apply (delift_irr Hsiwf Hrfwf Hpart_cb Hxx).
  inversion Ht.
Qed.

Lemma cb_coeq (E : set Event) (co si lob : set Event -> Rln Event) cb :
  si_well_formed E (si E) ->
  co_well_formed E co -> 
  rf_well_formed E ->
  clinearisations (lift (MemC E si) (pre_gc E co si lob)) (MemC E si) cb ->
  rel_equal (co E) (cb_co E (dgcb_loc E (MemC E si) cb)).
Proof.
intros Hsiwf Hcowf Hrfwf Hlin; generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?]; split. 

  apply co_cb_incl with lob; auto.

  apply cb_co_incl with lob; auto.
Qed. 

Lemma cb_rf_incl (E : set Event) (co si lob : set Event -> Rln Event) (cb : Rln (Class Event)) :
  clinearisations (lift (MemC E si) (pre_gc E co si lob)) (MemC E si) cb ->
  rel_incl (cb_rf E (dgcb_loc E (MemC E si) cb)) (rf E).
Proof.
intros Hlin; generalize (clin_ext_prop (MemC E si) (lift (MemC E si) (pre_gc E co si lob)) cb); 
  intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; intros x y Hxy.

  destruct_lin Hlso; destruct_part Hpart; destruct Hxy as [Hwx [Hry [Hloc [Hval [Hfwd | Hnfwd]]]]].

  destruct Hfwd as [Hpo [Hcbyx Hnointerv]].
    split; auto; split; auto; split; auto; split; auto; split; auto. 
    apply dom_po_in_evts with y; auto. 
    apply ran_po_in_evts with x; auto.

  destruct Hnfwd as [Hcb Hnoniterv].
    split; auto; split; auto; split; auto; split; auto; split; auto;
    destruct Hcb as [? [? ?]]; auto.
Qed. 

Definition cb_rf_wf (E : set Event) (si : set Event -> Rln Event) (cb : Rln (Class Event)) :=
   forall r : Event, is_read r -> exists w : Event, cb_rf E (dgcb_loc E (MemC E si) cb) w r.

Lemma rf_implies_diff_classes E si w r :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  rf E w r ->
  class_of (erln E si) w <> class_of (erln E si) r.
Proof.
intros Hsiwf Hrfwf Hwr; generalize (erln_is_equiv si Hsiwf Hrfwf); intro Heqr.
generalize (dom_rf_is_write Hwr); intro Hw.
generalize (ran_rf_is_read Hwr); intro Hr.
generalize (class_of_refl w Heqr); intros HCw Heq; rewrite Heq in HCw; clear Heq.
destruct HCw as [? [? [? Hor]]]; 
  inversion Hor as [[Hiswr ?] | [[? [x [Hrfw ?]]] | [w1 [w2 [? [[Hrf ?] ?]]]]]]; clear Hor.
  apply read_write_contrad with r; auto.
  apply read_write_contrad with w; auto.
    apply ran_rf_is_read with E x; auto.
  apply read_write_contrad with w; auto.
    apply ran_rf_is_read with E w2; auto.
Qed.

Lemma cb_rf_is_wf (E : set Event) (co si lob : set Event -> Rln Event) (cb : Rln (Class Event)) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  external_visibility E co si lob ->
  clinearisations (lift (MemC E si) (pre_gc E co si lob)) (MemC E si) cb ->
  cb_rf_wf E si cb.
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin r Hr;
generalize (erln_is_equiv si Hsiwf Hrfwf); intros [Hrefl ?];
unfold gcb_rf.
generalize Hrfwf; intro Hrfwf'; destruct_rf_wf Hrfwf'; generalize (Hex_uni r Hr); intros [[w Hrf] Huni]; 
  generalize (clin_ext_prop (MemC E si) (lift (MemC E si) (pre_gc E co si lob)) cb); 
  intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso];
  exists w; split; [apply dom_rf_is_write with E r | split; [apply ran_rf_is_read with E w |
            split; [apply rf_implies_same_loc with E | split; [apply rf_implies_same_val with E | ]]]]; auto.

  assert (E w) as HEw.
    apply dom_rf_in_evts with r; auto.
  assert (E r) as HEr.
    apply ran_rf_in_evts with w; auto.

  generalize (int_or_ext E w r HEw HEr); intros [Hint | Hext].

  Focus 2.

  assert (dgcb_loc E (MemC E si) cb w r) as Hcbwr.
    split; auto; split; auto; split; auto.
    unfold delift; intros Cw Cr HinCw HinCr HCw HCr; 
    apply Hincl; unfold lift; split; auto; split; auto;   
    exists w; exists r; split; auto; split; auto;
    left; left; exists r; split; auto; split; auto.
      apply rf_implies_same_loc with E; auto.

  right; split; auto.

    intros [w' [Hw' [[Hcbww' Hlocww'] [Hcbw'r Hlocw'r]]]].
    assert (E w') as HEw'.
      destruct Hcbw'r; auto.
    generalize (cb_co_incl Hsiwf Hcowf Hrfwf Hlin); intro Hcoincl.
    assert (is_write w) as Hw.
      apply dom_rf_is_write with E r; auto.
    assert (co E w w') as Hcoww'.
      apply Hcoincl; split; auto; split; auto.
    assert (fr E co r w') as Hfrrw'.
      exists w; split; auto.
    assert (dgcb_loc E (MemC E si) cb r w') as Hcbrw'.
      split; auto; split; auto; split; auto.
      unfold delift; intros Cr Cw' HinCr HinCw' HCr HCw'.
      apply Hincl; unfold lift; split; auto; split; auto.
    exists r; exists w'; split; auto; split; auto; 
      left; right; right; left; auto; exists w'; split; auto.
    destruct_lin Hlso; destruct_part Hpart. 
    apply Hirr with (class_of (erln E si) w'); apply Htrans with (class_of (erln E si) r).
      destruct Hcbw'r as [? [? [Hd ?]]]; apply Hd. 
        apply class_of_in_classes; auto; apply erln_is_equiv; auto.
        apply class_of_in_classes; auto; apply erln_is_equiv; auto.
        apply class_of_refl; auto; apply erln_is_equiv; auto.
        apply class_of_refl; auto; apply erln_is_equiv; auto.

      destruct Hcbrw' as [? [? [Hd ?]]]; apply Hd. 
        apply class_of_in_classes; auto; apply erln_is_equiv; auto.
        apply class_of_in_classes; auto; apply erln_is_equiv; auto.
        apply class_of_refl; auto; apply erln_is_equiv; auto.
        apply class_of_refl; auto; apply erln_is_equiv; auto.

  assert (cb (class_of (erln E si) w) (class_of (erln E si) r) \/ 
          cb (class_of (erln E si) r) (class_of (erln E si) w)) as Hor.
    destruct_lin Hlso; apply Htot; auto. 
      apply rf_implies_diff_classes; auto.

        apply class_of_in_classes; auto; apply erln_is_equiv; auto.
        apply class_of_in_classes; auto; apply erln_is_equiv; auto.

  inversion Hor as [Hcbwr | Hcbrw]; clear Hor; [right|left]; split; auto.

    split; auto; split; auto; split; auto.
      unfold delift; intros Cw Cr HinCw HinCr HCw HCr. 
    destruct HinCr as [cr HinCr]; assert (Cr = class_of (erln E si) r) as HeqCr.
      rewrite HinCr; apply equiv_elts_have_equal_classes; auto.
        apply erln_is_equiv; auto.
        rewrite HinCr in HCr; unfold class_of in HCr; auto.
    destruct HinCw as [cw HinCw]; assert (Cw = class_of (erln E si) w) as HeqCw.
      rewrite HinCw; apply equiv_elts_have_equal_classes; auto.
        apply erln_is_equiv; auto.
        rewrite HinCw in HCw; unfold class_of in HCw; auto.
    rewrite HeqCr; rewrite HeqCw; auto.

      apply rf_implies_same_loc with E; auto.

    intros [w' [Hw' [[Hcbww' Hlocww'] [Hcbw'r Hlocw'r]]]].
    assert (E w') as HEw'.
      destruct Hcbw'r; auto.
    generalize (cb_co_incl Hsiwf Hcowf Hrfwf Hlin); intro Hcoincl.
    assert (is_write w) as Hw.
      apply dom_rf_is_write with E r; auto.
    assert (co E w w') as Hcoww'.
      apply Hcoincl; split; auto; split; auto.
    assert (fr E co r w') as Hfrrw'.
      exists w; split; auto.
    assert (dgcb_loc E (MemC E si) cb r w') as Hcbrw'.
      split; auto; split; auto; split; auto.
      unfold delift; intros Cr Cw' HinCr HinCw' HCr HCw'.
      apply Hincl; unfold lift; split; auto; split; auto.
    exists r; exists w'; split; auto; split; auto; 
      left; right; right; left; auto; exists w'; split; auto.
    destruct_lin Hlso; destruct_part Hpart. 
    apply Hirr with (class_of (erln E si) w'); apply Htrans with (class_of (erln E si) r).
      destruct Hcbw'r as [? [? [Hd ?]]]; apply Hd. 
        apply class_of_in_classes; auto; apply erln_is_equiv; auto.
        apply class_of_in_classes; auto; apply erln_is_equiv; auto.
        apply class_of_refl; auto; apply erln_is_equiv; auto.
        apply class_of_refl; auto; apply erln_is_equiv; auto.

      destruct Hcbrw' as [? [? [Hd ?]]]; apply Hd. 
        apply class_of_in_classes; auto; apply erln_is_equiv; auto.
        apply class_of_in_classes; auto; apply erln_is_equiv; auto.
        apply class_of_refl; auto; apply erln_is_equiv; auto.
        apply class_of_refl; auto; apply erln_is_equiv; auto.

  apply rfi_implies_po with co; auto; split; auto. 
  split; auto.
    split; auto; split; auto; split; auto.
    unfold delift; intros Cr Cw HinCr HinCw HCr HCw.
    destruct HinCr as [cr HinCr]; assert (Cr = class_of (erln E si) r) as HeqCr.
      rewrite HinCr; apply equiv_elts_have_equal_classes; auto.
        apply erln_is_equiv; auto.
        rewrite HinCr in HCr; unfold class_of in HCr; auto.
    destruct HinCw as [cw HinCw]; assert (Cw = class_of (erln E si) w) as HeqCw.
      rewrite HinCw; apply equiv_elts_have_equal_classes; auto.
        apply erln_is_equiv; auto.
        rewrite HinCw in HCw; unfold class_of in HCw; auto.
    rewrite HeqCr; rewrite HeqCw; auto.
    generalize (rf_implies_same_loc Hrf); auto.

  intros [w' [Hw' [[Hpoww' Hlocww'] [Hpow'r Hlocw'r]]]].
    assert (is_write w) as Hw.
      apply dom_rf_is_write with E r; auto.
    assert (co E w w') as Hcoww'.
      apply posWW_is_coi; auto.
    assert (fr E co r w') as Hfrrw'.
      exists w; split; auto.
    apply Hint_vis; exists w'; apply _trans with r; auto; [right; right; right; split|apply _base]; auto.
Qed.

Lemma rf_cb_incl (E : set Event) (co si lob : set Event -> Rln Event) (cb : Rln (Class Event)) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  external_visibility E co si lob ->
  clinearisations (lift (MemC E si) (pre_gc E co si lob)) (MemC E si) cb ->
  rel_incl (rf E) (cb_rf E (dgcb_loc E (MemC E si) cb)).
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin; intros x y Hxy.
assert (is_read y) as Hry.
  apply ran_rf_is_read with E x; auto.
generalize (cb_rf_is_wf Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin y Hry);
intros [w Hcbrfwy];
generalize (cb_rf_incl Hlin Hcbrfwy);
intro Hrfwy; destruct_rf_wf Hrfwf;
generalize (Hex_uni y Hry); intros [Hex Huni]; clear Hex_uni;
generalize (Huni x w Hxy Hrfwy); intro Heq; rewrite Heq; auto.
Qed.

Lemma cb_rfeq (E : set Event) (co si lob : set Event -> Rln Event) (cb : Rln (Class Event)) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  external_visibility E co si lob ->
  clinearisations (lift (MemC E si) (pre_gc E co si lob)) (MemC E si) cb ->
  rel_equal (rf E) (cb_rf E (dgcb_loc E (MemC E si) cb)).
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin; generalize (clin_ext_prop (MemC E si) (lift (MemC E si) (pre_gc E co si lob)) cb); 
  intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; split. 
  apply rf_cb_incl with co lob; auto.
  apply cb_rf_incl with co lob; auto.  
Qed.

Lemma global_completion_cb (E : set Event) (co si lob : set Event -> Rln Event) (cb : Rln (Class Event)) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  external_visibility E co si lob ->
  clinearisations (lift (MemC E si) (pre_gc E co si lob)) (MemC E si) cb ->
  global_completion E co si lob cb.
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin; split; [|split]; auto.
  apply cb_is_lin with co; auto.
  apply cb_rfeq with co lob; auto.
  apply cb_coeq with lob; auto.
Qed.

Lemma udr_lift2 E co si lob :
  Included (Class Event)
    (Union (Class Event)
       (dom (lift (MemC E si) (pre_gc E co si lob)))
       (ran (lift (MemC E si) (pre_gc E co si lob))))
    (MemC E si).
Proof.
unfold lift;
intros ? [Cx [Cy [? ?]] | Cy [Cx [? [? ?]]]]; auto.
Qed. 

Lemma pre_gc_erln_is_pre_gc E co si lob x y z :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  lob_well_formed E si lob ->
  pre_gc E co si lob x y ->
  erln E si y z ->
  pre_gc E co si lob x z.
Proof. 
intros Hsiwf Hrfwf Hlobwf Hxy Hyz; induction Hxy as [x y Hb | x e3 y Htc].
Focus 2.
  apply _trans with y; auto; unfold pre_gc in IHHxy; generalize (IHHxy Hyz); intro Ht; auto.
 
  assert (si E y z) as Hsi.
    destruct Hyz as [? [? [? ?]]]; auto.
  generalize (erln_is_equiv si Hsiwf Hrfwf); intros [? [? Htrans]];
  inversion Hb as [Hrfsi | [Hcosi | [Hfrsi | Hpc]]].
    
    destruct Hrfsi as [e [Hxe Hey]]; apply _base; left; exists e; split; auto.
    apply Htrans with y; auto.

    destruct Hcosi as [e [Hxe Hey]]; apply _base; right; left; exists e; split; auto;
    apply Htrans with y; auto.    

    destruct Hfrsi as [e [Hxe Hey]]; apply _base; right; right; left; exists e; split; auto;
    apply Htrans with y; auto.

    generalize Hlobwf; intro Hlobwf'; destruct_lob_wf Hlobwf'; 
    inversion Hpc as [Hlob | Hscaob]; clear Hpc; apply _base; right; right; right. 
      left. 
        apply Hlob_erln; exists y; split; auto.

      right; apply scaob_erln_is_scaob with y; auto.
Qed. 

Lemma trans_lift2 E co si lob :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  lob_well_formed E si lob ->
  partial_order (pre_gc E co si lob) E ->
  transitive (lift (MemC E si) (pre_gc E co si lob)).
Proof.
intros Hsiwf Hrfwf Hlobwf Hpart; destruct_part Hpart.
  unfold lift; intros Cx Cy Cz [HCx [HCy [x [y [Hx [Hy Hxy]]]]]] [? [HCz [y' [z [Hy' [Hz Hy'z]]]]]];
  split; auto; split; auto; exists x; exists z; split; auto; split; auto.
    apply Htrans with y'; auto.
    unfold MemC in HCy; unfold classes in HCy; unfold class_of in HCy; destruct HCy as [y0 HCy];
    rewrite HCy in Hy; rewrite HCy in Hy'.

    assert (erln E si y y') as Hyy'.
      generalize (erln_is_equiv si Hsiwf Hrfwf); intro Hequiv; clear Htrans; destruct_eqrln Hequiv;
      apply Htrans with y0; auto.

    apply pre_gc_erln_is_pre_gc with y; auto.
Qed.

Lemma irr_lift2 E co si lob Cx :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  lob_well_formed E si lob ->
  partial_order (pre_gc E co si lob) E ->
  ~ lift (MemC E si) (pre_gc E co si lob) Cx Cx.
Proof.
  intros Hsiwf Hrfwf Hlobwf Hpart; destruct_part Hpart.
  unfold lift; intros [HCx [? [e1 [e2 [H1 [H2 H12]]]]]].
  unfold MemC in HCx; unfold classes in HCx; unfold class_of in HCx; destruct HCx as [x0 HCx];
  rewrite HCx in H1; rewrite HCx in H2. 
    assert (erln E si e2 e1) as Hyy'.
      generalize (erln_is_equiv si Hsiwf Hrfwf); intro Hequiv; clear Htrans; destruct_eqrln Hequiv;
      apply Htrans with x0; auto.
  apply Hirr with e1; apply pre_gc_erln_is_pre_gc with e2; auto.
Qed.

Lemma lift_partial_order2 E co si lob :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  lob_well_formed E si lob ->
  partial_order (pre_gc E co si lob) E ->
  partial_order (lift (MemC E si) (pre_gc E co si lob)) (MemC E si). 
Proof.
intros Hsiwf Hrfwf Hlobwf Hpart; split; [|split].

  apply udr_lift2; auto.

  apply trans_lift2; auto.

  intros Cx; apply irr_lift2; auto.
Qed.

Lemma external_visibility_implies_global_completion (E : set Event) (co si lob : set Event -> Rln Event) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  external_visibility E co si lob -> 
  (exists cb, global_completion E co si lob cb).
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hintv Hextv. 
generalize (pre_gc_partial_order Hsiwf Hrfwf Hcowf Hlobwf Hintv Hextv); intro Hpart;
generalize (lift_partial_order2 Hsiwf Hrfwf Hlobwf Hpart); clear Hpart; intro Hpart.
generalize (corder_ext Hpart); intros [cb Hcb]; exists cb.
  apply global_completion_cb; auto.
Qed.

(** ** External Visibility <-> Global Completion **)
Theorem external_visibility_cb_equivalence (E : set Event) (co si lob : set Event -> Rln Event) :
  si_well_formed E (si E) ->
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E si lob ->
  internal_visibility E co ->
  (external_visibility E co si lob <-> (exists cb, global_completion E co si lob cb)).
Proof.
intros Hsiwf Hrfwf Hcowf Hlobwf Hint_vis.
split.
  apply external_visibility_implies_global_completion; auto.
  apply global_completion_implies_external_visibility; auto.
Qed.
