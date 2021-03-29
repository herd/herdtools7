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

Hypothesis order_ext : forall E r,
  partial_order r E ->
  (exists lin_ext, (linearisations r E) lin_ext).

Hypothesis lin_ext_prop : forall E r lin_ext,
  (linearisations r E) lin_ext <->
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
  forall (l : Location),
    linear_strict_order (co E) (Intersection _ E (is_write_same_loc l)).

Definition rfi (E : set Event) (e1 e2 : Event) : Prop := rf E e1 e2 /\ internal E e1 e2.
Definition coi (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) : Prop := co E e1 e2 /\ internal E e1 e2.
Definition fri (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) : Prop :=
  fr E co e1 e2 /\ internal E e1 e2.

Definition rfe (E : set Event) (e1 e2 : Event) : Prop := rf E e1 e2 /\ external E e1 e2.
Definition coe (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) : Prop := co E e1 e2 /\ external E e1 e2.
Definition fre (E : set Event) (co : set Event -> Rln Event) (e1 e2 : Event) : Prop :=
  fr E co e1 e2 /\ external E e1 e2.

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

Definition complus E co e1 e2 := rf E e1 e2 \/ co E e1 e2 \/ fr E co e1 e2 \/
  rel_seq (co E) (rf E) e1 e2 \/ rel_seq (fr E co) (rf E) e1 e2.

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

Lemma fr_implies_diff (E : set Event) (co : set Event -> Rln Event) (gcb : Rln Event) (e1 e2 : Event) :
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

Inductive ob (E : set Event) (co : set Event -> Rln Event) (lob : Rln Event) (e1 e2 : Event) : Prop :=
  | _obs : obs E co e1 e2 -> ob E co lob e1 e2
  | _lob : lob e1 e2 -> ob E co lob e1 e2
  | _ob : forall e, ob E co lob e1 e -> ob E co lob e e2 -> ob E co lob e1 e2.

Definition external_visibility (E : set Event) (co : set Event -> Rln Event) (lob : set Event -> Rln Event) : Prop :=
  irreflexive (ob E co (lob E)).

(** Well-formed lob: a relation lob over a set of events E is well-formed when:
    - lob is irreflexive
    - lob is transitive
    - lob starts with a write and ends with a read or write memory event
    - lob is included in po *)

Definition lob_well_formed (E:set Event) (lob : set Event -> Rln Event) :=
  irreflexive (lob E) /\
  transitive (lob E) /\
  (forall e1 e2, lob E e1 e2 -> is_write e1 \/ is_read e1) /\
  rel_incl (lob E) (po E).
Ltac destruct_lob_wf H := destruct H as [Hirr_lob [Htrans_lob [Hdom_lob Hincl_po]]].

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
intros Hrfwf Hcowf Hintv Hrx Hwy Hpoxy Hlocxy; generalize Hcowf; intros [Hincl Hlin]; split.
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
intros Hcowf Hintv Hisw Hisw' Hpoww' Hlocww'; generalize Hcowf; intros [Hincl Hlin].
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
  is_write e1 /\ is_write e2 /\ loc e1 = loc e2 /\ o e1 e2.

Lemma co_in_order E co o x y :
  rel_equal (co E) (order_to_co E o) ->
  co E x y ->
  o x y.
Proof.
intros Hcoeq Hco; destruct Hcoeq as [Hcoincl ?]; generalize (Hcoincl x y Hco); intros [? [? [? ?]]]; auto.
Qed.

Lemma co_order_incl (E : set Event) (co : set Event -> Rln Event) (r o : Rln Event) :
  co_well_formed E co ->
  linearisations r E o ->
  rel_incl (co E) r ->
  rel_incl (co E) (order_to_co E o).
Proof.
intros Hcowf Hlin Hcor; generalize (lin_ext_prop E r o);
  intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; intros x y Hxy.
split; [apply dom_co_is_write with E co y | split; [apply ran_co_is_write with E co x |
        split; [apply co_implies_same_loc with E co |apply Hincl; apply Hcor]]]; auto.
Qed.

(*
Lemma order_co_incl (E : set Event) (r o : Rln Event) :
  linearisations r E o ->
  rel_incl (order_to_co E o) (co E).
Proof.
intros Hlin; generalize (lin_ext_prop E r o);
  intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; intros x y Hxy.

  destruct_lin Hlso; destruct_part Hpart; destruct Hxy as [Hwx [Hwy [Hloc Hgcb]]];
  split; auto; split; auto; split; auto; split; auto;
  apply Hinc; auto; [left; exists y | right; exists x]; auto.
Qed.

Lemma order_coeq (E : set Event) (r o : Rln Event) :
  linearisations r E o ->
  rel_incl (co E) r ->
  rel_equal (co E) (order_to_co E o).
Proof.
intros Hlin Hcor; split.
  apply co_order_incl with r; auto.
  apply order_co_incl with r; auto.
Qed.
*)

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

Lemma mop_in_order E co r o x y :
  rf_well_formed E ->
  co_well_formed E co ->
  linearisations r E o ->
  rel_incl (rfe E) o ->
  rel_incl (co E) o ->
  rel_incl (fr E co) o ->
  maybe (obsplus E co) x y ->
  x = y \/ o x y.
Proof.
intros Hrfwf Hcowf Hlin Hrfeincl Hcoincl Hfrincl [? | Hxy]; auto; right.
generalize (obsplus_dec Hrfwf Hcowf Hxy); clear Hxy;
generalize (lin_ext_prop E r o); intros [Himpl _];
generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; destruct_lin Hlso; destruct_part Hpart;
intros [Hrfe | [Hco | [Hfr | [[z [Hco Hrfe]] | [z [Hfr Hrfe]]]]]].

  apply Hrfeincl; auto.
  apply Hcoincl; auto.
  apply Hfrincl; auto.
  apply Htrans with z; [apply Hcoincl | apply Hrfeincl]; auto.
  apply Htrans with z; [apply Hfrincl | apply Hrfeincl]; auto.
Qed.

Definition mobs_r_mobs E co r :=
  rel_seq (maybe (obsplus E co)) (rel_seq r (maybe (obsplus E co))).
 Definition tc_mobs_r_mobs E co r := transitive_closure (mobs_r_mobs E co r).

Lemma mobs_r_mobs_in_order E co r o e1 e2 :
  rf_well_formed E ->
  co_well_formed E co ->
  linearisations r E o ->
  rel_incl (rfe E) o ->
  rel_incl (co E) o ->
  rel_incl (fr E co) o ->
  (mobs_r_mobs E co r) e1 e2 ->
  o e1 e2.
Proof.
intros Hrfwf Hcowf Hlin Hrfincl Hcoincl Hfrincl [e [H1e [e' [Hee' He'2]]]].
generalize (lin_ext_prop E r o); intros [Himpl _];
generalize (Himpl Hlin); intros [Hincl Hlso];
generalize (Hincl e e' Hee'); intro Hgcbee';
destruct_lin Hlso; destruct_part Hpart;
generalize (mop_in_order Hrfwf Hcowf Hlin Hrfincl Hcoincl Hfrincl H1e);
generalize (mop_in_order Hrfwf Hcowf Hlin Hrfincl Hcoincl Hfrincl He'2);
intros [Heqe'2 | Hgcbe'2]; intros [Heqe'1 | Hgcbe'1].
  rewrite Heqe'1; rewrite <- Heqe'2; auto.
  rewrite <- Heqe'2; apply Htrans with e; auto.
  rewrite Heqe'1; apply Htrans with e'; auto.
  apply Htrans with e; auto; apply Htrans with e'; auto.
Qed.

Lemma tc_mobs_r_mobs_in_order E co r o x y :
  rf_well_formed E ->
  co_well_formed E co ->
  linearisations r E o ->
  rel_incl (rfe E) o ->
  rel_incl (co E) o ->
  rel_incl (fr E co) o ->
  tc_mobs_r_mobs E co r x y ->
  o x y.
Proof.
intros Hrfwf Hcowf Hlin Hrfincl Hcoincl Hfrincl Hxy.
induction Hxy.
  apply mobs_r_mobs_in_order with E co r; auto.

  generalize (lin_ext_prop E r o); intros [Himpl _];
  generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; destruct_lin Hlso; destruct_part Hpart;
  apply Htrans with e; auto.
    apply mobs_r_mobs_in_order with E co r; auto.
Qed.

Lemma tc_mobs_r_mobs_irr E co r o x :
  rf_well_formed E ->
  co_well_formed E co ->
  linearisations r E o ->
  rel_incl (rfe E) o ->
  rel_incl (co E) o ->
  rel_incl (fr E co) o ->
  ~(tc_mobs_r_mobs E co r x x).
Proof.
intros Hrfwf Hcowf Hlin Hrfeincl Hcoincl Hfrincl Hxx; generalize (lin_ext_prop E r o); intros [Himpl _];
generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; destruct_lin Hlso; destruct_part Hpart;
apply Hirr with x; apply tc_mobs_r_mobs_in_order with E co r; auto.
Qed.

(** * External Global Completion *)

(** ** External Global Completion: Definitions *)
Definition intervening_write (r : Rln Event) (e1 e2 : Event) : Prop :=
  exists w, is_write w /\ r e1 w /\ r w e2.

Definition gcb_rf (E : set Event) (gcb : Rln Event) (e1 e2 : Event) : Prop :=
  is_write e1 /\ is_read e2 /\ loc e1 = loc e2 /\ val e1 = val e2 /\
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

Definition preorder_gcb (E : set Event) (lob : set Event -> Rln Event) (e1 e2 : Event) : Prop :=
  lob E e1 e2 /\
  (is_write e1 \/
   (is_read e1 /\ read_requirements E lob e1 e2)).

Definition external_global_completion (E : set Event) (co lob : set Event -> Rln Event) (gcb : Rln Event): Prop :=
  (linearisations (preorder_gcb E lob) E) gcb /\
  rel_equal (rf E) (gcb_rf E gcb) /\ rel_equal (co E) (gcb_co E gcb).

(** ** External Global Completion: Auxiliary definitions, for convenience in the proofs below *)

Definition lob' (E : set Event) (lob : set Event -> Rln Event) (e1 e2 : Event) :=
  lob E e1 e2 /\ (is_read e1 /\ ~(read_requirements E lob e1 e2)).

Definition big_rel E co lob := tc_mobs_r_mobs E co (preorder_gcb E lob).

(** ** External Global Completion: Lemmas that do _not_ need the existence of a External  Global Completion order *)

Lemma lob'_irr E lob x :
  lob_well_formed E lob ->
  ~(lob' E lob x x).
Proof.
intros Hlobwf [Hxx ?]; destruct_lob_wf Hlobwf.
apply Hirr_lob; exists x; auto.
Qed.

Lemma lob_implies_pgcb_or_lob' (E : set Event) (lob : set Event -> Rln Event) (e1 e2 : Event) :
  lob_well_formed E lob ->
  lob E e1 e2 ->
  preorder_gcb E lob e1 e2 \/ lob' E lob e1 e2.
Proof.
intros Hlobwf H12; destruct_lob_wf Hlobwf; generalize (Hdom_lob  e1 e2 H12); intros [Hw1 | Hr1].
  left; split; auto.
  generalize (excluded_middle (read_requirements E lob e1 e2)); intros [Hrr | Hnrr];
  [left | right]; split; auto.
Qed.

Lemma pgcb_in_big_rel E co lob e1 e2 :
  preorder_gcb E lob e1 e2 ->
  big_rel E co lob e1 e2.
Proof.
intros Hpc; apply _base; exists e1; split; [left; auto | exists e2; split; [|left]; auto].
Qed.

Lemma lob'_in_lob'_seq_big_rel E co lob e1 e2 :
  lob' E lob e1 e2 ->
  rel_seq (lob' E lob) (maybe (big_rel E co lob)) e1 e2.
Proof.
intros H12; exists e2; split; auto; left; auto.
Qed.

Lemma mop_br_in_br E co lob e1 e2 e3 :
  maybe (obsplus E co) e1 e2 ->
  big_rel E co lob e2 e3 ->
  big_rel E co lob e1 e3.
Proof.
intros H12 H23. induction H23 as [e2 e3 [e [Hmop Hr]] | e2 e3 e' [e [H2e Hee']]];
[apply _base; exists e; split; auto; apply mop_trans with e2; auto |
apply _trans with e'; auto; exists e; split; auto; apply mop_trans with e2; auto].
Qed.

Lemma rfe_lob'_contrad E lob e1 e2 e3 :
  rf_well_formed E ->
  rfe E e1 e2 -> lob' E lob e2 e3 ->
  False.
Proof.
intros Hrfwf [Hrf12 Hext] [Hlob [Hr2 Hnrr]]; apply Hnrr; left; intros [w [Hrfw2 Hint]].
destruct Hrfwf as [? Hr]; generalize (Hr e2 Hr2); intros [? Huni]; generalize (Huni e1 w Hrf12 Hrfw2);
intro Heq; rewrite Heq in Hext; apply int_ext_contrad with E w e2; auto.
Qed.

Lemma co_lob'_contrad E (co : set Event -> Rln Event) lob e1 e2 e3 :
  co_well_formed E co ->
  co E e1 e2 -> lob' E lob e2 e3 ->
  False.
Proof.
intros Hcowf Hco [? [Hr2 ?]].
generalize (ran_co_is_write e1 e2 Hcowf Hco); intro Hw2.
apply read_write_contrad with e2; auto.
Qed.

Lemma fr_lob'_contrad E co lob e1 e2 e3 :
  co_well_formed E co ->
  fr E co e1 e2 -> lob' E lob e2 e3 ->
  False.
Proof.
intros Hcowf Hfr [? [Hr2 ?]];
generalize (ran_fr_is_write Hcowf Hfr); intro Hw2.
apply read_write_contrad with e2; auto.
Qed.

Lemma mop_lob'_in_lob' E co lob e1 e2 e3 :
  rf_well_formed E ->
  co_well_formed E co ->
  maybe (obsplus E co) e1 e2 ->
  lob' E lob e2 e3 ->
  lob' E lob e1 e3.
Proof.
intros Hrfwf Hcowf [Heq12 | H12] H23; [rewrite Heq12|]; auto.
generalize (obsplus_dec Hrfwf Hcowf H12); clear H12;
intros [Hrfe12 | [Hco12 | [Hfr12 | [Hcorfe12 | Hfrrfe12]]]].
  generalize (rfe_lob'_contrad Hrfwf Hrfe12 H23); auto; intro Ht; inversion Ht.
  generalize (co_lob'_contrad e1 Hcowf Hco12 H23); auto; intro Ht; inversion Ht.
  generalize (fr_lob'_contrad Hcowf Hfr12 H23); auto; intro Ht; inversion Ht.
  destruct Hcorfe12 as [x [? Hrfe]]; generalize (rfe_lob'_contrad Hrfwf Hrfe H23); auto; intro Ht; inversion Ht.
  destruct Hfrrfe12 as [x [? Hrfe]]; generalize (rfe_lob'_contrad Hrfwf Hrfe H23); auto; intro Ht; inversion Ht.
Qed.

Lemma br_trans E co lob e1 e2 e3 :
  big_rel E co lob e1 e2 ->
  big_rel E co lob e2 e3 ->
  big_rel E co lob e1 e3.
Proof.
intros H12 H23.
apply tc_trans with e2; auto.
Qed.

Lemma br_mop_in_br E co lob e1 e2 e3 :
  big_rel E co lob e1 e2 ->
  maybe (obsplus E co) e2 e3 ->
  big_rel E co lob e1 e3.
Proof.
intros Hbr12 Hmop23; induction Hbr12 as [e1 e2 Hb | e1 e2 e H1e].
destruct Hb as [e [He1e [e'[Hee' He'2]]]];
apply _base; exists e; split; auto; exists e'; split; auto; apply mop_trans with e2; auto.

apply br_trans with e; auto; apply _base; auto.
Qed.

Lemma pgcb_lob'_in_pgcb_or_lob' E lob e1 e2 e3 :
  lob_well_formed E lob ->
  (preorder_gcb E lob) e1 e2 ->
  lob' E lob e2 e3 ->
  preorder_gcb E lob e1 e3 \/ lob' E lob e1 e3.
Proof.
intros Hlobwf Hpc12 H23; destruct_lob_wf Hlobwf.
  left; destruct Hpc12 as [H12 Hor12]; destruct H23 as [H23 [Hr2 Hnrr]].
  assert (lob E e1 e3) as Hlob13.
    apply (Htrans_lob e1 e2 e3 H12 H23).
  destruct Hor12 as [Hw1|[Hr1 Hrr]]; split; auto.
  right; split; auto.
  inversion Hrr as [Hext | Hint]; [left | right]; auto.
    destruct Hint as [w1 [Hrfi Hlob]]; exists w1; split; auto.
    apply (Htrans_lob w1 e2 e3 Hlob H23).
Qed.

Lemma br_lob'_in_br_or_lob' E co lob e1 e2 e3 :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  big_rel E co lob e1 e2 ->
  lob' E lob e2 e3 ->
  big_rel E co lob e1 e3 \/ lob' E lob e1 e3.
Proof.
intros Hrfwf Hcowf Hlobwf H12 H23; induction H12 as [e1 e2 [e [H1e [e' [Hee' He'2]]]] | e1 e2 e H1e He2].

  generalize (mop_lob'_in_lob' Hrfwf Hcowf He'2 H23); clear He'2 H23; intro He'3.
  generalize (pgcb_lob'_in_pgcb_or_lob' Hlobwf Hee' He'3); intros [Hobs_extra3 | Hlob'e3].
    left; apply _base; exists e; split; auto; exists e3; split; [|left]; auto.
    right; apply mop_lob'_in_lob' with co e; auto.

  generalize (IHHe2 H23); intros [Hbre3 | Hlob'e3].
    left; apply br_trans with e; auto; apply _base; auto.
    clear IHHe2 H23 He2 e2.
    destruct H1e as [x [H1x [y [Hxy Hye]]]].
    generalize (mop_lob'_in_lob' Hrfwf Hcowf Hye Hlob'e3); clear Hye Hlob'e3 e; intro Hy3.
    generalize (pgcb_lob'_in_pgcb_or_lob' Hlobwf Hxy Hy3); clear Hxy Hy3 y; intro Hx3.
    inversion Hx3 as [Hpcx3 | Hlob'x3]; clear Hx3.
      left; apply _base; exists x; split; auto; exists e3; split; auto; left; auto.
      right; apply mop_lob'_in_lob' with co x; auto.
Qed.

Lemma lob'_seq_lob'_in_pgcb_or_lob' E lob e1 e2 e3:
  lob_well_formed E lob ->
  lob' E lob e1 e2 ->
  lob' E lob e2 e3 ->
  (preorder_gcb E lob) e1 e3 \/ lob' E lob e1 e3.
Proof.
intros Hlobwf [Hlob12 [Hw1 Hrr1]] [Hlob23 [Hw2 Hrr2]]; destruct_lob_wf Hlobwf.
assert (lob E e1 e3) as H13.
  apply (Htrans_lob e1 e2 e3 Hlob12 Hlob23).
generalize (excluded_middle (read_requirements E lob e1 e3)); intros [Hrr | Hnrr].
  left; split; auto.
  right; split; auto.
Qed.

Lemma pgcb_mbr_in_br E co lob e1 e2 e3 :
  (preorder_gcb E lob) e1 e2 ->
  maybe (big_rel E co lob) e2 e3 ->
  big_rel E co lob e1 e3.
Proof.
intros H12 [Heq23 | H23].
  rewrite <- Heq23; apply _base; exists e1; split; [left | exists e2; split; [ | left]]; auto.
  induction H23.
    apply br_trans with e0; apply _base; auto.
      exists e1; split; [left | exists e0; split; [|left]]; auto.

    apply br_trans with e; auto; apply br_trans with e0; apply _base; auto.
      exists e1; split; [left | exists e0; split; [|left]]; auto.
Qed.

  Lemma gcb_path_ob_dec (E : set Event) (co lob : set Event -> Rln Event) (gcb : Rln Event) (e1 e2 : Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  ob E co (lob E) e1 e2 ->
  obsplus E co e1 e2 \/ big_rel E co lob e1 e2 \/
  rel_seq (lob' E lob) (maybe (big_rel E co lob)) e1 e2
  \/ rel_seq (lob' E lob) (maybe (obsplus E co)) e1 e2.
Proof.
intros Hrfwf Hcowf Hlobwf H12.
induction H12 as [e1 e2 Hobs | e1 e2 Hlob |]; auto.
  (*e1,e2 in obs*)
  left; apply _base; auto.

  (*e1,e2 in lob*)
  generalize (lob_implies_pgcb_or_lob' e1 e2 Hlobwf Hlob); intros [Hpc | Hlob']; right;
    [left; apply pgcb_in_big_rel | right; left; apply lob'_in_lob'_seq_big_rel]; auto.

  (*inductive case*)
  clear H12_ H12_0; inversion IHob1 as [Hmop1e | [Hbr1e | [Hlob'1e | Hlob'1e]]]; clear IHob1.

    (*e1,e in obs+*)
    inversion IHob2 as [Hmop2 | [Hbre2 | Hlob'e2]]; clear IHob2.

      (*e,e2 in obs+*)
      left; apply op_trans with e; auto.

      (*e,e2 in br*)
      right; left; apply mop_br_in_br with e; auto; right; auto.

      (*e,e2 in lob';mbr U lob';mop*)
      inversion Hlob'e2 as [Hee2 | Hee2]; clear Hlob'e2; destruct Hee2 as [e' [Hlob'ee' Hbre'2]];
      right; right; [left | right]; exists e'; split; auto;
      apply mop_lob'_in_lob' with co e; auto; right; auto.

    (*e1,e in br*)
    inversion IHob2 as [Hmop2 | [Hbre2 | Hlob'e2]]; clear IHob2.

      (*e,e2 in op*)
      right; left; apply br_mop_in_br with e; auto; right; auto.

      (*e,e2 in br*)
      right; left; apply br_trans with e; auto.

      (*e,e2 in lob';mbr U lob';mop*)
      inversion Hlob'e2 as [Hee2 | Hee2]; clear Hlob'e2; destruct Hee2 as [e' [Hee' He'2]];
      generalize (br_lob'_in_br_or_lob' Hrfwf Hcowf Hlobwf Hbr1e Hee');
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
         intros [Hpc1'' | Hlob'1'']; right; [left; apply pgcb_mbr_in_br with e''; auto | right; left; exists e''; auto].

        generalize (br_lob'_in_br_or_lob' Hrfwf Hcowf Hlobwf Hbre'e Hee''); clear Hbre'e Hee'' e; intros [Hbre'e'' | Hlobe'e''].
          right; right; left; exists e'; split; auto.
        inversion He''2 as [Heq''2 | Hbre''2]; clear He''2.
          rewrite Heq''2 in Hbre'e''; right; auto.
        right; apply br_trans with e''; auto.

          generalize (lob'_seq_lob'_in_pgcb_or_lob' Hlobwf H1e' Hlobe'e''); clear e' H1e' Hlobe'e''; intros [Hmpc1'' | Hlob'1''].
            right; left; apply pgcb_mbr_in_br with e''; auto.
            right; right; left; exists e''; split; auto.

      (*e,e2 in lob;mop*)

         rewrite Heqe'e in H1e'; generalize (lob'_seq_lob'_in_pgcb_or_lob' Hlobwf H1e' Hee''); clear e Heqe'e H1e' Hee'';
         intros [Hpc1'' | Hlob'1'']; right; [left; apply _base; exists e1; split; [left | exists e''; split]; auto |
                                             right; right; exists e''; split; auto].

         generalize (br_lob'_in_br_or_lob' Hrfwf Hcowf Hlobwf Hbre'e Hee''); clear Hbre'e Hee'' e; intros [Hbre'e'' | Hlob'e'e''].
           right; right; left; exists e'; split; auto; right; apply br_mop_in_br with e''; auto.

         generalize (lob'_seq_lob'_in_pgcb_or_lob' Hlobwf H1e' Hlob'e'e''); intros [Hpc | Hlob]; clear H1e' Hlob'e'e''.
           right; left; apply _base; exists e1; split; [left|exists e'']; auto.
           right; right; right; exists e''; split; auto.

    (*e,1e in lob';mop*)
    inversion IHob2 as [Hmop2 | [Hbre2 | Hlob'e2]]; clear IHob2; destruct Hlob'1e as [e' [H1' H'e]].

      (*e,e2 in obs+*)
      right; right; right; exists e'; split; auto.
        inversion H'e as [Heq'e | Hop'e]; clear H'e; [rewrite Heq'e; right|right; apply op_trans with e]; auto.

      (*e,e2 in br*)
      right; right; left; exists e'; split; auto.
        right; apply mop_br_in_br with e; auto.

      (*e,e2 in lob';mbr U lob';mop*)
      inversion Hlob'e2 as [He2 | He2]; destruct He2 as [e'' [He'' H''2]];
      generalize (mop_lob'_in_lob' Hrfwf Hcowf H'e He''); clear H'e He''; intros Hlob;
      generalize (lob'_seq_lob'_in_pgcb_or_lob' Hlobwf H1' Hlob); clear H1' Hlob; intros [Hpc | Hlob]; right.

          left; apply pgcb_mbr_in_br with e''; auto.
          right; left; exists e''; auto.
          left; apply _base; exists e1; split; [left | exists e''; split]; auto.
          right; right; exists e''; split; auto.
Qed.

(** * External Global Completion -> External Visibility lemmas *)
Lemma fr_implies_gcb_or_gcb_minus_1 (E : set Event) (co lob : set Event -> Rln Event) (gcb : Rln Event) (e1 e2 : Event) :
  co_well_formed E co ->
  linearisations (preorder_gcb E lob) E gcb ->
  fr E co e1 e2 ->
  gcb e1 e2 \/ gcb e2 e1.
Proof.
intros Hcowf Hlin H12.
apply fr_implies_order_or_order_minus_1 with E co (preorder_gcb E lob); auto.
Qed.

Lemma rf_in_gcb E gcb x y :
  rel_equal (rf E) (gcb_rf E gcb) ->
  rf E x y ->
  gcb x y.
Proof.
intros Hrfeq Hrf; destruct Hrfeq as [Hrfincl ?]; generalize (Hrfincl x y Hrf); intros [? [? [? [? [? ?]]]]]; auto.
Qed.

Lemma rfe_in_gcb E gcb x y :
  rel_equal (rf E) (gcb_rf E gcb) ->
  rfe E x y ->
  gcb x y.
Proof.
intros Hrfeq [Hrf ?]; apply rf_in_gcb with E; auto.
Qed.

Lemma co_in_gcb E co gcb x y :
  rel_equal (co E) (gcb_co E gcb) ->
  co E x y ->
  gcb x y.
Proof.
  apply co_in_order; auto.
Qed.

Lemma fr_in_gcb E co lob gcb x y :
  co_well_formed E co ->
  linearisations (preorder_gcb E lob) E gcb ->
  rel_equal (rf E) (gcb_rf E gcb) ->
  rel_equal (co E) (gcb_co E gcb) ->
  fr E co x y ->
  gcb x y.
Proof.
intros Hcowf Hlin Hrfeq Hcoeq Hfr.
generalize (fr_implies_gcb_or_gcb_minus_1 Hcowf Hlin (*Hrfeq Hcoeq*) Hfr); intros [? | Hgcbyx]; auto;
generalize Hfr; intros [w [Hw [Hrf Hco]]];
generalize (rf_in_gcb Hrfeq Hrf); intro Hgcbwx;
generalize (co_in_gcb co w y Hcoeq Hco); intro Hgcbwy.
destruct Hrfeq as [Hincl ?]; generalize (Hincl w x Hrf); intros [? [? [? Hnointerv]]].
assert False as Ht.
  apply Hnointerv; exists y; split; [|split; split]; auto.
    apply ran_co_is_write with E co w; auto.
    apply co_implies_same_loc with E co; auto.
  generalize (fr_implies_same_loc Hcowf Hfr); auto.
inversion Ht.
Qed.

Lemma big_rel_irr E co lob gcb x :
  rf_well_formed E ->
  co_well_formed E co ->
  linearisations (preorder_gcb E lob) E gcb ->
  rel_equal (rf E) (gcb_rf E gcb) ->
  rel_equal (co E) (gcb_co E gcb) ->
  ~(big_rel E co lob x x).
Proof.
intros Hrfwf Hcowf Hlin Hrfeq Hcoeq; apply tc_mobs_r_mobs_irr with gcb; auto; intros e1 e2 H12.
  apply rfe_in_gcb with E; auto.
  apply co_in_gcb with E co; auto.
  apply fr_in_gcb with E co lob; auto.
Qed.

(** ** External global completion implies External Visibility *)

Lemma external_global_completion_implies_external_visibility (E : set Event) (co lob : set Event -> Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  (exists gcb, external_global_completion E co lob gcb) -> external_visibility E co lob.

(** We show here that:
    - given a well-formed execution (E, lob, rf, co)
    - if the [internal_visibility] axiom holds over E,
    - and there exists an [external_global_completion] External Global Completion order gcb,
    - then the [external_visibility] axiom holds over E *)

Proof.
intros Hrfwf Hcowf Hlobwf Hscpv [gcb [Hlin [Hrfeq Hcoeq]]] [x Hx].
(** Reason by contradiction: suppose that the [external_visibility] axiom does not hold over E, viz,
    there exists x s.t. (x,x) in ob.*)
generalize (gcb_path_ob_dec gcb Hrfwf Hcowf Hlobwf Hx); clear Hx; intros [Hmop | [Hbr | [Hlbr | Hlop]]].

(** Now observe (c.f. [gcb_path_ob_dec]) that (x,x) in ob means either:
 - (x,x) obsplus
 - (x,x) in big_rel
 - (x,x) in lob';big_rel?
 - (x,x) in lob';obs *)

(** *** Case 1: (x,x) obsplus*)
  apply Hscpv; exists x; apply obsplus_in_scpv; auto.

(** This is a contradiction of the [internal_visibility] axiom. *)

(** *** Case 2: (x,x) in big_rel *)
  apply (big_rel_irr Hrfwf Hcowf Hlin Hrfeq Hcoeq Hbr); auto.

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
      generalize (br_lob'_in_br_or_lob' Hrfwf Hcowf Hlobwf Hbryx Hxy); clear Hbryx Hxy; intros [Hbr | Hlob'].
      (** Using [br_lob'_in_br_or_lob'] we can reason by case disjunction:
          - 3bi. (x,x) in big_rel
          - 3bii. (x,x) in lob'*)

          (** Case 3bi: (x,x) in big_rel *)
          apply (big_rel_irr Hrfwf Hcowf Hlin Hrfeq Hcoeq Hbr); auto.

          (** This is impossible because big_rel is irreflexive (c.f. [big_rel_irr]). *)

          (** Case 3bii: (x,x) in lob' *)
          apply (lob'_irr Hlobwf Hlob').

          (** This is impossible because lob' is irreflexive (c.f. [lob'_irr]). *)

(** *** Case 4: (x,x) in lob';obs* *)
  destruct Hlop as [y [Hxy Hyx]].

(** By definition this means that there exists y s.t. (x,y) in lob' and (y,x) in obs*. *)
  generalize (mop_lob'_in_lob' Hrfwf Hcowf Hyx Hxy); intro Hlob'.

(** Therefore (y,y) in obs*;lob'.
    Now observe that (c.f. [mop_lob'_in_lob']) this entails (y,y) in lob'. *)
  apply (lob'_irr Hlobwf Hlob').

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
Definition pre_egc E co lob := transitive_closure (rel_union (rf E)
  (rel_union (co E) (rel_union (fr E co) (preorder_gcb E lob)))).
Definition big_rel2 E co lob := transitive_closure (rel_seq (maybe (obsplus E co))
  (rel_seq (preorder_gcb E lob) (maybe (obsplus E co)))).

Lemma rfi_pgcb_is_pgcb E lob e1 e2 e3 :
  rf_well_formed E ->
  rfi E e1 e2 ->
  preorder_gcb E lob e2 e3 ->
  preorder_gcb E lob e1 e3.
Proof.
intros Hrfwf [Hrf12 Hint12];
generalize (ran_rf_is_read Hrf12); intros Hr2 [Hlob23 [Hw2 | [_ Hrr23]]];
  [generalize (read_write_contrad e2 Hr2 Hw2); intro Ht; inversion Ht | inversion Hrr23 as [Hext | Hint]; clear Hrr23].
  assert False as Ht.
    apply Hext; exists e1; split; auto.
  inversion Ht.
  destruct Hint as [w' [[Hrf' ?] Hlob]]; destruct_rf_wf Hrfwf;
  generalize (Hex_uni e2 Hr2); intros [? Huni]; generalize (Huni e1 w' Hrf12 Hrf'); intro Heq.
  rewrite Heq; split; auto; left; apply dom_rf_is_write with E e2; auto.
Qed.

Lemma rfi_seq_obs_in_obs E co lob x y z :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  rfi E x y ->
  obs E co y z ->
  obs E co x z.
Proof.
intros Hrfwf Hcowf Hlobwf Hrfi Hobs.
  destruct Hrfi as [Hrfxy ?]; inversion Hobs as [Hrfe | [Hco | Hfr]]; clear Hobs.

    destruct Hrfe as [Hrfyz ?];
    generalize (dom_rf_is_write Hrfyz); intro Hwy;
    generalize (ran_rf_is_read Hrfxy); intro Hry;
    generalize (read_write_contrad y Hry Hwy); intro Ht; inversion Ht.

    generalize (dom_co_is_write y z Hcowf Hco); intro Hwy;
    generalize (ran_rf_is_read Hrfxy); intro Hry;
    generalize (read_write_contrad y Hry Hwy); intro Ht; inversion Ht.

    generalize (rf_fr_is_co Hrfwf Hrfxy Hfr); intro Hco; right; left; auto.
Qed.

Lemma rfi_seq_obsplus_in_obsplus E co lob x y z :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  rfi E x y ->
  obsplus E co y z ->
  obsplus E co x z.
Proof.
intros Hrfwf Hcowf Hlobwf Hrfi Hobsp.
induction Hobsp.
  apply _base; apply rfi_seq_obs_in_obs with lob e1; auto.
  apply _trans with e; auto.
    apply rfi_seq_obs_in_obs with lob e1; auto.
Qed.


Lemma rfe_seq_obsplus_in_obsplus E co lob x y z :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  rfe E x y ->
  obsplus E co y z ->
  obsplus E co x z.
Proof.
intros Hrfwf Hcowf Hlobwf Hrfi Hobsp.
  apply _trans with y; auto; left; auto.
Qed.

Lemma rf_seq_obsplus_in_obsplus E co lob x y z :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  rf E x y ->
  obsplus E co y z ->
  obsplus E co x z.
Proof.
intros Hrfwf Hcowf Hlobwf Hrfi Hobsp.
  assert (E x) as HEx.
    apply dom_rf_in_evts with y; auto.
  assert (E y) as HEy.
    apply ran_rf_in_evts with x; auto.
  generalize (int_or_ext E x y HEx HEy); intros [Hint | Hext].
    apply rfi_seq_obsplus_in_obsplus with lob y; auto; split; auto.
    apply rfe_seq_obsplus_in_obsplus with lob y; auto; split; auto.
Qed.

Lemma rfi_base_br2_in_br2 E co lob e1 e2 e3 :
  co_well_formed E co ->
  rf_well_formed E ->
  lob_well_formed E lob ->
  rfi E e1 e2 ->
  rel_seq (maybe (obsplus E co))
      (rel_seq (preorder_gcb E lob) (maybe (obsplus E co))) e2 e3 ->
  rel_seq (maybe (obsplus E co))
      (rel_seq (preorder_gcb E lob) (maybe (obsplus E co))) e1 e3.
Proof.
intros Hcowf Hrfwf Hlobwf Hrfi12 H23.
destruct H23 as [x [H2x [y [Hxy Hy3]]]].
inversion H2x as [Heq2x | Hobsp2x]; clear H2x.
rewrite Heq2x in *; clear Heq2x.
  exists e1; split; [left|]; auto; exists y; split; auto.
    apply rfi_pgcb_is_pgcb with x; auto; split; auto.
  exists x; split; [right|exists y; split]; auto.
    apply rfi_seq_obsplus_in_obsplus with lob e2; auto; split; auto.
Qed.

Lemma rfe_base_br2_in_br2 E co lob e1 e2 e3 :
  co_well_formed E co ->
  rf_well_formed E ->
  lob_well_formed E lob ->
  rfe E e1 e2 ->
  rel_seq (maybe (obsplus E co))
      (rel_seq (preorder_gcb E lob) (maybe (obsplus E co))) e2 e3 ->
  rel_seq (maybe (obsplus E co))
      (rel_seq (preorder_gcb E lob) (maybe (obsplus E co))) e1 e3.
Proof.
intros Hcowf Hrfwf Hlobwf Hrfe12 H23.
destruct H23 as [x [H2x [y [Hxy Hy3]]]].
inversion H2x as [Heq2x | Hobsp2x]; clear H2x.
rewrite Heq2x in *; clear Heq2x.
  exists x; split; [right; apply _base; left|exists y; split]; auto.
  exists x; split; [right; apply _trans with e2|exists y; split]; auto; left; auto.
Qed.

Lemma rf_br2_in_br2 E co lob e1 e2 e3 :
  co_well_formed E co ->
  rf_well_formed E ->
  lob_well_formed E lob ->
  rf E e1 e2 ->
  big_rel2 E co lob e2 e3 ->
  big_rel2 E co lob e1 e3.
Proof.
intros Hcowf Hrfwf Hlobwf Hrf12 Hbr23.

assert (E e1) as HE1.
  apply dom_rf_in_evts with e2; auto.
assert (E e2) as HE2.
  apply ran_rf_in_evts with e1; auto.
generalize (int_or_ext E e1 e2 HE1 HE2); intros [Hint12 | Hext12].

induction Hbr23 as [e2 e3 H23|].
  apply _base; apply rfi_base_br2_in_br2 with e2; auto; split; auto.

  apply _trans with e; auto.
    apply rfi_base_br2_in_br2 with e0; auto; split; auto.

induction Hbr23 as [e2 e3 H23|].
  apply _base; apply rfe_base_br2_in_br2 with e2; auto; split; auto.

  apply _trans with e; auto.
    apply rfe_base_br2_in_br2 with e0; auto; split; auto.
Qed.

Lemma co_seq_obsplus_in_obsplus E co lob x y z :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  co E x y ->
  obsplus E co y z ->
  obsplus E co x z.
Proof.
intros Hrfwf Hcowf Hlobwf Hrfi Hobsp.
  apply _trans with y; auto; right; left; auto.
Qed.

Lemma co_br2_in_br2 E co lob e1 e2 e3 :
  co_well_formed E co ->
  co E e1 e2 ->
  big_rel2 E co lob e2 e3 ->
  big_rel2 E co lob e1 e3.
Proof.
intros Hcowf H12 H23; induction H23 as [e2 e3 H23|].
  destruct H23 as [x [H2x Hx3]]; apply _base; exists x; split; auto;
  right; inversion H2x as [Heq2x | Ho2x]; clear H2x.
    rewrite Heq2x in *; clear Heq2x; apply _base; right; left; auto.
    apply _trans with e2; auto; right; left; auto.

  apply _trans with e; auto.
    destruct H as [x [H0x Hxe]]; exists x; split; auto.
    right; inversion H0x as [Heq0x | Ho0x]; clear H0x.
      rewrite Heq0x in *; clear Heq0x; apply _base; right; left; auto.
      apply _trans with e0; auto; right; left; auto.
Qed.


Lemma fr_seq_obsplus_in_obsplus E co lob x y z :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  fr E co x y ->
  obsplus E co y z ->
  obsplus E co x z.
Proof.
intros Hrfwf Hcowf Hlobwf Hrfi Hobsp.
  apply _trans with y; auto; right; right; auto.
Qed.

Lemma fr_br2_in_br2 E co lob e1 e e2 :
  co_well_formed E co ->
  fr E co e1 e ->
  big_rel2 E co lob e e2 ->
  big_rel2 E co lob e1 e2.
Proof.
intros Hcowf H12 H23; induction H23 as [e2 e3 H23|].
  destruct H23 as [x [H2x Hx3]]; apply _base; exists x; split; auto;
  right; inversion H2x as [Heq2x | Ho2x]; clear H2x.
    rewrite Heq2x in *; clear Heq2x; apply _base; right; right; auto.
    apply _trans with e2; auto; right; right; auto.

  apply _trans with e; auto.
    destruct H as [x [H0x Hxe]]; exists x; split; auto.
    right; inversion H0x as [Heq0x | Ho0x]; clear H0x.
      rewrite Heq0x in *; clear Heq0x; apply _base; right; right; auto.
      apply _trans with e0; auto; right; right; auto.
Qed.

Definition nrel E co lob :=
  maybe (transitive_closure
    (rel_union (obsplus E co) (preorder_gcb E lob))).

Lemma rf_seq_obsplus_u_pgcb_in_tc_obsplus_u_pgcb E co lob e1 e2 e3 :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  rf E e1 e2 ->
  rel_union (obsplus E co) (preorder_gcb E lob) e2 e3 ->
  transitive_closure (rel_union (obsplus E co) (preorder_gcb E lob)) e1 e3.
Proof.
intros Hrfwf Hcowf Hlobwf H12 H23.
        inversion H23 as [Hop23 | Hpgcb23]; clear H23.

            apply _base; left; apply rf_seq_obsplus_in_obsplus with lob e2; auto.

            assert (E e1) as HE1.
              apply dom_rf_in_evts with e2; auto.
            assert (E e2) as HE2.
              apply ran_rf_in_evts with e1; auto.
            generalize (int_or_ext E e1 e2 HE1 HE2); intros [Hint | Hext].
              apply _base; right; apply rfi_pgcb_is_pgcb with e2; auto; split; auto.
              apply _trans with e2; [left; apply _base; left; split|apply _base; right]; auto.
Qed.

Lemma rf_seq_nrel_seq_rfi_in_nrel_seq_rfi E co lob e1 e2 e3 :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  rf E e1 e2 ->
  rel_seq (nrel E co lob) (rfi E) e2 e3 ->
  rel_seq (nrel E co lob) (rfi E) e1 e3.
Proof.
intros Hrfwf Hcowf Hlobwf H12 [x [H2x Hx3]].
inversion H2x as [Heq2x | Hn2x]; clear H2x.
  rewrite Heq2x in H12; clear Heq2x.
    assert (is_write x) as Hwx.
      apply dom_rf_is_write with E e3; destruct Hx3; auto.
    assert (is_read x) as Hrx.
      apply ran_rf_is_read with E e1; auto.
    generalize (read_write_contrad x Hrx Hwx); intro Ht; inversion Ht.
    exists x; split; auto; clear Hx3; right.
      induction Hn2x as [e2 x H2x|].

        apply rf_seq_obsplus_u_pgcb_in_tc_obsplus_u_pgcb with e2; auto.

        apply tc_trans with e; auto; apply rf_seq_obsplus_u_pgcb_in_tc_obsplus_u_pgcb with e0; auto.
Qed.

Lemma co_seq_obsplus_u_pgcb_in_tc_obsplus_u_pgcb E co lob e1 e2 e3 :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  co E e1 e2 ->
  rel_union (obsplus E co) (preorder_gcb E lob) e2 e3 ->
  transitive_closure (rel_union (obsplus E co) (preorder_gcb E lob)) e1 e3.
Proof.
intros Hrfwf Hcowf Hlobwf H12 H23.
        inversion H23 as [Hop23 | Hpgcb23]; clear H23.
          apply _base; left; apply _trans with e2; auto; right; left; auto.
          apply _trans with e2; [|apply _base; right]; auto; left; apply _base; right; left; auto.
Qed.

Lemma co_seq_nrel_seq_rfi_in_nrel_seq_rfi E co lob e1 e2 e3 :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  co E e1 e2 ->
  rel_seq (nrel E co lob) (rfi E) e2 e3 ->
  rel_seq (nrel E co lob) (rfi E) e1 e3.
Proof.
intros Hrfwf Hcowf Hlobwf H12 [x [H2x Hx3]].
inversion H2x as [Heq2x | Hn2x]; clear H2x.
  rewrite Heq2x in H12; clear Heq2x.
    exists x; split; auto; right; apply _base; left; apply _base; right; left; auto.
    exists x; split; auto; right.
      induction Hn2x.
        apply co_seq_obsplus_u_pgcb_in_tc_obsplus_u_pgcb with e0; auto.
        apply tc_trans with e; auto; apply co_seq_obsplus_u_pgcb_in_tc_obsplus_u_pgcb with e0; auto.
Qed.

Lemma fr_seq_obsplus_u_pgcb_in_tc_obsplus_u_pgcb E co lob e1 e2 e3 :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  fr E co e1 e2 ->
  rel_union (obsplus E co) (preorder_gcb E lob) e2 e3 ->
  transitive_closure (rel_union (obsplus E co) (preorder_gcb E lob)) e1 e3.
Proof.
intros Hrfwf Hcowf Hlobwf H12 H23.
        inversion H23 as [Hop23 | Hpgcb23]; clear H23.
          apply _base; left; apply _trans with e2; auto; right; right; auto.
          apply _trans with e2; [|apply _base; right]; auto; left; apply _base; right; right; auto.
Qed.

Lemma fr_seq_nrel_seq_rfi_in_nrel_seq_rfi E co lob e1 e2 e3 :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  fr E co e1 e2 ->
  rel_seq (nrel E co lob) (rfi E) e2 e3 ->
  rel_seq (nrel E co lob) (rfi E) e1 e3.
Proof.
intros Hrfwf Hcowf Hlobwf H12 [x [H2x Hx3]].
inversion H2x as [Heq2x | Hn2x]; clear H2x.
  rewrite Heq2x in H12; clear Heq2x.
    exists x; split; auto; right; apply _base; left; apply _base; right; right; auto.
    exists x; split; auto; right.
      induction Hn2x.
        apply fr_seq_obsplus_u_pgcb_in_tc_obsplus_u_pgcb with e0; auto.
        apply tc_trans with e; auto; apply fr_seq_obsplus_u_pgcb_in_tc_obsplus_u_pgcb with e0; auto.
Qed.

Lemma pgcb_seq_nrel_seq_rfi_in_nrel_seq_rfi E co lob e1 e2 e3 :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  preorder_gcb E lob e1 e2 ->
  rel_seq (nrel E co lob) (rfi E) e2 e3 ->
  rel_seq (nrel E co lob) (rfi E) e1 e3.
Proof.
intros Hrfwf Hcowf Hlobwf H12 [x [H2x Hx3]].
  exists x; split; auto.
  inversion H2x as [Heq2x | Hu2x]; clear H2x.
    rewrite Heq2x in H12; clear Heq2x.
      right; apply _base; right; auto.
      right; apply _trans with e2; auto; right; auto.
Qed.

Lemma pre_egc_dec (E : set Event) (co lob : set Event -> Rln Event) (e1 e2 : Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  pre_egc E co lob e1 e2 ->
  rel_seq (nrel E co lob) (rfi E) e1 e2 \/ (obsplus E co) e1 e2 \/ (big_rel2 E co lob) e1 e2.
Proof.
intros Hrfwf Hcowf Hlobwf H12; induction H12 as [e1 e2 H12 | e1 e2 e H1e He2].
  inversion H12 as [Hrf12 | [Hco12 | [Hfr12 | Hpc12]]].

    assert (E e1) as HE1.
      apply dom_rf_in_evts with e2; auto.
    assert (E e2) as HE2.
      apply ran_rf_in_evts with e1; auto.
    generalize (int_or_ext E e1 e2 HE1 HE2); intros [Hint | Hext].
      left; exists e1; split; [left|split]; auto.
      right; left; apply _base; left; split; auto.

    right; left; apply _base; right; left; auto.
    right; left; apply _base; right; right; auto.
    right; right; left; exists e1; split; [left|exists e2; split]; auto; left; auto.

  inversion H1e as [Hrf1e | [Hco1e | [Hfr1e | Hpc1e]]]; clear H1e;
    inversion IHHe2 as [Hnrfie2 | [Hobspluse2 | Hbre2]]; clear IHHe2.
    left; apply rf_seq_nrel_seq_rfi_in_nrel_seq_rfi with e; auto.
    right; left; apply rf_seq_obsplus_in_obsplus with lob e; auto.
    right; right; apply rf_br2_in_br2 with e; auto.
    left; apply co_seq_nrel_seq_rfi_in_nrel_seq_rfi with e; auto.
    right; left; apply co_seq_obsplus_in_obsplus with lob e; auto.
    right; right; apply co_br2_in_br2 with e; auto.
    left; apply fr_seq_nrel_seq_rfi_in_nrel_seq_rfi with e; auto.
    right; left; apply fr_seq_obsplus_in_obsplus with lob e; auto.
    right; right; apply fr_br2_in_br2 with e; auto.
    left; apply pgcb_seq_nrel_seq_rfi_in_nrel_seq_rfi with e; auto.
    right; right; apply _base; exists e1; split; [left|exists e;split; [|right]]; auto.
    right; right; apply _trans with e; auto; exists e1; split; auto; [left |]; auto; exists e; split; [|left]; auto.
Qed.

Definition obs_extra E co := rel_union (rfe E) (rel_union (co E)
  (rel_union (fr E co) (rel_union (corfe E co) (frrfe E co)))).

Lemma mcomplus_seq_pgcb E co lob e1 e2 e3 :
  rf_well_formed E ->
  maybe (complus E co) e1 e2 ->
  preorder_gcb E lob e2 e3 ->
  rel_seq (maybe (obs_extra E co)) (preorder_gcb E lob) e1 e3.
Proof.
intros Hrfwf H12 H23; inversion H12 as [Heq12 | Hcomplus12]; clear H12.
  rewrite Heq12; exists e2; split; [left|]; auto.
  inversion Hcomplus12 as [Hrf | [Hco | [Hfr | [Hcorf | Hfrrf]]]]; clear Hcomplus12.
    generalize (dom_rf_in_evts Hrf); intro HE1;
    generalize (ran_rf_in_evts Hrf); intro HE2;
    generalize (int_or_ext E e1 e2 HE1 HE2); intros [Hint | Hext].
      exists e1; split; [left|]; auto; apply rfi_pgcb_is_pgcb with e2; auto; split; auto.

      exists e2; split; auto; right; left; split; auto.

      exists e2; split; auto; right; right; left; auto.

      exists e2; split; auto; right; right; right; left; auto.

      destruct Hcorf as [w [Hco Hrf]].
      generalize (dom_rf_in_evts Hrf); intro HEw;
      generalize (ran_rf_in_evts Hrf); intro HE2;
      generalize (int_or_ext E w e2 HEw HE2); intros [Hint | Hext].
        exists w; split; [right; right; left|]; auto; apply rfi_pgcb_is_pgcb with e2; auto; split; auto.
        exists e2; split; auto; right; right; right; right; left; auto; exists w; split; auto; split; auto.

      destruct Hfrrf as [w [Hfr Hrf]].
      generalize (dom_rf_in_evts Hrf); intro HEw;
      generalize (ran_rf_in_evts Hrf); intro HE2;
      generalize (int_or_ext E w e2 HEw HE2); intros [Hint | Hext].
        exists w; split; [right; right; right; left|]; auto; apply rfi_pgcb_is_pgcb with e2; auto; split; auto.
        exists e2; split; auto; right; right; right; right; right; auto; exists w; split; auto; split; auto.
Qed.

Lemma mcomplus_seq_pgcb_incl E co lob :
  rf_well_formed E ->
  rel_incl (rel_seq (maybe (complus E co))
    (preorder_gcb E lob)) (rel_seq (maybe (obs_extra E co)) (preorder_gcb E lob)).
Proof.
intros Hrfwf e1 e3 [e2 [H12 H23]]; apply mcomplus_seq_pgcb with e2; auto.
Qed.

(*Lemma br2_dec (E : set Event) (co lob : set Event -> Rln Event) (e1 e2 : Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  (big_rel2 E co lob) e1 e2 ->
  rel_seq (transitive_closure (rel_seq (maybe (obs_extra E co))
    (preorder_gcb E lob))) (maybe (complus E co)) e1 e2.
Proof.
intros Hrfwf Hcowf H12.
assert (rel_seq (transitive_closure (rel_seq (maybe (complus E co))
  (preorder_gcb E lob))) (maybe (complus E co)) e1 e2) as H12'.
  induction H12 as [e1 e2 H12 | e1 e2 e H1e _].
    apply seq_tc_reorg; auto.
    apply seq_tc_reorg2 with e; auto; apply mcomplus_trans; auto.

apply tc_seq_incl with (rel_seq (maybe (complus E co)) (preorder_gcb E lob)); auto.
  apply mcomplus_seq_pgcb_incl; auto.
Qed.*)

Lemma br2_base_dec (E : set Event) (co lob : set Event -> Rln Event) (e1 e2 : Event) :
  lob_well_formed E lob ->
  rf_well_formed E ->
  co_well_formed E co ->
  rel_seq
      (maybe (obsplus E co))
      (rel_seq
         (preorder_gcb E lob)
         (maybe (obsplus E co))) e1 e2 ->
  (transitive_closure (rel_union (obsplus E co) (preorder_gcb E lob))) e1 e2.
Proof.
intros Hlobwf Hrfwf Hcowf H12.

  destruct H12 as [x [H1x [y [Hxy Hy2]]]];
  inversion H1x as [Heq1x | Htc1x]; clear H1x;
  inversion Hy2 as [Heqy2 | Htcy2]; clear Hy2.

    apply _base; rewrite Heq1x; rewrite <- Heqy2; right; auto.
    rewrite <- Heq1x in Hxy; apply _trans with y; [right|apply _base; left]; auto.
    rewrite Heqy2 in Hxy; apply _trans with x; [left|apply _base; right]; auto.
    apply _trans with x; [left|apply _trans with y; [right|apply _base; left]]; auto.
Qed.

Lemma br2_dec (E : set Event) (co lob : set Event -> Rln Event) (e1 e2 : Event) :
  lob_well_formed E lob ->
  rf_well_formed E ->
  co_well_formed E co ->
  (big_rel2 E co lob) e1 e2 ->
  (transitive_closure (rel_union (obsplus E co) (preorder_gcb E lob))) e1 e2.
Proof.
intros Hlobwf Hrfwf Hcowf H12; induction H12.

  apply br2_base_dec; auto.

  generalize (br2_base_dec Hlobwf Hrfwf Hcowf H); clear H H12;
  intros H1e; apply tc_trans with e; auto.
Qed.

Lemma pgcb_in_lob (E : set Event) (lob : set Event -> Rln Event) :
  rel_incl (preorder_gcb E lob) (lob E).
Proof.
intros x y [? ?]; auto.
Qed.

Lemma mobs_extra_trans E (co lob : set Event -> Rln Event) (*x y z*) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  external_visibility E co lob ->
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
      apply (read_write_contrad y);
        [apply ran_rf_is_read with E x; destruct Hrfexy |
         apply dom_co_is_write with E co z]; auto.
    inversion Ht.

    destruct Hrfexy as [? ?]; right; left; apply rf_fr_is_co with y; auto.

    assert False as Ht.
      apply (read_write_contrad y); [apply ran_rf_is_read with E x; destruct Hrfexy |
                                       destruct Hcorfeyz as [e [Hye Hez]];
                                       apply dom_co_is_write with E co e]; auto.
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
                                       destruct Hcorfeyz as [e [Hye Hez]];
                                       apply dom_co_is_write with E co e]; auto.
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

Lemma mobs_extra_in_mcomplus E co x y :
  maybe (obs_extra E co) x y ->
  maybe (complus E co) x y.
Proof.
intros [Heqxy | Hobs_extraxy]; [left | right]; auto.
apply obs_extra_in_complus; auto.
Qed.

Lemma complus_seq_obs_extra_is_complus E co x y z :
  rf_well_formed E ->
  co_well_formed E co ->
  maybe (complus E co) x y ->
  maybe (obs_extra E co) y z ->
  maybe (complus E co) x z.
Proof.
intros Hrfwf Hcowf Hxy Hyz.
apply mcomplus_trans with y; auto.
apply mobs_extra_in_mcomplus; auto.
Qed.

Lemma mcomplus_seq_obs_extra_seq_pgcb E co lob e' e2 e'' e3 :
  rf_well_formed E ->
  co_well_formed E co ->
  maybe (complus E co) e' e2 ->
  obs_extra E co e2 e'' ->
  preorder_gcb E lob e'' e3 ->
  rel_seq (maybe (obs_extra E co)) (preorder_gcb E lob) e' e3.
Proof.
intros Hrfwf Hcowf H'2 H2'' H''3.
apply mcomplus_seq_pgcb_incl; auto.
exists e''; split; auto; apply complus_seq_obs_extra_is_complus with e2; auto; right; auto.
Qed.

Lemma complus_seq_tc E (co lob : set Event -> Rln Event) x y z :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  external_visibility E co lob ->
  maybe (complus E co) x y ->
  transitive_closure (rel_seq (maybe (obs_extra E co)) (preorder_gcb E lob)) y z ->
  transitive_closure (rel_seq (maybe (obs_extra E co)) (preorder_gcb E lob)) x z.
Proof.
intros Hrfwf Hcowf Hlobwf Hintv Hextv Hxy Hyz.
induction Hyz. Focus 2.
  apply _trans with e; auto.
  destruct H as [e' [H1' H'e]]; inversion H1' as [Heq1' | Hobs_extra1']; clear H1'.
    rewrite Heq1' in Hxy; apply mcomplus_seq_pgcb with e'; auto.
    apply mcomplus_seq_obs_extra_seq_pgcb with e1 e'; auto.

  destruct H as [e' [H1' H'e]]; inversion H1' as [Heq1' | Hobs_extra1']; clear H1'.
    apply _base; rewrite Heq1' in Hxy; apply mcomplus_seq_pgcb with e'; auto.
    apply _base; apply mcomplus_seq_obs_extra_seq_pgcb with e1 e'; auto.
Qed.

(** ** The relation pre_egc is a partial order over events
        viz,
        - it is defined over events
        - it is transitive
        - it is irreflexive

*)

Lemma pre_egc_in_evts (E : set Event) (co lob : set Event -> Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  Included Event (Union Event (dom (pre_egc E co lob)) (ran (pre_egc E co lob))) E.
Proof.
(** The relation pre_egc is defined over events and transitive:
    since pre_egc is defined as the transitive closure of a union of relations over events,
    it is trivially defined over events and trivially transitive.
*)
intros Hrfwf Hcowf Hlobwf; apply r_in_evts_implies_tc_in_evts; intros _x [x Hdom | y Hran].

  destruct Hdom as [y Hxy]; inversion Hxy as [Hrf | [Hco | [Hfr | [Hlob ?]]]].
    apply dom_rf_in_evts with y; auto.
    apply dom_co_in_evts with co y; auto.
    apply dom_fr_in_evts with co y; auto.
    destruct_lob_wf Hlobwf; apply dom_po_in_evts with y; auto.
  destruct Hran as [x Hxy]; inversion Hxy as [Hrf | [Hco | [Hfr | [Hlob ?]]]].
    apply ran_rf_in_evts with x; auto.
    apply ran_co_in_evts with co x; auto.
    apply ran_fr_in_evts with co x; auto.
    destruct_lob_wf Hlobwf; apply ran_po_in_evts with x; auto.
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
    unfold obsplus. unfold obs.
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

Lemma obsp_pgcb_in_ob (E : set Event) (co lob : set Event -> Rln Event) (x y : Event) :
  lob_well_formed E lob ->
  rf_well_formed E ->
  (rel_union (obsplus E co) (preorder_gcb E lob)) x y ->
  ob E co (lob E) x y.
Proof.
intros Hlobwf Hrfwf Hxy.
  inversion Hxy as [Hobsp | Hpgcb]; clear Hxy.
    induction Hobsp.
      apply _obs; auto.
      apply _ob with e; auto; apply _obs; auto.
    apply _lob; destruct Hpgcb as [Hlob ?]; auto.
Qed.

Lemma tc_obsp_pgcb_in_ob (E : set Event) (co lob : set Event -> Rln Event) (x y : Event) :
  lob_well_formed E lob ->
  rf_well_formed E ->
  transitive_closure (rel_union (obsplus E co) (preorder_gcb E lob)) x y ->
  ob E co (lob E) x y.
Proof.
intros Hlobwf Hrfwf Hxy; induction Hxy.

  apply obsp_pgcb_in_ob; auto.

  apply _ob with e; auto; apply obsp_pgcb_in_ob; auto.
Qed.

Lemma tc_union_irr (E : set Event) (co lob : set Event -> Rln Event) (x : Event) :
  lob_well_formed E lob ->
  rf_well_formed E ->
  external_visibility E co lob ->
  (transitive_closure (rel_union (obsplus E co) (preorder_gcb E lob))) x x ->
  False.
Proof.
intros Hlobwf Hrfwf Hev Hx; apply Hev; exists x.
apply tc_obsp_pgcb_in_ob; auto.
Qed.

Lemma tc_dec2 r e1 e3 :
  transitive_closure r e1 e3 ->
  exists e2, r e1 e2 /\ maybe (transitive_closure r) e2 e3.
Proof.
intros H13; induction H13.
  exists e2; split; auto; left; auto.
  exists e; split; auto; right; auto.
Qed.

Lemma rfi_seq_union_in_nrel E co lob x y z :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  rfi E x y ->
  rel_union (obsplus E co) (preorder_gcb E lob) y z ->
  transitive_closure (rel_union (obsplus E co) (preorder_gcb E lob)) x z.
Proof.
intros Hrfwf Hcowf Hlobwf Hrfi [Hobsp | Hpgcb].
  apply _base; left.
    apply rfi_seq_obsplus_in_obsplus with lob y; auto.

  apply _base; right; apply rfi_pgcb_is_pgcb with y; auto.
Qed.

Lemma rfi_seq_nrel_in_tc_obsp_pgcb E co lob x y z :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  rfi E x y ->
  nrel E co lob y z ->
  rfi E x z \/ transitive_closure (rel_union (obsplus E co) (preorder_gcb E lob)) x z.
Proof.
intros Hrfwf Hcowf Hlobwf Hxy [Heqyz | Htcyz].
  rewrite Heqyz in Hxy; left; auto.
  induction Htcyz as [y z Hyz|].
    right; apply rfi_seq_union_in_nrel with y; auto.
    right; apply tc_trans with e; auto;
      apply rfi_seq_union_in_nrel with e1; auto.
Qed.

Lemma rfi_irr E x :
  rfi E x x -> False.
Proof.
intros [Hrf Hint];
generalize (dom_rf_is_write Hrf); intro Hwx;
generalize (ran_rf_is_read Hrf); intro Hrx.
apply read_write_contrad with x; auto.
Qed.

(** ** The relation pre_egc is irreflexive:
*)
Lemma pre_egc_irr (E : set Event) (co lob : set Event -> Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  external_visibility E co lob ->
  (forall x : Event, ~ pre_egc E co lob x x).
Proof.
(** Reason by contradiction and suppose that there exists x s.t. (x,x) in pre_egc.*)
intros Hrfwf Hcowf Hlobwf Hint_vis Hext_vis x Hx.
generalize (pre_egc_dec Hrfwf Hcowf Hlobwf Hx); clear Hx; intros [Hx|[Hx|Hx]].

  destruct Hx as [y [Hnrel Hrfi]].
  generalize (rfi_seq_nrel_in_tc_obsp_pgcb Hrfwf Hcowf Hlobwf Hrfi Hnrel); intros [Hrfiy | Hnrely].
    apply rfi_irr with E y; auto.
    apply tc_union_irr with E co lob y; auto.

  apply Hint_vis; exists x; apply obsplus_in_scpv; auto.

  generalize (br2_dec Hlobwf Hrfwf Hcowf Hx); clear Hx; apply tc_union_irr; auto.
Qed.

Lemma pre_egc_partial_order (E : set Event) (co lob : set Event -> Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  external_visibility E co lob ->
  partial_order (pre_egc E co lob) E.
Proof.
intros Hrfwf Hcowf Hlobwf Hint_vis Hext_vis; split; [|split].
apply pre_egc_in_evts; auto.
intros e1 e2 e3 H12 H23; apply tc_trans with e2; auto.
apply pre_egc_irr; auto.
Qed.

(** ** A linear extension of the relation pre_egc satisfies the [external_global_completion] requirement
        viz,
        - gcb is a linear extension of the preorder_gcb relation
        - the read-froms extracted from gcb [gcb_rf] are the same as the axiomatic ones [rf]
        - the coherence extracted from gcb [gcb_co] are the same as the axiomatic ones [co]

*)

(** *** The gcb relation is a linear extension of the preorder_gcb relation *)
Lemma gcb_is_lin (E : set Event) (co lob : set Event -> Rln Event) (gcb : Rln Event) :
  linearisations (pre_egc E co lob) E gcb ->
  linearisations (preorder_gcb E lob) E gcb.
Proof.
(** By definition gcb is a linearisation of the relation pre_egc.

    Observe that preorder_gcb is included in pre_egc.

    Note that for all relation r2 included in another relation r1,
    a linearisation of the bigger relation r1
    is a linearisation of the smaller relation r2.

    Therefore a linearisation of the bigger relation pre_egc
    is also a linearisation of the smaller relation preorder_gcb. *)
intros Hlin; apply lin_of_big_is_lin_of_little with (pre_egc E co lob); auto.
intros x y Hxy; apply _base; right; right; right; auto.
Qed.

Lemma co_in_pre_egc E co lob :
  rel_incl (co E) (pre_egc E co lob).
Proof.
intros x y Hxy; apply _base; right; left; auto.
Qed.

Lemma co_gcb_incl (E : set Event) (co lob : set Event -> Rln Event) (gcb : Rln Event) :
  co_well_formed E co ->
  linearisations (pre_egc E co lob) E gcb ->
  rel_incl (co E) (gcb_co E gcb).
Proof.
intros Hcowf Hlin x y Hxy.
split.
  apply dom_co_is_write with E co y; auto.
  split.
    apply ran_co_is_write with E co x; auto.
    split.
      apply co_implies_same_loc with E co; auto.
      assert (pre_egc E co lob x y) as Hin.
        apply co_in_pre_egc; auto.
      generalize (lin_ext_prop E (pre_egc E co lob) gcb); intros [Hd1 Hd2].
      generalize (Hd1 Hlin); intros [Hincl ?]; apply Hincl; auto.
Qed.

Lemma gcb_co_incl (E : set Event) (co lob : set Event -> Rln Event) (gcb : Rln Event) :
  co_well_formed E co ->
  linearisations (pre_egc E co lob) E gcb ->
  rel_incl (gcb_co E gcb) (co E).
Proof.
intros Hcowf Hlin x y Hxy; generalize Hcowf; intros [Hincl Hlin_co].
  generalize (Hlin_co (loc x)); clear Hlin_co; intro Hlin_co; destruct_lin Hlin_co.
  destruct Hxy as [Hwx [Hwy [Heqloc Hgcbxy]]].
  generalize (lin_ext_prop E (pre_egc E co lob) gcb); intros [Hd1 Hd2].
  generalize (Hd1 Hlin); intros [? Hlin_gcb]; destruct Hlin_gcb as [Hpart_gcb ?]; destruct_part Hpart_gcb.
  assert (x <> y) as Hdiff.
    intro Heq; rewrite Heq in Hgcbxy; destruct Heq.
      apply Hirr with x; auto.
  assert (Intersection Event E (is_write_same_loc (loc x)) x) as Hx.
    split.
      apply Hinc; left; exists y; auto.
      split; auto.
  assert (Intersection Event E (is_write_same_loc (loc x)) y) as Hy.
    split.
      apply Hinc; right; exists x; auto.
      split; auto.
  generalize (Htot x y Hdiff Hx Hy); intros [?|Hyx]; auto.
  assert (gcb y x) as Hgcbyx.
    generalize (co_gcb_incl Hcowf Hlin y x Hyx); intros [? [? [? ?]]]; auto.
  assert False as Ht.
    apply Hirr with x; apply Htrans with y; auto.
  inversion Ht.
Qed.

Lemma gcb_coeq (E : set Event) (co lob : set Event -> Rln Event) (gcb : Rln Event) :
  co_well_formed E co ->
  linearisations (pre_egc E co lob) E gcb ->
  rel_equal (co E) (gcb_co E gcb).
Proof.
intros Hcowf Hlin; split.
apply co_gcb_incl with lob; auto.
apply gcb_co_incl with lob; auto.
Qed.

Lemma gcb_rf_incl (E : set Event) (co lob : set Event -> Rln Event) (gcb : Rln Event) :
  linearisations (pre_egc E co lob) E gcb ->
  rel_incl (gcb_rf E gcb) (rf E).
Proof.
intros Hlin; generalize (lin_ext_prop E (pre_egc E co lob) gcb);
  intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; intros x y Hxy.

  destruct_lin Hlso; destruct_part Hpart; destruct Hxy as [Hwx [Hry [Hloc [Hval [Hgcb Hnointerv]]]]];
  split; auto; split; auto; split; auto; split; auto; split; auto;
  apply Hinc; auto; [left; exists y | right; exists x]; auto.
Qed.

Lemma gcb_rf_is_wf (E : set Event) (co lob : set Event -> Rln Event) (gcb : Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  external_visibility E co lob ->
  linearisations (pre_egc E co lob) E gcb ->
  gcb_rf_wf E gcb.
Proof.
intros Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin r Hr.
generalize Hrfwf; intro Hrfwf'; destruct_rf_wf Hrfwf'; generalize (Hex_uni r Hr); intros [[w Hrf] Huni];
  generalize (lin_ext_prop E (pre_egc E co lob) gcb);
  intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso];
  exists w; split; [apply dom_rf_is_write with E r | split; [apply ran_rf_is_read with E w |
            split; [apply rf_implies_same_loc with E | split; [apply rf_implies_same_val with E | split]]]]; auto.

  apply Hincl; left; left; auto.

  intros [w' [Hw' [[Hgcbww' Hlocww']  [Hgcbw'r Hlocw'r]]]].
  generalize (gcb_co_incl Hcowf Hlin); intro Hcoincl.
  assert (is_write w) as Hw.
    apply dom_rf_is_write with E r; auto.
  assert (co E w w') as Hcoww'.
    apply Hcoincl; split; auto.
  assert (fr E co r w') as Hfrrw'.
    exists w; split; auto.
  assert (gcb r w') as Hgcbrw'.
    apply Hincl; left; right; right; left; auto.
  destruct_lin Hlso; destruct_part Hpart.
  generalize (Htrans w' r w' Hgcbw'r Hgcbrw'); intro Hcy; apply Hirr with w'; auto.
Qed.

Lemma rf_gcb_incl (E : set Event) (co lob : set Event -> Rln Event) (gcb : Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  external_visibility E co lob ->
  linearisations (pre_egc E co lob) E gcb ->
  rel_incl (rf E) (gcb_rf E gcb).
Proof.
intros Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin; intros x y Hxy.
assert (is_read y) as Hry.
  apply ran_rf_is_read with E x; auto.
generalize (gcb_rf_is_wf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin y Hry);
intros [w Hgcbrfwy];
generalize (gcb_rf_incl Hlin Hgcbrfwy);
intro Hrfwy; destruct_rf_wf Hrfwf;
generalize (Hex_uni y Hry); intros [Hex Huni]; clear Hex_uni;
generalize (Huni x w Hxy Hrfwy); intro Heq; rewrite Heq; auto.
Qed.

Lemma gcb_rfeq (E : set Event) (co lob : set Event -> Rln Event) (gcb : Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  external_visibility E co lob ->
  linearisations (pre_egc E co lob) E gcb ->
  rel_equal (rf E) (gcb_rf E gcb).
Proof.
intros Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin;
  generalize (lin_ext_prop E (pre_egc E co lob) gcb);
  intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; split.
  apply rf_gcb_incl with co lob; auto.
  apply gcb_rf_incl with co lob; auto.
Qed.

Lemma external_global_completion_gcb (E : set Event) (co lob : set Event -> Rln Event) (gcb : Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  external_visibility E co lob ->
  linearisations (pre_egc E co lob) E gcb ->
  external_global_completion E co lob gcb.
Proof.
intros Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin; split; [|split]; auto.
  apply gcb_is_lin with co; auto.
  apply gcb_rfeq with co lob; auto.
  apply gcb_coeq with lob; auto.
Qed.

(** ** All in all *)
Lemma external_visibility_implies_external_global_completion (E : set Event) (co lob : set Event -> Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  (external_visibility E co lob ->
   exists gcb : Rln Event, external_global_completion E co lob gcb).
Proof.
intros Hrfwf Hcowf Hlobwf Hint_vis Hext_vis.
generalize (pre_egc_partial_order Hrfwf Hcowf Hlobwf Hint_vis Hext_vis); intro Hpart;
generalize (order_ext Hpart); intros [gcb Hgcb]; exists gcb.
  apply external_global_completion_gcb; auto.
Qed.

(** * External Visibility <-> External Global Completion *)
Theorem external_visibility_gcb_equivalence (E : set Event) (co lob : set Event -> Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  (external_visibility E co lob <-> (exists gcb, external_global_completion E co lob gcb)).
Proof.
intros Hrfwf Hcowf Hlob_wf Hint_vis.
split.
  apply external_visibility_implies_external_global_completion; auto.
  apply external_global_completion_implies_external_visibility; auto.
Qed.

(** * External Completion *)

(** ** External Completion: Definitions *)
Definition fwd (E : set Event) (cb : Rln Event) (w r : Event) :=
  po E w r /\ cb r w /\ ~intervening_write (fun e1 e2 => po E e1 e2 /\ loc e1 = loc e2) w r.
Definition nfwd (E : set Event) (cb : Rln Event) (w r : Event) :=
  cb w r /\ ~intervening_write (fun e1 e2 => cb e1 e2 /\ loc e1 = loc e2) w r.
Definition cb_rf (E : set Event) (cb : Rln Event) (e1 e2 : Event) : Prop :=
  is_write e1 /\ is_read e2 /\ loc e1 = loc e2 /\ val e1 = val e2 /\
  ((fwd E cb e1 e2) \/ (nfwd E cb e1 e2)).

Definition cb_co (E : set Event) (cb : Rln Event) (e1 e2 : Event) : Prop :=
  order_to_co E cb e1 e2.

Definition external_completion (E : set Event) (co lob : set Event -> Rln Event) (cb : Rln Event): Prop :=
  (linearisations (lob E) E) cb /\
  rel_equal (rf E) (cb_rf E cb) /\ rel_equal (co E) (cb_co E cb).

Definition big_rel3 (E : set Event) (co lob : set Event -> Rln Event) :=
  tc_mobs_r_mobs E co (lob E).

(** ** External Completion: Lemmas that do _not_ need the existence of a External Global Completion order *)

Lemma rfe_in_cb E cb x y :
  rel_equal (rf E) (cb_rf E cb) ->
  rfe E x y ->
  cb x y.
Proof.
intros Hrfeq [Hrf Hext]; destruct Hrfeq as [Hrfincl ?]; generalize (Hrfincl x y Hrf); intros [? [? [? [? [Hfwd | Hnfwd]]]]]; auto.
  assert False as Ht.
    apply int_ext_contrad with E x y; auto.
    destruct Hfwd as [[? ?] ?]; auto.
  inversion Ht.
  destruct Hnfwd as [? ?]; auto.
Qed.

Lemma co_in_cb E co cb x y :
  rel_equal (co E) (cb_co E cb) ->
  co E x y ->
  cb x y.
Proof.
apply co_in_order; auto.
Qed.

Lemma fr_implies_cb_or_cb_minus_1 (E : set Event) (co lob : set Event -> Rln Event) (cb : Rln Event) (e1 e2 : Event) :
  co_well_formed E co ->
  linearisations (lob E) E cb ->
  fr E co e1 e2 ->
  cb e1 e2 \/ cb e2 e1.
Proof.
intros Hlin H12.
apply fr_implies_order_or_order_minus_1 with (lob E); auto.
Qed.

Lemma fr_in_cb E co lob cb x y :
  co_well_formed E co ->
  linearisations (lob E) E cb ->
  rel_equal (rf E) (cb_rf E cb) ->
  rel_equal (co E) (cb_co E cb) ->
  fr E co x y ->
  cb x y.
Proof.
intros Hcowf Hlin Hrfeq Hcoeq Hfr.
generalize (fr_implies_cb_or_cb_minus_1 lob Hcowf Hlin Hfr); intros [? | Hgcbyx]; auto;
generalize Hfr; intros [w [Hw [Hrf Hco]]].
destruct Hrfeq as [Hinclrf ?]; generalize (Hinclrf w x Hrf); intros [? [? [? [? [Hfwd | Hnfwd]]]]].
  destruct Hfwd as [? [Hcbxw ?]].
  assert False as Ht.
    generalize (lin_ext_prop E (lob E) cb); intros [Himpl ?];
    generalize (Himpl Hlin); intros [Hincl Hlso]; destruct_lin Hlso; destruct_part Hpart; apply (Hirr x).
    apply Htrans with w; auto; apply Htrans with y; auto.
    destruct Hcoeq as [Hcoincl ?]; apply Hcoincl; auto.
  inversion Ht.

  destruct Hnfwd as [Hcbwx Hnointerv]; (*here: observe the different structure from fr_in_gcb, need to refactor*)
  generalize (co_in_cb co w y Hcoeq Hco); intro Hcbwy.
  assert False as Ht.
    apply Hnointerv; exists y; split; [|split; split]; auto.
      apply ran_co_is_write with E co w; auto.
      apply co_implies_same_loc with E co; auto.
    generalize (fr_implies_same_loc Hcowf Hfr); auto.
  inversion Ht.
Qed.

Lemma cb_path_ob_dec (E : set Event) (co lob : set Event -> Rln Event) (cb : Rln Event) (e1 e2 : Event) :
  rel_equal (rf E) (cb_rf E cb) ->
  rel_equal (co E) (cb_co E cb) ->
  ob E co (lob E) e1 e2 ->
  obsplus E co e1 e2 \/ big_rel3 E co lob e1 e2.
Proof.
intros Hrfeq Hcoeq H12.
induction H12 as [e1 e2 Hobs | e1 e2 Hlob |]; auto.
  left; apply _base; auto.
  right; apply _base; exists e1; split; [left|exists e2; split; [|left]]; auto.
  inversion IHob1 as [Hob1 | Hbr1]; clear IHob1; inversion IHob2 as [Hob2 | Hbr2]; clear IHob2.
    left; apply tc_trans with e; auto.
    right; apply tc_seq_left with e; auto; [intros x y z Hxy Hyz; apply mop_trans with y | right]; auto.
    right; apply tc_seq_reorg with e; auto; [intros x y z Hxy Hyz; apply mop_trans with y |right]; auto.
    right; apply tc_trans with e; auto.
Qed.

(** ** External Completion -> External Visibility lemmas *)
Lemma big_rel3_irr E co lob cb x :
  rf_well_formed E ->
  co_well_formed E co ->
  linearisations (lob E) E cb ->
  rel_equal (rf E) (cb_rf E cb) ->
  rel_equal (co E) (cb_co E cb) ->
  ~(big_rel3 E co lob x x).
Proof.
intros Hrfwf Hcowf Hlin Hrfeq Hcoeq; apply tc_mobs_r_mobs_irr with cb; auto; intros e1 e2 H12.
  apply rfe_in_cb with E; auto.
  apply co_in_cb with E co; auto.
  apply fr_in_cb with E co lob; auto.
Qed.

Lemma external_completion_implies_external_visibility (E : set Event) (co lob : set Event -> Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  internal_visibility E co ->
  (exists cb : Rln Event, external_completion E co lob cb) ->
  external_visibility E co lob.
Proof.
intros Hrfwf Hcowf Hintv [cb [Hlin [Hrf Hco]]] [x Hx].
generalize (cb_path_ob_dec lob Hrf Hco Hx); intros [Hobs | Hbr3].

  apply Hintv; exists x; apply obsplus_in_scpv; auto.

  apply (big_rel3_irr Hrfwf Hcowf Hlin Hrf Hco Hbr3); auto.
Qed.

(** ** External Visibility -> External Completion lemmas *)

Definition pre_ec (E : set Event) (co lob : set Event -> Rln Event) :=
  transitive_closure (rel_union (rfe E) (rel_union (co E) (rel_union (fr E co) (lob E)))).
 Definition big_rel4 E co lob :=
  transitive_closure (rel_seq (maybe (obs_extra E co)) (rel_seq (lob E) (maybe (obs_extra E co)))).

Lemma rfe_obs_extra_in_obs_extra E co e1 e e2 :
  co_well_formed E co ->
  rf_well_formed E ->
  rfe E e1 e ->
  obs_extra E co e e2 ->
  obs_extra E co e1 e2.
Proof.
intros Hcowf Hrfwf H1e [Hrfe | [Hco | [Hfr | [Hcorfe | Hfrrfe]]]].
  assert False as Ht.
    apply read_write_contrad with e;
      [apply ran_rf_is_read with E e1; destruct H1e | apply dom_rf_is_write with E e2; destruct Hrfe]; auto.
  inversion Ht.

  assert False as Ht.
    apply read_write_contrad with e;
      [apply ran_rf_is_read with E e1; destruct H1e | apply dom_co_is_write with E co e2]; auto.
  inversion Ht.

  right; left; apply rf_fr_is_co with e; destruct H1e; auto.

  assert False as Ht.
    apply read_write_contrad with e;
      [apply ran_rf_is_read with E e1; destruct H1e |
       destruct Hcorfe as [e' [? ?]]; apply dom_co_is_write with E co e']; auto.
  inversion Ht.

  destruct Hfrrfe as [e' [Hfr Hrfe]];
  right; right; right; left; exists e'; split; auto; apply rf_fr_is_co with e; destruct H1e; auto.
Qed.

Lemma rfe_br4_in_br4 E co lob e1 e e2 :
  co_well_formed E co ->
  rf_well_formed E ->
  rfe E e1 e ->
  big_rel4 E co lob e e2 ->
  big_rel4 E co lob e1 e2.
Proof.
intros Hcowf Hrfwf H1e He2; induction He2 as [e e2 He2 | e e2 e' Hee'].
  destruct He2 as [x [Hex [y [Hxy Hy2]]]].
  inversion Hex as [Heqex | Hcomplusex]; clear Hex.
    rewrite Heqex in H1e; clear Heqex; apply _base; exists x; split; auto; [right; left|exists y; split]; auto.
    apply _base; exists x; split; auto; [right; apply rfe_obs_extra_in_obs_extra with e | exists y; split]; auto.

  apply _trans with e'; auto.
  destruct Hee' as [x [Hex [y [Hxy Hye']]]].
  inversion Hex as [Heqex | Hcomplusex]; clear Hex.
    rewrite Heqex in H1e; clear Heqex; exists x; split; auto; [right; left|exists y; split]; auto.
    exists x; split; auto; [right; apply rfe_obs_extra_in_obs_extra with e|exists y; split]; auto.
Qed.

Lemma co_obs_extra_in_obs_extra E co e1 e e2 :
  co_well_formed E co ->
  co E e1 e ->
  obs_extra E co e e2 ->
  obs_extra E co e1 e2.
Proof.
intros Hrfwf H1e [Hrfe | [Hco | [Hfr | [Hcorfe | Hfrrfe]]]].

  right; right; right; left; exists e; split; auto.
  right; left; apply co_trans with e; auto.
  assert False as Ht.
    apply read_write_contrad with e;
    [apply dom_fr_is_read with E co e2 | apply ran_co_is_write with E co e1]; auto.
  inversion Ht.
  destruct Hcorfe as [e' [Hco Hrfe]]; right; right; right; left; exists e'; split; auto; apply co_trans with e; auto.
  destruct Hfrrfe as [e'[Hfr Hrfe]]; assert False as Ht.
    apply read_write_contrad with e;
    [apply dom_fr_is_read with E co e' | apply ran_co_is_write with E co e1]; auto.
  inversion Ht.
Qed.

Lemma co_br4_in_br4 E co lob e1 e e2 :
  co_well_formed E co ->
  co E e1 e ->
  big_rel4 E co lob e e2 ->
  big_rel4 E co lob e1 e2.
Proof.
intros Hcowf H1e He2; induction He2 as [e e2 He2 | e e2 e' Hee'].
  destruct He2 as [e' [Hee' He'2]]; inversion Hee' as [Heqee' | Hcomplusee']; clear Hee';
    apply _base; exists e'; split; auto; right; [rewrite Heqee' in H1e; right; left | apply co_obs_extra_in_obs_extra with e]; auto.
    apply _trans with e'; auto; destruct Hee' as [e'' [Hee'' He''e']]; exists e''; split; auto;
    inversion Hee'' as [Heqee'' | Hcomplusee'']; clear Hee'';
    right; [rewrite <- Heqee''; right; left | apply co_obs_extra_in_obs_extra with e]; auto.
Qed.

Lemma fr_obs_extra_in_obs_extra E co e1 e e2 :
  rf_well_formed E ->
  co_well_formed E co ->
  fr E co e1 e ->
  obs_extra E co e e2 ->
  obs_extra E co e1 e2.
Proof.
intros Hrfwf Hcowf H1e [Hrfe | [Hco | [Hfr | [Hcorfe | Hfrrfe]]]].

  right; right; right; right; exists e; split; auto.
  right; right; left; apply fr_co_is_fr with e; auto.
  assert False as Ht.
    apply read_write_contrad with e;
    [apply dom_fr_is_read with E co e2 | apply ran_fr_is_write with E co e1]; auto.
  inversion Ht.
  destruct Hcorfe as [e' [Hco Hrfe]]; right; right; right; right; exists e'; split; auto; apply fr_co_is_fr with e; auto.
  destruct Hfrrfe as [e' [Hfr Hrfe]]; assert False as Ht.
    apply read_write_contrad with e;
    [apply dom_fr_is_read with E co e' | apply ran_fr_is_write with E co e1]; auto.
  inversion Ht.
Qed.

Lemma fr_br4_in_br4 E co lob e1 e e2 :
  rf_well_formed E ->
  co_well_formed E co ->
  fr E co e1 e ->
  big_rel4 E co lob e e2 ->
  big_rel4 E co lob e1 e2.
Proof.
intros Hrfwf Hcowf H1e He2; induction He2 as [e e2 He2 | e e2 e' Hee'].
  destruct He2 as [e' [Hee' He'2]]; inversion Hee' as [Heqee' | Hcomplusee']; clear Hee';
    apply _base; exists e'; split; auto; right; [rewrite Heqee' in H1e; right; right; left | apply fr_obs_extra_in_obs_extra with e]; auto.
    apply _trans with e'; auto; destruct Hee' as [e'' [Hee'' He''e']]; exists e''; split; auto;
    inversion Hee'' as [Heqee'' | Hcomplusee'']; clear Hee'';
    right; [rewrite <- Heqee''; right; right; left | apply fr_obs_extra_in_obs_extra with e]; auto.
Qed.

Lemma lob_br4_in_br4 E (co lob : set Event -> Rln Event) e1 e e2 :
  rf_well_formed E ->
  co_well_formed E co ->
  lob E e1 e ->
  big_rel4 E co lob e e2 ->
  big_rel4 E co lob e1 e2.
Proof.
intros Hrfwf Hcowf H1e He2; induction He2 as [e e2 He2 | e e2 e' Hee'].

  apply _trans with e; [exists e1; split; [left | exists e; split; [|left]]|apply _base]; auto.
  apply tc_trans with e'; auto.
    apply _trans with e; [exists e1; split; [left | exists e; split; [|left]]|apply _base]; auto.
Qed.

Lemma pre_ec_dec (E : set Event) (co lob : set Event -> Rln Event) (e1 e2 : Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  pre_ec E co lob e1 e2 ->
  (obs_extra E co) e1 e2 \/ (big_rel4 E co lob) e1 e2.
Proof.
intros Hrfwf Hcowf H12; induction H12 as [e1 e2 H12 | e1 e2 e H1e He2].
  inversion H12 as [Hrf | [Hco | [Hfr | Hlob]]].
    left; left; auto.
    left; right; left; auto.
    left; right; right; left; auto.
    right; apply _base; exists e1; split; [left | exists e2; split; [| left]]; auto.

  inversion H1e as [Hrfe | [Hco | [Hfr | Hlob]]]; clear H1e;
  inversion IHHe2 as [Hcpe2 | Hbr4e2]; clear IHHe2.
    left; apply rfe_obs_extra_in_obs_extra with e; auto.
    right; apply rfe_br4_in_br4 with e; auto.
    left; apply co_obs_extra_in_obs_extra with e; auto.
    right; apply co_br4_in_br4 with e; auto.
    left; apply fr_obs_extra_in_obs_extra with e; auto.
    right; apply fr_br4_in_br4 with e; auto.
    right; apply _base; exists e1; split; [left |exists e; split; [|right]]; auto.
    right; apply lob_br4_in_br4 with e; auto.
Qed.

Lemma obsplus_in_ob E co lob :
  rel_incl (obsplus E co) (ob E co (lob E)).
Proof.
intros x y Hxy; induction Hxy.
  apply _obs; auto.
  apply _ob with e; auto; apply _obs; auto.
Qed.

Lemma tc_ob_in_ob E co lob x y :
  transitive_closure (ob E co (lob E)) x y ->
  ob E co (lob E) x y.
Proof.
intros Hxy; induction Hxy; auto.
  apply _ob with e; auto.
Qed.

Lemma pre_ec_irr (E : set Event) (co lob : set Event -> Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  external_visibility E co lob ->
  (forall x : Event, ~ pre_ec E co lob x x).
Proof.
intros Hrfwf Hcowf Hlobwf Hint_vis Hext_vis x Hx.
generalize (pre_ec_dec Hrfwf Hcowf Hx); clear Hx; intros [Hx|Hx].
  apply complus_irr with E co x; auto. apply obs_extra_in_complus; auto.

    generalize (tc_seq_inv Hx); intros [e1 [e2 [Hx1 [H12 H2x]]]].
    assert (transitive_closure (rel_seq (lob E) (maybe (obs_extra E co))) e1 e2) as H12'.
      apply tc_incl with (rel_seq (rel_seq (lob E) (maybe (obs_extra E co))) (maybe (obs_extra E co))); auto.
      intros a d [c [[b [Hab Hbc]] Hcd]]; exists b; split; auto.
        generalize Hbc Hcd; apply mobs_extra_trans with lob; auto.
      clear H12.
    generalize (mobs_extra_trans Hrfwf Hcowf Hlobwf Hint_vis Hext_vis); intro Htr.
    generalize (seq_tc_seq e1 Htr H12' H2x Hx1); intro H11; clear Hx Hx1 H2x H12' x.
    rewrite obs_extra_obsp_eq in H11; auto.
    assert (transitive_closure (ob E co (lob E)) e1 e1) as Htc11.
      apply tc_incl with (rel_seq (lob E) (maybe (obsplus E co))); auto.
      intros x y [z [Hxy Hyz]]; inversion Hyz as [Heqyz | Hoyz]; clear Hyz.
        rewrite <- Heqyz; apply _lob; auto.
        apply _ob with z; [apply _lob|]; auto.
        apply obsplus_in_ob; auto.

    apply Hext_vis; exists e1; auto.
      apply tc_ob_in_ob; auto.
Qed.

Lemma pre_ec_in_evts (E : set Event) (co lob : set Event -> Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  Included Event (Union Event (dom (pre_ec E co lob)) (ran (pre_ec E co lob))) E.
Proof.
intros Hrfwf Hcowf Hlobwf; apply r_in_evts_implies_tc_in_evts; intros _x [x Hdom | y Hran].

  destruct Hdom as [y Hxy]; inversion Hxy as [[Hrf ?] | [Hco | [Hfr | Hlob]]].
    apply dom_rf_in_evts with y; auto.
    apply dom_co_in_evts with co y; auto.
    apply dom_fr_in_evts with co y; auto.
    destruct_lob_wf Hlobwf; apply dom_po_in_evts with y; auto.
  destruct Hran as [x Hxy]; inversion Hxy as [[Hrf ?] | [Hco | [Hfr | Hlob]]].
    apply ran_rf_in_evts with x; auto.
    apply ran_co_in_evts with co x; auto.
    apply ran_fr_in_evts with co x; auto.
    destruct_lob_wf Hlobwf; apply ran_po_in_evts with x; auto.
Qed.

Lemma pre_ec_partial_order (E : set Event) (co lob : set Event -> Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  external_visibility E co lob ->
  partial_order (pre_ec E co lob) E.
Proof.
intros Hrfwf Hcowf Hlobwf Hint_vis Hext_vis; split; [|split].
apply pre_ec_in_evts; auto.
intros e1 e2 e3 H12 H23; apply tc_trans with e2; auto.
apply pre_ec_irr; auto.
Qed.

Lemma cb_is_lin (E : set Event) (co lob : set Event -> Rln Event) (cb : Rln Event) :
  linearisations (pre_ec E co lob) E cb ->
  linearisations (lob E) E cb.
Proof.
intros Hlin; apply lin_of_big_is_lin_of_little with (pre_ec E co lob); auto.
intros x y Hxy; apply _base; right; right; right; auto.
Qed. (*exactly the same as gcb_is_lin*)

Lemma co_in_pre_ec (E : set Event) (co lob : set Event -> Rln Event) :
  rel_incl (co E) (pre_ec E co lob).
Proof.
intros x y Hxy; apply _base; right; left; auto.
Qed.

Lemma co_cb_incl (E : set Event) (co lob : set Event -> Rln Event) (cb : Rln Event) :
  co_well_formed E co ->
  linearisations (pre_ec E co lob) E cb ->
  rel_incl (co E) (cb_co E cb).
Proof.
intros Hcowf Hlin x y Hxy.
split.
  apply dom_co_is_write with E co y; auto.
  split.
    apply ran_co_is_write with E co x; auto.
    split.
      apply co_implies_same_loc with E co; auto.
      assert (pre_ec E co lob x y) as Hin.
        apply co_in_pre_ec; auto.
      generalize (lin_ext_prop E (pre_ec E co lob) cb); intros [Hd1 Hd2].
      generalize (Hd1 Hlin); intros [Hincl ?]; apply Hincl; auto.
Qed.

Lemma cb_co_incl (E : set Event) (co lob : set Event -> Rln Event) (cb : Rln Event) :
  co_well_formed E co ->
  linearisations (pre_ec E co lob) E cb ->
  rel_incl (cb_co E cb) (co E).
Proof.
intros Hcowf Hlin x y Hxy; generalize Hcowf; intros [Hincl Hlin_co].
  generalize (Hlin_co (loc x)); clear Hlin_co; intro Hlin_co; destruct_lin Hlin_co.
  destruct Hxy as [Hwx [Hwy [Heqloc Hcbxy]]].
  generalize (lin_ext_prop E (pre_ec E co lob) cb); intros [Hd1 Hd2].
  generalize (Hd1 Hlin); intros [? Hlin_cb]; destruct Hlin_cb as [Hpart_cb ?]; destruct_part Hpart_cb.
  assert (x <> y) as Hdiff.
    intro Heq; rewrite Heq in Hcbxy; destruct Heq.
      apply Hirr with x; auto.
  assert (Intersection Event E (is_write_same_loc (loc x)) x) as Hx.
    split.
      apply Hinc; left; exists y; auto.
      split; auto.
  assert (Intersection Event E (is_write_same_loc (loc x)) y) as Hy.
    split.
      apply Hinc; right; exists x; auto.
      split; auto.
  generalize (Htot x y Hdiff Hx Hy); intros [?|Hyx]; auto.
  assert (cb y x) as Hcbyx.
    generalize (co_cb_incl Hcowf Hlin y x Hyx); intros [? [? [? ?]]]; auto.
  assert False as Ht.
    apply Hirr with x; apply Htrans with y; auto.
  inversion Ht.
Qed.

Lemma cb_coeq (E : set Event) (co lob : set Event -> Rln Event) (cb : Rln Event) :
  co_well_formed E co ->
  linearisations (pre_ec E co lob) E cb ->
  rel_equal (co E) (cb_co E cb).
Proof.
intros Hcowf Hlin; split.
  apply co_cb_incl with lob; auto.
  apply cb_co_incl with lob; auto.
Qed.

Lemma cb_rf_incl (E : set Event) (co lob : set Event -> Rln Event) (cb : Rln Event) :
  linearisations (pre_ec E co lob) E cb ->
  rel_incl (cb_rf E cb) (rf E).
Proof.
intros Hlin; generalize (lin_ext_prop E (pre_ec E co lob) cb);
  intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; intros x y Hxy.

  destruct_lin Hlso; destruct_part Hpart; destruct Hxy as [Hwx [Hry [Hloc [Hval [Hfwd | Hnfwd]]]]].

  destruct Hfwd as [Hpo [Hcbyx Hnointerv]].
    split; auto; split; auto; split; auto; split; auto; split; auto.
    apply dom_po_in_evts with y; auto.
    apply ran_po_in_evts with x; auto.

  destruct Hnfwd as [Hcb Hnoniterv].
    split; auto; split; auto; split; auto; split; auto; split; auto;
    apply Hinc; auto; [left; exists y | right; exists x]; auto.
Qed. (*almost the same as cb_rf_incl*)

Definition cb_rf_wf (E : set Event) (gcb : Rln Event) :=
   forall r : Event, is_read r -> exists w : Event, cb_rf E gcb w r.

Lemma cb_rf_is_wf (E : set Event) (co lob : set Event -> Rln Event) (cb : Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  external_visibility E co lob ->
  linearisations (pre_ec E co lob) E cb ->
  cb_rf_wf E cb.
Proof.
intros Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin r Hr.
unfold gcb_rf.
generalize Hrfwf; intro Hrfwf'; destruct_rf_wf Hrfwf'; generalize (Hex_uni r Hr); intros [[w Hrf] Huni];
  generalize (lin_ext_prop E (pre_ec E co lob) cb);
  intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso];
  exists w; split; [apply dom_rf_is_write with E r | split; [apply ran_rf_is_read with E w |
            split; [apply rf_implies_same_loc with E | split; [apply rf_implies_same_val with E | ]]]]; auto.

  assert (E w) as HEw.
    apply dom_rf_in_evts with r; auto.
  assert (E r) as HEr.
    apply ran_rf_in_evts with w; auto.

  generalize (int_or_ext E w r HEw HEr); intros [Hint | Hext].

  Focus 2.

  assert (cb w r) as Hcbwr.
    apply Hincl; left; left; split; auto.
  right; split; auto.

    (*this should be a lemma*)
    intros [w' [Hw' [[Hcbww' Hlocww'] [Hcbw'r Hlocw'r]]]].
      generalize (cb_co_incl Hcowf Hlin); intro Hcoincl.
      assert (is_write w) as Hw.
        apply dom_rf_is_write with E r; auto.
      assert (co E w w') as Hcoww'.
        apply Hcoincl; split; auto.
      assert (fr E co r w') as Hfrrw'.
        exists w; split; auto.
      assert (cb r w') as Hcbrw'.
        apply Hincl; left; right; right; left; auto.
      destruct_lin Hlso; destruct_part Hpart.
      generalize (Htrans w' r w' Hcbw'r Hcbrw'); intro Hcy; apply Hirr with w'; auto.
    (*up to here*)

  assert (cb w r \/ cb r w) as Hor.
    destruct_lin Hlso; apply Htot; auto.
    intro Heq; rewrite Heq in Hrf; apply read_write_contrad with r;
      [apply ran_rf_is_read with E r | apply dom_rf_is_write with E r]; auto.

  inversion Hor as [Hcbwr | Hcbrw]; clear Hor; [right|left]; split; auto.

    (*this should be a lemma*)
    intros [w' [Hw' [[Hcbww' Hlocww'] [Hcbw'r Hlocw'r]]]].
      generalize (cb_co_incl Hcowf Hlin); intro Hcoincl.
      assert (is_write w) as Hw.
        apply dom_rf_is_write with E r; auto.
      assert (co E w w') as Hcoww'.
        apply Hcoincl; split; auto.
      assert (fr E co r w') as Hfrrw'.
        exists w; split; auto.
      assert (cb r w') as Hcbrw'.
        apply Hincl; left; right; right; left; auto.
      destruct_lin Hlso; destruct_part Hpart.
      generalize (Htrans w' r w' Hcbw'r Hcbrw'); intro Hcy; apply Hirr with w'; auto.
    (*up to here*)

  apply rfi_implies_po with co; auto; split; auto.
  split; auto.
  intros [w' [Hw' [[Hpoww' Hlocww'] [Hpow'r Hlocw'r]]]].
    assert (is_write w) as Hw.
      apply dom_rf_is_write with E r; auto.
    assert (co E w w') as Hcoww'.
      apply posWW_is_coi; auto.
    assert (fr E co r w') as Hfrrw'.
      exists w; split; auto.
    apply Hint_vis; exists w'; apply _trans with r; auto; [right; right; right; split|apply _base]; auto.
Qed.

Lemma rf_cb_incl (E : set Event) (co lob : set Event -> Rln Event) (cb : Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  external_visibility E co lob ->
  linearisations (pre_ec E co lob) E cb ->
  rel_incl (rf E) (cb_rf E cb).
Proof.
intros Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin; intros x y Hxy.
assert (is_read y) as Hry.
  apply ran_rf_is_read with E x; auto.
generalize (cb_rf_is_wf Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin y Hry);
intros [w Hcbrfwy];
generalize (cb_rf_incl Hlin Hcbrfwy);
intro Hrfwy; destruct_rf_wf Hrfwf;
generalize (Hex_uni y Hry); intros [Hex Huni]; clear Hex_uni;
generalize (Huni x w Hxy Hrfwy); intro Heq; rewrite Heq; auto.
Qed.

Lemma cb_rfeq (E : set Event) (co lob : set Event -> Rln Event) (cb : Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  external_visibility E co lob ->
  linearisations (pre_ec E co lob) E cb ->
  rel_equal (rf E) (cb_rf E cb).
Proof.
intros Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin; generalize (lin_ext_prop E (pre_ec E co lob) cb);
  intros [Himpl _]; generalize (Himpl Hlin); clear Himpl; intros [Hincl Hlso]; split.
  apply rf_cb_incl with co lob; auto.
  apply cb_rf_incl with co lob; auto.
Qed.

Lemma external_completion_cb (E : set Event) (co lob : set Event -> Rln Event) (cb : Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  external_visibility E co lob ->
  linearisations (pre_ec E co lob) E cb ->
  external_completion E co lob cb.
Proof.
intros Hrfwf Hcowf Hlobwf Hint_vis Hext_vis Hlin; split; [|split]; auto.
  apply cb_is_lin with co; auto.
  apply cb_rfeq with co lob; auto.
  apply cb_coeq with lob; auto.
Qed.

Lemma external_visibility_implies_external_completion (E : set Event) (co lob : set Event -> Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  external_visibility E co lob ->
  (exists cb, external_completion E co lob cb).
Proof.
intros Hrfwf Hcowf Hlobwf Hintv Hextv.
generalize (pre_ec_partial_order Hrfwf Hcowf Hlobwf Hintv Hextv); intro Hpart;
generalize (order_ext Hpart); intros [cb Hcb]; exists cb.
  apply external_completion_cb; auto.
Qed.

(** ** External Visibility <-> External Completion equivalence **)
Theorem external_visibility_cb_equivalence (E : set Event) (co lob : set Event -> Rln Event) :
  rf_well_formed E ->
  co_well_formed E co ->
  lob_well_formed E lob ->
  internal_visibility E co ->
  (external_visibility E co lob <-> (exists cb, external_completion E co lob cb)).
Proof.
intros Hrfwf Hcowf Hlobwf Hint_vis.
split.
  apply external_visibility_implies_external_completion; auto.
  apply external_completion_implies_external_visibility; auto.
Qed.
