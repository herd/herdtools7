;;****************************************************************************;;
;;                                ASLRef                                      ;;
;;****************************************************************************;;
;;
;; SPDX-FileCopyrightText: Copyright 2025 Arm Limited and/or its affiliates <open-source-office@arm.com>
;; SPDX-License-Identifier: BSD-3-Clause
;; 
;;****************************************************************************;;
;; Disclaimer:                                                                ;;
;; This material covers both ASLv0 (viz, the existing ASL pseudocode language ;;
;; which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  ;;
;; experimental, and as yet unreleased version of ASL.                        ;;
;; This material is work in progress, more precisely at pre-Alpha quality as  ;;
;; per Arm’s quality standards.                                               ;;
;; In particular, this means that it would be premature to base any           ;;
;; production tool development on this material.                              ;;
;; However, any feedback, question, query and feature request would be most   ;;
;; welcome; those can be sent to Arm’s Architecture Formal Team Lead          ;;
;; Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    ;;
;; herdtools7 github repository.                                              ;;
;;****************************************************************************;;

(in-package "ASL")

(include-book "interp")


(local (in-theory (enable ev_error->desc-when-wrong-kind)))

(defthm not-trace-abort-of-v_to_bool
  (not (equal (ev_error->desc (v_to_bool x))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable v_to_bool))))

(defthm not-trace-abort-of-v_to_int
  (not (equal (ev_error->desc (v_to_int x))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable v_to_int))))

(defthm not-trace-abort-of-v_to_label
  (not (equal (ev_error->desc (v_to_label x))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable v_to_label))))

(defthm not-trace-abort-of-env-find-global
  (not (equal (ev_error->desc (env-find-global v env))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable env-find-global))))

(defthm not-trace-abort-of-tick_loop_limit
  (not (equal (ev_error->desc (tick_loop_limit x))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable tick_loop_limit))))

(defthm not-trace-abort-of-rethrow_implicit
  (equal (ev_error->desc (rethrow_implicit throw blkres bt))
         (ev_error->desc blkres))
  :hints(("Goal" :in-theory (enable rethrow_implicit))))

(defthm not-trace-abort-of-bitvec_fields_to_record!
  (not (equal (ev_error->desc (bitvec_fields_to_record! fields slices rec bv width))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable bitvec_fields_to_record!
                                    (:i bitvec_fields_to_record!))
          :induct t)))

(defthm not-trace-abort-of-bitvec_fields_to_record
  (not (equal (ev_error->desc (bitvec_fields_to_record fields pairs res v))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable bitvec_fields_to_record))))

(defthm not-trace-abort-of-check_two_ranges_non_overlapping
  (not (equal (ev_error->desc (check_two_ranges_non_overlapping x y))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable check_two_ranges_non_overlapping))))

(defthm not-trace-abort-of-check_non_overlapping_slices-1
  (not (equal (ev_error->desc (check_non_overlapping_slices-1 x y))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable check_non_overlapping_slices-1)
          :induct t)))

(defthm not-trace-abort-of-check_non_overlapping_slices
  (not (equal (ev_error->desc (check_non_overlapping_slices x))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable check_non_overlapping_slices)
          :induct t)))

(defthm not-trace-abort-of-vbv-to-int
  (not (equal (ev_error->desc (vbv-to-int vec))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable vbv-to-int))))

(defthm not-trace-abort-of-check-bad-slices
  (not (equal (ev_error->desc (check-bad-slices width slices))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable check-bad-slices)
          :induct t)))

(defthm not-trace-abort-of-check_recurse_limit
  (not (equal (ev_error->desc (check_recurse_limit env name res))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable check_recurse_limit))))

(defthm not-trace-abort-of-write_to_bitvector
  (not (equal (ev_error->desc (write_to_bitvector pairs vec val))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable write_to_bitvector))))

(defthm not-trace-abort-of-eval_primitive
  (not (equal (ev_error->desc (eval_primitive name params args))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable eval_primitive))))

(defthm not-trace-abort-of-eval_binop
  (not (equal (ev_error->desc (eval_binop op arg1 arg2))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable eval_binop))))

(defthm not-trace-abort-of-eval_unop
  (not (equal (ev_error->desc (eval_unop op arg))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable eval_unop))))

(defthm not-trace-abort-of-eval_pattern_mask
  (not (equal (ev_error->desc (eval_pattern_mask val mask))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable eval_pattern_mask))))

(defthm not-trace-abort-of-get_field!
  (not (equal (ev_error->desc (get_field! field rec))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable get_field!))))

(defthm not-trace-abort-of-get_field
  (not (equal (ev_error->desc (get_field field rec))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable get_field))))

(defthm not-trace-abort-of-map-get_field!
  (not (equal (ev_error->desc (map-get_field! field rec))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable map-get_field!))))

(defthm not-trace-abort-of-map-get_field
  (not (equal (ev_error->desc (map-get_field field rec))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable map-get_field))))

(defthm not-trace-abort-of-concat_bitvectors
  (not (equal (ev_error->desc (concat_bitvectors vals))
              "Trace abort"))
  :hints(("Goal" :in-theory (enable concat_bitvectors))))

