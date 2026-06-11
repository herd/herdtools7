;;****************************************************************************;;
;;                              ACL2 ASL Library                              ;;
;;****************************************************************************;;
;;
;; SPDX-FileCopyrightText: Copyright 2026 Arm Limited and/or its affiliates <open-source-office@arm.com>
;; SPDX-License-Identifier: BSD-3-Clause
;; 
;; ****************************************************************************;;



(in-package "ASL")

(include-book "centaur/fgl/fty" :dir :system)
(include-book "centaur/fgl/checks" :dir :system)
(include-book "centaur/fgl/config" :dir :system)
(include-book "centaur/fgl/helper-utils" :dir :system)
(include-book "defs")
(include-book "tools/easy-simplify" :dir :system)
(local (include-book "centaur/bitops/ihsext-basics" :dir :System))

;; ----------------------------------------------------------------------------------
;; FGL FTY automatic rules for various types

(fty::add/remove-fgl-rules-for-fty-sum eval_result)


(defmacro check-might-be-true (free-var x)
  `(let ((config (fgl::make-fgl-ipasir-config)))
     (not (fgl::check-true ,free-var (not (fgl::fgl-sat-check config ,x))))))

(local (in-theory (enable fgl::if! fgl::fgl-sat-check fgl::check-true)))
(fty::def-fgl-fty-sum-splitter eval_result-split eval_result
  :prod-tree (:ev_normal :ev_throwing . :ev_error)
  :field-merges ((:ev_error data fgl::if!)
                 (:ev_error backtrace fgl::if!)
                 (:ev_throwing backtrace fgl::if!))
  :check-might-be-true check-might-be-true
  )

(fgl::def-fgl-rewrite IF-FOR-EVAL_RESULT-SPLIT-OF-EVAL_RESULT-SPLIT-redef
  (EQUAL
   (IF-FOR-EVAL_RESULT-SPLIT
    TEST
    (EVAL_RESULT-SPLIT IS-EV_NORMAL-1
                       RES-1 IS-EV_THROWING-1 THROWDATA-1 ENV-1
                       BACKTRACE-1 DESC-1 DATA-1 BACKTRACE0-1)
    (EVAL_RESULT-SPLIT IS-EV_NORMAL-2
                       RES-2 IS-EV_THROWING-2 THROWDATA-2 ENV-2
                       BACKTRACE-2 DESC-2 DATA-2 BACKTRACE0-2))
   (b* ((normalp (and (if test is-ev_normal-1 is-ev_normal-2) t))
        (throwingp (and (fgl::conditionalize _throwingp (not normalp)
                                        (if test is-ev_throwing-1 is-ev_throwing-2))
                        t))
        (throwingp! (and (not normalp) throwingp))
        (errorp! (and (not normalp) (not throwingp))))
     (eval_result-split
      normalp
      (fgl::conditionalize _res normalp (if test res-1 res-2))
      throwingp
      (fgl::conditionalize _throwdata throwingp! (if test throwdata-1 throwdata-2))
      (fgl::conditionalize _env throwingp! (if test env-1 env-2))
      (fgl::conditionalize _backtrace throwingp! (fgl::if! test backtrace-1 backtrace-2))
      (fgl::conditionalize _desc errorp! (if test desc-1 desc-2))
      (fgl::conditionalize _data errorp! (fgl::if! test data-1 data-2))
      (fgl::conditionalize _backtrace0 errorp! (fgl::if! test backtrace0-1 backtrace0-2)))))
  :hints(("Goal" :in-theory (enable if-for-eval_result-split
                                    eval_result-split
                                    fgl::if!))))

(fgl::disable-if-merge-args ev_throwing->throwdata$inline)
(fgl::disable-if-merge-args ev_throwing->env$inline)
(fgl::disable-if-merge-args ev_error->desc$inline)

(fgl::remove-fgl-rewrite IF-FOR-EVAL_RESULT-SPLIT-OF-EVAL_RESULT-SPLIT-redef)
(fgl::add-fgl-rewrite IF-FOR-EVAL_RESULT-SPLIT-OF-EVAL_RESULT-SPLIT)

#|
(fgl::remove-fgl-rewrite IF-FOR-EVAL_RESULT-SPLIT-OF-EVAL_RESULT-SPLIT)
(fgl::add-fgl-rewrite  IF-FOR-EVAL_RESULT-SPLIT-OF-EVAL_RESULT-SPLIT-redef)

|#


(fty::add/remove-fgl-rules-for-fty-sum control_flow_state)
(fty::def-fgl-fty-sum-splitter control_flow_state-split control_flow_state
  :check-might-be-true check-might-be-true
  )

(fty::add/remove-fgl-rules-for-fty-sum static_env_global)
(fty::add/remove-fgl-rules-for-fty-sum global-env)
(fty::add/remove-fgl-rules-for-fty-sum local-env)
(fty::add/remove-fgl-rules-for-fty-sum env)

(fty::add/remove-fgl-rules-for-fty-sum val)
(fgl::add-fgl-rewrite integerp-of-v_bitvector->val)
(fgl::add-fgl-rewrite integerp-of-v_int->val)


(fgl::def-fgl-branch-merge merge-unrewritten-envs
  (implies (syntaxp (not (equal env env2)))
           (equal (if test (val-imap-put-pairs pairs (global-env->storage env)) (global-env->storage env2))
                  (fgl::if! test (fgl::fgl-hide (val-imap-put-pairs pairs (global-env->storage env)))
                            (fgl::fgl-hide (global-env->storage env2)))))
  :hints(("Goal" :in-theory (enable fgl::if!))))


(encapsulate nil
  (local (include-book "centaur/bitops/ihsext-basics" :dir :system))
  (local (in-theory (disable integer-listp true-listp)))
  (fty::def-fgl-cons-rules-for-fty-sum val))
;; Note: We could add a splitter function for val, but I think since ASL is
;; supposed to be so strictly typed it shouldn't be necessary.
;; (Have tried this -- see commented code just above -- but it slows things down quite a lot)

(fty::add/remove-fgl-rules-for-fty-sum func_result)
(fty::add/remove-fgl-rules-for-fty-sum throwdata)
(fty::add/remove-fgl-rules-for-fty-sum maybe-throwdata)

(fgl::add-fgl-rewrite MAYBE-THROWDATA-P-OF-EV_THROWING->THROWDATA)
(fgl::add-fgl-rewrite stringp-of-ev_error->desc)

(fgl::add-fgl-rewrite maybe-throwdata-p-when-throwdata-p)

(fgl::def-fgl-branch-merge merge-throwdata
  (equal (if test (throwdata val ty) y)
         (fgl::if! test (fgl::fgl-hide (throwdata val ty)) y))
  :hints(("Goal" :in-theory (enable fgl::if!))))


(fgl::def-fgl-branch-merge merge-ev_error->desc
  (equal (if test (ev_error->desc err) else)
         (fgl::if! test (fgl::fgl-hide (ev_error->desc err)) else))
  :hints(("Goal" :in-theory (enable fgl::if!))))

(fgl::def-fgl-branch-merge merge-existing-if-then-else
  (equal (if test (if test1 then1 else1) else)
         (fgl::if! test (fgl::if! test1 then1 else1) else))
  :hints(("Goal" :in-theory (enable fgl::if!))))



(fgl::def-fgl-branch-merge merge-global-env
  (equal (if test (global-env static storage stack_size) x)
         (if (global-env-p x)
             (global-env (if test static (global-env->static x))
                         (if test storage (global-env->storage x))
                         (if test stack_size (global-env->stack_size x)))
           (fgl::fgl-prog2 (fgl::fgl-error! :msg "Merging global env with something that's not!"
                                            :debug-obj (list test (global-env static storage stack_size) x))
                           (if test (global-env static storage stack_size) x)))))

(fgl::def-fgl-branch-merge merge-local-env
  (equal (if test (local-env storage scope unroll declared) x)
         (if (local-env-p x)
             (local-env (if test storage (local-env->storage x))
                        (if test scope (local-env->scope x))
                        (if test unroll (local-env->unroll x))
                        (if test declared (local-env->declared x)))
           (fgl::fgl-prog2 (fgl::fgl-error! :msg "Merging local env with something that's not!"
                                            :debug-obj (list test (local-env storage scope unroll declared) x))
                           (if test (local-env storage scope unroll declared) x)))))

(fgl::enable-split-ifs stringp)



;; ----------------------------------------------------------------------------------
;; Merge rules for val kinds
;; We don't define an automatic splitter/merge rule for vals, because we don't
;; expect different kinds of vals to get merged with each other due to type
;; safety. But we still need rules for merging the individual kinds with
;; themselves.
(fgl::def-fgl-branch-merge v_bitvector-self-merge
  (implies (and (or test (val-p y))
                (or test (equal (val-kind y) :v_bitvector)))
           (equal (if test (v_bitvector len val) y)
                  (v_bitvector (if test len (v_bitvector->len y))
                               (if test val (v_bitvector->val y))))))

(fgl::def-fgl-branch-merge func_result-self-merge
  (implies (and (or test (func_result-p y)))
           (equal (if test (func_result vals env) y)
                  (func_result (if test vals (func_result->vals y))
                               (if test env (func_result->env y))))))


;; (fgl::def-fgl-branch-merge v_record-self-merge
;;   (implies (and (or test (val-p y))
;;                 (or test (equal (val-kind y) :v_record)))
;;            (equal (if test (v_record rec) y)
;;                   (v_record (if test rec (v_record->rec y))))))

;; Note: v_record-self-merge (above) is undesirable because we don't want to
;; deal in v_record->recs of lazy object forms (ty-fix-val) -- see the comment
;; atop type-resolution.lisp.  Instead we add rules for merging explicit
;; constructors and constants. Maybe replace v_array-self-merge below as well?
(fgl::def-fgl-branch-merge v_record-constructor-merge
  (equal (if test (v_record x) (v_record y))
         (v_record (if test x y))))

(fgl::def-fgl-branch-merge v_record-constructor/constant-merge
  (implies (and (syntaxp (fgl::fgl-object-case y :g-concrete))
                (val-p y)
                (equal (val-kind y) :v_record))
           (equal (if test (v_record x) y)
                  (v_record (if test x (v_record->rec y))))))

(fgl::def-fgl-branch-merge v_array-self-merge
  (implies (and (or test (val-p y))
                (or test (equal (val-kind y) :v_array)))
           (equal (if test (v_array arr) y)
                  (v_array (if test arr (v_array->arr y))))))


(fgl::def-fgl-branch-merge v_bool-self-merge
  (implies (and (or test (val-p y))
                (or test (equal (val-kind y) :v_bool)))
           (equal (if test (v_bool xval) y)
                  (v_bool (if test xval (v_bool->val y))))))

(fgl::def-fgl-branch-merge v_int-self-merge
  (implies (and (or test (val-p y))
                (or test (equal (val-kind y) :v_int)))
           (equal (if test (v_int xval) y)
                  (v_int (if test xval (v_int->val y))))))


(fgl::def-fgl-branch-merge merge-v_bitvector-cons
  (implies (and (val-p (cons :v_bitvector y))
                (val-p z)
                (val-case z :v_bitvector))
           (equal (if test (cons :v_bitvector y) z)
                  (v_bitvector (if test (v_bitvector->len (cons :v_bitvector y))
                                 (v_bitvector->len z))
                               (if test
                                   (v_bitvector->val (cons :v_bitvector y))
                                 (v_bitvector->val z)))))
  :hints(("Goal" :in-theory (enable val-kind))))

(fgl::def-fgl-branch-merge v_label-self-merge
  (implies (and (or test (val-p y))
                (or test (equal (val-kind y) :v_label)))
           (equal (if test (v_label xval) y)
                  (v_label (choose-value-if test xval (v_label->val y)))))
  :hints(("Goal" :in-theory (enable choose-value-if))))




(fgl::def-ctrex-rule v_bitvector->val-ctrex-rule
  :match ((val (v_bitvector->val x))
          ;; (len (v_bitvector->len x))
          )
  :assign (v_bitvector (+ 2 (integer-length val)) val)
  :assigned-var x
  :ruletype :elim)




(fgl::def-ctrex-rule v_record->rec-ctrex-rule
  :match ((rec (v_record->rec x)))
  :assign (v_record rec)
  :assigned-var x
  :ruletype :elim)

(fgl::def-ctrex-rule v_bool->val-ctrex-rule
  :match ((val (v_bool->val x)))
  :assign (v_bool val)
  :assigned-var x
  :ruletype :elim)

(fgl::def-ctrex-rule v_int->val-ctrex-rule
  :match ((val (v_int->val x)))
  :assign (v_int val)
  :assigned-var x
  :ruletype :elim)

(fgl::def-ctrex-rule v_real->val-ctrex-rule
  :match ((val (v_real->val x)))
  :assign (v_real val)
  :assigned-var x
  :ruletype :elim)

(fgl::def-ctrex-rule v_string->val-ctrex-rule
  :match ((val (v_string->val x)))
  :assign (v_string val)
  :assigned-var x
  :ruletype :elim)



;; (fgl::def-ctrex-rule hide-v_label->val-ctrex-rule
;;   :match ((val (hide-v_label->val x)))
;;   :assign (v_label val)
;;   :assigned-var x
;;   :ruletype :elim)

(fgl::def-ctrex-rule v_label->val-ctrex-rule
  :match ((val (v_label->val x)))
  :assign (v_label val)
  :assigned-var x
  :ruletype :elim)



(fgl::def-ctrex-rule global-env-ctrex-rule
  :match ((static (global-env->static x))
          (storage (global-env->storage x))
          (stack_size (global-env->stack_size x)))
  :assigned-var x
  :assign (global-env static storage stack_size)
  :ruletype :elim)

(fgl::def-ctrex-rule env-ctrex-rule
  :match ((global (env->global x))
          (local (env->local x)))
  :assigned-var x
  :assign (env global local)
  :ruletype :elim)





(fgl::def-fgl-rewrite termination-error-p-of-eval_result-split
  (equal (termination-error-p
          (eval_result-split is-normal res is-throwing throwdata env backtrace1
                             desc data backtrace2))
         (and (not is-normal) (not is-throwing)
              (termination-errmsg-p desc)))
  :hints(("Goal" :in-theory (enable termination-error-p eval_result-split
                                    termination-errmsg-p))))


(fgl::enable-split-ifs val-imap-p)
(fgl::add-fgl-rewrite env-p-of-ev_throwing->env)
(fgl::enable-split-ifs maybe-throwdata-p)




(fgl::add-fgl-rewrite pos-imap-p-of-global-env->stack_size)
(fgl::add-fgl-rewrite pos-imap-fix-when-pos-imap-p)



(fgl::def-fgl-rewrite stringp-of-termination-errmsg-fix
  (stringp (termination-errmsg-fix x))
  :hints(("Goal" :in-theory (enable termination-errmsg-fix
                                    some-value))))

(fgl::def-fgl-branch-merge merge-termination-errmsg-fix-with-other
  (equal (if test (termination-errmsg-fix x) y)
         (fgl::if! test (fgl::fgl-hide (termination-errmsg-fix x)) y))
  :hints(("Goal" :in-theory (enable fgl::if!))))

(define v_bool-val-hyp (x)
  (and (val-p x)
       (val-case x :v_bool))
  ///
  (fgl::remove-fgl-rewrite v_bool-val-hyp)

  (fgl::def-fgl-rewrite v_bool-val-hyp-rw
    (equal (v_bool-val-hyp x)
           (let ((bool (and (v_bool->val (fgl::fgl-hide x)) t)))
             (and (fgl::left-to-right (fgl::fgl-hide (equal x (v_bool bool)))) t))))

  (fgl::def-fgl-rewrite v_bool-val-hyp-relieve
    (implies (and (val-p x)
                  (val-case x :v_bool))
             (v_bool-val-hyp x))))
