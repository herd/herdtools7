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

(include-book "trace-free")
(local (include-book "std/lists/sets" :dir :system))
(local (std::add-default-post-define-hook :fix))

(define call-siglist-find-tracespecs ((x call-siglist-p)
                                  (tracespec tracespec-p))
  :returns (specs call-tracespeclist-p)
  (b* (((when (atom x)) nil)
       ((call-sig x1) (car x))
       (look (find-call-tracespec x1.fn x1.pos tracespec)))
    (if look
        (cons look (call-siglist-find-tracespecs (cdr x) tracespec))
      (call-siglist-find-tracespecs (cdr x) tracespec)))
  ///
  (defthm call-siglist-find-tracespecs-of-call-siglist-filter-traced
    (equal (call-siglist-find-tracespecs (call-siglist-filter-traced x tracespec) tracespec)
           (call-siglist-find-tracespecs x tracespec))
    :hints(("Goal" :in-theory (enable call-siglist-filter-traced))))

  (defthm call-siglist-find-tracespecs-of-nil
    (equal (call-siglist-find-tracespecs nil tracespec) nil))

  (local (defret member-when-member
           (implies (and (member c x)
                         (find-call-tracespec (call-sig->fn c) (call-sig->pos c) tracespec))
                    (member (find-call-tracespec (call-sig->fn c) (call-sig->pos c) tracespec)
                            specs))))

  (local (defret subset-when-subset
           (implies (subsetp y x)
                    (subsetp (call-siglist-find-tracespecs y tracespec)
                             specs))))

  (defcong acl2::set-equiv acl2::set-equiv (call-siglist-find-tracespecs x tracespec) 1
    :hints(("Goal" :in-theory (enable acl2::set-equiv))))

  (defthm call-siglist-find-tracespecs-of-append
    (equal (call-siglist-find-tracespecs (append x y) tracespec)
           (append (call-siglist-find-tracespecs x tracespec)
                   (call-siglist-find-tracespecs y tracespec))))

  (defthm call-siglist-find-tracespecs-of-cons
    (equal (call-siglist-find-tracespecs (cons x y) tracespec)
           (let* ((first (find-call-tracespec (call-sig->fn x) (call-sig->pos x) tracespec))
                  (rest (call-siglist-find-tracespecs y tracespec)))
             (if first
                 (cons first rest)
               rest)))))

(define stmtlist-find-tracespecs ((x stmtlist-p)
                               (tracespec tracespec-p))
  :returns (specs stmt-tracespeclist-p)
  (b* (((when (atom x)) nil)
       (look (find-stmt-tracespec (car x) tracespec)))
    (if look
        (cons look (stmtlist-find-tracespecs (cdr x) tracespec))
      (stmtlist-find-tracespecs (cdr x) tracespec)))
  ///
  (defthm stmtlist-find-tracespecs-of-stmtlist-filter-traced
    (equal (stmtlist-find-tracespecs (stmtlist-filter-traced x tracespec) tracespec)
           (stmtlist-find-tracespecs x tracespec))
    :hints(("Goal" :in-theory (enable stmtlist-filter-traced))))

  (defthm stmtlist-find-tracespecs-of-nil
    (equal (stmtlist-find-tracespecs nil tracespec) nil))

  
  (local (defret member-when-member
           (implies (and (member s x)
                         (find-stmt-tracespec s tracespec))
                    (member (find-stmt-tracespec s tracespec)
                            specs))))

  (local (defret subset-when-subset
           (implies (subsetp y x)
                    (subsetp (stmtlist-find-tracespecs y tracespec)
                             specs))))

  (defcong acl2::set-equiv acl2::set-equiv (stmtlist-find-tracespecs x tracespec) 1
    :hints(("Goal" :in-theory (enable acl2::set-equiv))))

  (defthm stmtlist-find-tracespecs-of-append
    (equal (stmtlist-find-tracespecs (append x y) tracespec)
           (append (stmtlist-find-tracespecs x tracespec)
                   (stmtlist-find-tracespecs y tracespec))))

  (defthm stmtlist-find-tracespecs-of-cons
    (equal (stmtlist-find-tracespecs (cons x y) tracespec)
           (let* ((first (find-stmt-tracespec x tracespec))
                  (rest (stmtlist-find-tracespecs y tracespec)))
             (if first
                 (cons first rest)
               rest)))))

(defines tracelist-subsetp
  (define tracelist-subsetp ((x asl-tracelist-p) (y asl-tracelist-p))
    :measure (+ (asl-tracelist-count x)
                (asl-tracelist-count y))
    :returns (subsetp)
    (if (atom x)
        t
      (and (find-trace-subset (car x) y)
           (tracelist-subsetp (cdr x) y))))

  (define find-trace-subset ((x asl-trace-p)
                             (y asl-tracelist-p))
    :measure (+ (asl-trace-count x)
                (asl-tracelist-count y))
    :returns (subsetp)
    (b* (((when (atom y)) nil))
      (or (trace-subsetp x (car y))
          (find-trace-subset x (asl-trace->subtraces (car y)))
          (find-trace-subset x (cdr y)))))

  (define trace-subsetp ((x asl-trace-p)
                         (y asl-trace-p))
    :measure (+ (asl-trace-count x)
                (asl-trace-count y))
    :returns (subsetp)
    (asl-trace-case x
      :calltrace (asl-trace-case y
                   :calltrace
                   (and (eq x.name y.name)
                        (equal x.fn y.fn)
                        (equal x.params y.params)
                        (equal x.args y.args)
                        (tracelist-subsetp x.subtraces y.subtraces)
                        (equal x.result y.result)
                        (equal x.pos y.pos))
                   :stmttrace nil)
      :stmttrace (asl-trace-case y
                   :stmttrace
                   (and (eq x.name y.name)
                        (equal x.stmt y.stmt)
                        (equal x.initial-vars y.initial-vars)
                        (tracelist-subsetp x.subtraces y.subtraces)
                        (equal x.result y.result)
                        (equal x.final-vars y.final-vars))
                   :calltrace nil)))
  ///
  (fty::deffixequiv-mutual tracelist-subsetp)

  (std::defret-mutual <fn>-when-subset
    (defret <fn>-when-subset
      (implies (subsetp x y)
               subsetp)
      :hints ('(:expand (<call>)))
      :fn tracelist-subsetp)

    (defret <fn>-when-member
      (implies (member-equal x y)
               subsetp)
      :hints ('(:expand (<call>)))
      :fn find-trace-subset)

    (defret <fn>-when-equiv
      (implies (asl-trace-equiv x y)
               subsetp)
      :hints ('(:expand (<call>)))
      :fn trace-subsetp)))

  
(define interp-trace-subsetp (interp1 interp2)
  :non-executable t
  :verify-guards nil
  (b* (((mv result1 & trace1) interp1)
       ((mv result2 & trace2) interp2))
    (and (implies (equal (ev_error->desc result1) "Trace abort")
                  (equal (ev_error->desc result2) "Trace abort"))
         (tracelist-subsetp trace1 trace2))))

(define interp-result-trace-subsetp ((res1 eval_result-p)
                                     (trace1 asl-tracelist-p)
                                     (res2 eval_result-p)
                                     (trace2 asl-tracelist-p))
  :non-executable t
  :verify-guards nil
  (or (equal (ev_error->desc res2) "Trace abort")
      (and (equal (eval_result-fix res1)
                  (eval_result-fix res2))
           (tracelist-subsetp trace1 trace2))))


;; (DEFINE CALL-SIG-TRACESPEC-SUBSETP
;;                ((X CALL-SIG-P)
;;                 &KEY
;;                 ((TRACESPEC TRACESPEC-P) 'TRACESPEC)
;;                 ((TRACESPEC2 TRACESPEC-P) 'TRACESPEC2))
;;          (B* (((CALL-SIG X))
;;               (LOOK1 (FIND-CALL-TRACESPEC X.FN X.POS TRACESPEC)))
;;            (OR (NOT LOOK1)
;;                (EQUAL LOOK1
;;                       (FIND-CALL-TRACESPEC X.FN X.POS TRACESPEC2))))
;;          ///
;;          (DEFTHM CALL-SIG-TRACESPEC-SUBSETP-WHEN-NOT-FOUND
;;            (IMPLIES (NOT (FIND-CALL-TRACESPEC (CALL-SIG->FN X)
;;                                               (CALL-SIG->POS X)
;;                                               TRACESPEC))
;;                     (CALL-SIG-TRACESPEC-SUBSETP X))))


(local (defthm tracespec-count-of-maybe-tracespec
         (implies x
                  (< (tracespec-count x) (maybe-tracespec-count x)))
         :hints (("goal" :expand ((maybe-tracespec-count x))
                  :in-theory (enable maybe-tracespec-some->val)))
         :rule-classes ((:linear :trigger-terms ((tracespec-count x))))))



(defthm call-tracespeclist-find-of-append
  (equal (call-tracespeclist-find fn pos (append x y))
         (or (call-tracespeclist-find fn pos x)
             (call-tracespeclist-find fn pos y)))
  :hints(("Goal" :in-theory (enable call-tracespeclist-find))))

(defthm stmt-tracespeclist-find-of-append
  (equal (stmt-tracespeclist-find s (append x y))
         (or (stmt-tracespeclist-find s x)
             (stmt-tracespeclist-find s y)))
  :hints(("Goal" :in-theory (enable stmt-tracespeclist-find))))

(define union-tracespecs ((x tracespec-p) (y tracespec-p))
  :returns (comb tracespec-p)
  (b* (((tracespec x))
       ((tracespec y)))
    (make-tracespec
     :stmt-specs-permanent (append x.stmt-specs-permanent y.stmt-specs-permanent)
     :call-specs-permanent (append x.call-specs-permanent y.call-specs-permanent)
     :stmt-specs-transient (append x.stmt-specs-transient y.stmt-specs-transient)
     :call-specs-transient (append x.call-specs-transient y.call-specs-transient)))
  ///
  (defret find-call-tracespec-of-<fn>-under-iff
    (iff (find-call-tracespec fn pos comb)
         (or (find-call-tracespec fn pos x)
             (find-call-tracespec fn pos y)))
    :hints(("Goal" :in-theory (enable find-call-tracespec))))

  (defret find-stmt-tracespec-of-<fn>-under-iff
    (iff (find-stmt-tracespec s comb)
         (or (find-stmt-tracespec s x)
             (find-stmt-tracespec s y)))
    :hints(("Goal" :in-theory (enable find-stmt-tracespec)))))



(local (defthm check-call-tracespec-lemma
         (implies (check-call-tracespec fn pos x)
                  (equal (check-call-tracespec fn pos x)
                         (call-tracespec-fix x)))
         :hints(("Goal" :in-theory (enable check-call-tracespec)))))



(defthm maybe-call-tracespec->interior-tracespec-when-no-interior-tracespec
  (implies (not (call-tracespec->interior-tracespec x))
           (not (maybe-call-tracespec->interior-tracespec x)))
  :hints(("Goal" :in-theory (enable maybe-call-tracespec->interior-tracespec))))

(defthm maybe-stmt-tracespec->interior-tracespec-when-no-interior-tracespec
  (implies (not (stmt-tracespec->interior-tracespec x))
           (not (maybe-stmt-tracespec->interior-tracespec x)))
  :hints(("Goal" :in-theory (enable maybe-stmt-tracespec->interior-tracespec))))

(defthm maybe-call-tracespec->empty-tracespec-when-not
  (implies (not (call-tracespec->empty-tracespec x))
           (not (maybe-call-tracespec->empty-tracespec x)))
  :hints(("Goal" :in-theory (enable maybe-call-tracespec->empty-tracespec))))

(defthm maybe-stmt-tracespec->empty-tracespec-when-not
  (implies (not (stmt-tracespec->empty-tracespec x))
           (not (maybe-stmt-tracespec->empty-tracespec x)))
  :hints(("Goal" :in-theory (enable maybe-stmt-tracespec->empty-tracespec))))




(define call-sig-tracespec-subsetp ((x call-sig-p)
                                    &key
                                    ((tracespec tracespec-p) 'tracespec)
                                    ((tracespec2 tracespec-p) 'tracespec2))
  (b* (((call-sig x))
       ((tracespec tracespec))
       ((tracespec tracespec2))
       (trans-look1 (call-tracespeclist-find x.fn x.pos tracespec.call-specs-transient))
       (trans-look2 (call-tracespeclist-find x.fn x.pos tracespec2.call-specs-transient))
       (perm-look1 (call-tracespeclist-find x.fn x.pos tracespec.call-specs-permanent))
       (perm-look2 (call-tracespeclist-find x.fn x.pos tracespec2.call-specs-permanent)))
    (and (cond (trans-look1 (equal trans-look1 trans-look2))
               (trans-look2 (b* (((call-tracespec trans-look2)))
                              (and (not perm-look1)
                                   (not trans-look2.empty-tracespec)
                                   (not trans-look2.interior-tracespec)))) ;; *y
               (t t))
         (cond (perm-look1 (equal perm-look1 perm-look2)) ;; *y
               (perm-look2 (b* (((call-tracespec perm-look2)))
                              (and (not perm-look2.empty-tracespec)
                                   (not perm-look2.interior-tracespec))))
               (t t))))
  ///
  (defthm call-sig-tracespec-subsetp-when-not-found
    (implies (and (not (find-call-tracespec (call-sig->fn x) (call-sig->pos x) tracespec))
                  (not (find-call-tracespec (call-sig->fn x) (call-sig->pos x) tracespec2)))
             (call-sig-tracespec-subsetp x))
    :hints(("Goal" :in-theory (enable find-call-tracespec))))

  (defthm call-sig-tracespec-subsetp-norm-1
    (implies (syntaxp (not (and (equal sts1 ''nil)
                                (equal sts2 ''nil))))
             (equal (call-sig-tracespec-subsetp x :tracespec (tracespec sts1 cls1 sts2 cls2))
                    (call-sig-tracespec-subsetp x :tracespec (tracespec nil cls1 nil cls2)))))

  (defthm call-sig-tracespec-subsetp-norm-2
    (implies (syntaxp (not (and (equal sts1 ''nil)
                                (equal sts2 ''nil))))
             (equal (call-sig-tracespec-subsetp x :tracespec2 (tracespec sts1 cls1 sts2 cls2))
                    (call-sig-tracespec-subsetp x :tracespec2 (tracespec nil cls1 nil cls2)))))

  (defthm call-sig-tracespec-subsetp-norm-1-acc
    (equal (call-sig-tracespec-subsetp x :tracespec (tracespec sts1
                                                               (tracespec->call-specs-permanent tracespec)
                                                               sts2
                                                               (tracespec->call-specs-transient tracespec)))
           (call-sig-tracespec-subsetp x)))

  (defthm call-sig-tracespec-subsetp-norm-2-acc
    (equal (call-sig-tracespec-subsetp x :tracespec2 (tracespec sts1
                                                                (tracespec->call-specs-permanent tracespec2)
                                                                sts2
                                                                (tracespec->call-specs-transient tracespec2)))
           (call-sig-tracespec-subsetp x)))

  (defthm call-sig-tracespec-subsetp-of-call-interior-tracespec
    (implies (and (call-sig-tracespec-subsetp x)
                  (call-sig-tracespec-subsetp y))
             (b* (((call-sig y))
                  (entry1 (find-call-tracespec y.fn y.pos tracespec))
                  (entry2 (find-call-tracespec y.fn y.pos tracespec2))
                  (tracespec (call-interior-tracespec entry1 tracespec))
                  (tracespec2 (call-interior-tracespec entry2 tracespec2)))
               (call-sig-tracespec-subsetp x)))
    :hints(("Goal" :in-theory (enable find-call-tracespec
                                      combine-tracespecs
                                      call-interior-tracespec))))

  (defthm call-sig-tracespec-subsetp-of-call-interior-tracespec-nil
    (implies (call-sig-tracespec-subsetp x)
             (b* ((tracespec (call-interior-tracespec nil tracespec))
                  (tracespec2 (call-interior-tracespec nil tracespec2)))
               (call-sig-tracespec-subsetp x)))
    :hints(("Goal" :in-theory (enable combine-tracespecs
                                      call-interior-tracespec))))

  (defthm call-sig-tracespec-subsetp-of-stmt-interior-tracespec-nil
    (implies (call-sig-tracespec-subsetp x)
             (b* ((tracespec (stmt-interior-tracespec nil tracespec))
                  (tracespec2 (stmt-interior-tracespec nil tracespec2)))
               (call-sig-tracespec-subsetp x)))
    :hints(("Goal" :in-theory (enable combine-tracespecs
                                      stmt-interior-tracespec)))))

(define call-siglist-tracespec-subsetp ((x call-siglist-p)
                                        &key
                                        ((tracespec tracespec-p) 'tracespec)
                                        ((tracespec2 tracespec-p) 'tracespec2))
  (b* (((when (atom x)) t)
       ((call-sig x1) (car x)))
    (and (call-sig-tracespec-subsetp (car x))
         (call-siglist-tracespec-subsetp (cdr x))))
  ///
  (defthm call-siglist-tracespec-subsetp-of-append
    (iff (call-siglist-tracespec-subsetp (append x y))
         (and (call-siglist-tracespec-subsetp x)
              (call-siglist-tracespec-subsetp y))))

  (defthm call-siglist-tracespec-subsetp-of-nil
    (equal (call-siglist-tracespec-subsetp nil) t))

  (defthm call-siglist-tracespec-subsetp-of-cons
    (iff (call-siglist-tracespec-subsetp (cons x y))
         (and (call-sig-tracespec-subsetp x)
              (call-siglist-tracespec-subsetp y))))

  (local
   (defthm call-siglist-tracespec-subsetp-implies-member
     (implies (and (call-siglist-tracespec-subsetp x)
                   (member-equal c x))
              (call-sig-tracespec-subsetp c))))
  (defthm call-siglist-tracespec-subsetp-when-subsetp
    (implies (and (call-siglist-tracespec-subsetp x)
                  (subsetp y x))
             (call-siglist-tracespec-subsetp y))
    :hints(("Goal" :in-theory (enable subsetp))))

  (defcong acl2::set-equiv equal (call-siglist-tracespec-subsetp x) 1
    :hints(("Goal" :in-theory (enable acl2::set-equiv)
            :cases ((call-siglist-tracespec-subsetp x)))))

  (defthm call-siglist-tracespec-subsetp-of-call-siglist-filter-traced
    (iff (call-siglist-tracespec-subsetp (call-siglist-filter-traced x (union-tracespecs tracespec tracespec2)))
         (call-siglist-tracespec-subsetp x))
    :hints(("Goal" :in-theory (enable call-siglist-filter-traced))))

  

  (defthm call-siglist-tracespec-subsetp-norm-1
    (implies (syntaxp (not (and (equal sts1 ''nil)
                                (equal sts2 ''nil))))
             (equal (call-siglist-tracespec-subsetp x :tracespec (tracespec sts1 cls1 sts2 cls2))
                    (call-siglist-tracespec-subsetp x :tracespec (tracespec nil cls1 nil cls2)))))

  (defthm call-siglist-tracespec-subsetp-norm-2
    (implies (syntaxp (not (and (equal sts1 ''nil)
                                (equal sts2 ''nil))))
             (equal (call-siglist-tracespec-subsetp x :tracespec2 (tracespec sts1 cls1 sts2 cls2))
                    (call-siglist-tracespec-subsetp x :tracespec2 (tracespec nil cls1 nil cls2)))))

  (defthm call-siglist-tracespec-subsetp-norm-1-acc
    (equal (call-siglist-tracespec-subsetp x :tracespec (tracespec sts1
                                                                   (tracespec->call-specs-permanent tracespec)
                                                                   sts2
                                                                   (tracespec->call-specs-transient tracespec)))
           (call-siglist-tracespec-subsetp x)))

  (defthm call-siglist-tracespec-subsetp-norm-2-acc
    (equal (call-siglist-tracespec-subsetp x :tracespec2 (tracespec sts1
                                                                    (tracespec->call-specs-permanent tracespec2)
                                                                    sts2
                                                                    (tracespec->call-specs-transient tracespec2)))
           (call-siglist-tracespec-subsetp x)))

  

  (defthm call-siglist-tracespec-subsetp-of-call-interior-tracespec
    (implies (and (call-siglist-tracespec-subsetp x)
                  (call-sig-tracespec-subsetp y))
             (b* (((call-sig y))
                  (entry1 (find-call-tracespec y.fn y.pos tracespec))
                  (entry2 (find-call-tracespec y.fn y.pos tracespec2))
                  (tracespec (call-interior-tracespec entry1 tracespec))
                  (tracespec2 (call-interior-tracespec entry2 tracespec2)))
               (call-siglist-tracespec-subsetp x))))

  (defthm call-siglist-tracespec-subsetp-of-call-interior-tracespec-nil
    (implies (call-siglist-tracespec-subsetp x)
             (b* ((tracespec (call-interior-tracespec nil tracespec))
                  (tracespec2 (call-interior-tracespec nil tracespec2)))
               (call-siglist-tracespec-subsetp x))))

  (defthm call-siglist-tracespec-subsetp-of-stmt-interior-tracespec-nil
    (implies (call-siglist-tracespec-subsetp x)
             (b* ((tracespec (stmt-interior-tracespec nil tracespec))
                  (tracespec2 (stmt-interior-tracespec nil tracespec2)))
               (call-siglist-tracespec-subsetp x)))))


;; (define stmt-tracespec-subsetp ((x stmt-p)
;;                                     &key
;;                                     ((tracespec tracespec-p) 'tracespec)
;;                                     ((tracespec2 tracespec-p) 'tracespec2))
;;   (b* ((look1 (find-stmt-tracespec x tracespec)))
;;     (or (not look1)
;;         (equal look1
;;                (find-stmt-tracespec x tracespec2))))
;;   ///
;;   (defthm stmt-tracespec-subsetp-when-not-found
;;     (implies (not (find-stmt-tracespec x tracespec))
;;              (stmt-tracespec-subsetp x))))

(define stmt-tracespec-subsetp ((x stmt-p)
                                    &key
                                    ((tracespec tracespec-p) 'tracespec)
                                    ((tracespec2 tracespec-p) 'tracespec2))
  (b* (((tracespec tracespec))
       ((tracespec tracespec2))
       (trans-look1 (stmt-tracespeclist-find x tracespec.stmt-specs-transient))
       (trans-look2 (stmt-tracespeclist-find x tracespec2.stmt-specs-transient))
       (perm-look1 (stmt-tracespeclist-find x tracespec.stmt-specs-permanent))
       (perm-look2 (stmt-tracespeclist-find x tracespec2.stmt-specs-permanent)))
    (and (cond (trans-look1 (equal trans-look1 trans-look2))
               (trans-look2 (b* (((stmt-tracespec trans-look2)))
                              (and (not perm-look1)
                                   (not trans-look2.empty-tracespec)
                                   (not trans-look2.interior-tracespec)))) ;; *y
               (t t))
         (cond (perm-look1 (equal perm-look1 perm-look2)) ;; *y
               (perm-look2 (b* (((stmt-tracespec perm-look2)))
                              (and (not perm-look2.empty-tracespec)
                                   (not perm-look2.interior-tracespec))))
               (t t))))
  ///
  (defthm stmt-tracespec-subsetp-when-not-found
    (implies (and (not (find-stmt-tracespec x tracespec))
                  (not (find-stmt-tracespec x tracespec2)))
             (stmt-tracespec-subsetp x))
    :hints(("Goal" :in-theory (enable find-stmt-tracespec))))

  (defthm stmt-tracespec-subsetp-norm-1
    (implies (syntaxp (not (and (equal cls1 ''nil)
                                (equal cls2 ''nil))))
             (equal (stmt-tracespec-subsetp x :tracespec (tracespec sts1 cls1 sts2 cls2))
                    (stmt-tracespec-subsetp x :tracespec (tracespec sts1 nil sts2 nil)))))

  (defthm stmt-tracespec-subsetp-norm-2
    (implies (syntaxp (not (and (equal cls1 ''nil)
                                (equal cls2 ''nil))))
             (equal (stmt-tracespec-subsetp x :tracespec2 (tracespec sts1 cls1 sts2 cls2))
                    (stmt-tracespec-subsetp x :tracespec2 (tracespec sts1 nil sts2 nil)))))

  (defthm stmt-tracespec-subsetp-norm-1-acc
    (equal (stmt-tracespec-subsetp x :tracespec (tracespec (tracespec->stmt-specs-permanent tracespec)
                                                           cls1
                                                           (tracespec->stmt-specs-transient tracespec)
                                                           cls2))
           (stmt-tracespec-subsetp x)))

  (defthm stmt-tracespec-subsetp-norm-2-acc
    (equal (stmt-tracespec-subsetp x :tracespec2 (tracespec (tracespec->stmt-specs-permanent tracespec2)
                                                            cls1
                                                            (tracespec->stmt-specs-transient tracespec2)
                                                            cls2))
           (stmt-tracespec-subsetp x)))

  (defthm stmt-tracespec-subsetp-of-call-interior-tracespec
    (implies (and (stmt-tracespec-subsetp x)
                  (call-sig-tracespec-subsetp y))
             (b* (((call-sig y))
                  (entry1 (find-call-tracespec y.fn y.pos tracespec))
                  (entry2 (find-call-tracespec y.fn y.pos tracespec2))
                  (tracespec (call-interior-tracespec entry1 tracespec))
                  (tracespec2 (call-interior-tracespec entry2 tracespec2)))
               (stmt-tracespec-subsetp x)))
    :hints(("Goal" :in-theory (enable find-call-tracespec
                                      combine-tracespecs
                                      call-interior-tracespec
                                      call-sig-tracespec-subsetp))))

  (defthm stmt-tracespec-subsetp-of-stmt-interior-tracespec
    (implies (and (stmt-tracespec-subsetp x)
                  (stmt-tracespec-subsetp y))
             (b* ((entry1 (find-stmt-tracespec y tracespec))
                  (entry2 (find-stmt-tracespec y tracespec2))
                  (tracespec (stmt-interior-tracespec entry1 tracespec))
                  (tracespec2 (stmt-interior-tracespec entry2 tracespec2)))
               (stmt-tracespec-subsetp x)))
    :hints(("Goal" :in-theory (enable find-stmt-tracespec
                                      combine-tracespecs
                                      stmt-interior-tracespec))))

  (defthm call-sig-tracespec-subsetp-of-stmt-interior-tracespec
    (implies (and (call-sig-tracespec-subsetp x)
                  (stmt-tracespec-subsetp y))
             (b* ((entry1 (find-stmt-tracespec y tracespec))
                  (entry2 (find-stmt-tracespec y tracespec2))
                  (tracespec (stmt-interior-tracespec entry1 tracespec))
                  (tracespec2 (stmt-interior-tracespec entry2 tracespec2)))
               (call-sig-tracespec-subsetp x)))
    :hints(("Goal" :in-theory (enable find-stmt-tracespec
                                      combine-tracespecs
                                      stmt-interior-tracespec
                                      call-sig-tracespec-subsetp))))

  (defthm call-siglist-tracespec-subsetp-of-stmt-interior-tracespec
    (implies (and (call-siglist-tracespec-subsetp x)
                  (stmt-tracespec-subsetp y))
             (b* ((entry1 (find-stmt-tracespec y tracespec))
                  (entry2 (find-stmt-tracespec y tracespec2))
                  (tracespec (stmt-interior-tracespec entry1 tracespec))
                  (tracespec2 (stmt-interior-tracespec entry2 tracespec2)))
               (call-siglist-tracespec-subsetp x)))
    :hints(("Goal" :in-theory (e/d (call-siglist-tracespec-subsetp)
                                   (stmt-tracespec-subsetp)))))

  (defthm stmt-tracespec-subsetp-of-call-interior-tracespec-nil
    (implies (stmt-tracespec-subsetp x)
             (b* ((tracespec (call-interior-tracespec nil tracespec))
                  (tracespec2 (call-interior-tracespec nil tracespec2)))
               (stmt-tracespec-subsetp x)))
    :hints(("Goal" :in-theory (enable combine-tracespecs
                                      call-interior-tracespec))))

  (defthm stmt-tracespec-subsetp-of-stmt-interior-tracespec-nil
    (implies (stmt-tracespec-subsetp x)
             (b* ((tracespec (stmt-interior-tracespec nil tracespec))
                  (tracespec2 (stmt-interior-tracespec nil tracespec2)))
               (stmt-tracespec-subsetp x)))
    :hints(("Goal" :in-theory (enable combine-tracespecs
                                      stmt-interior-tracespec)))))




(define stmtlist-tracespec-subsetp ((x stmtlist-p)
                                        &key
                                        ((tracespec tracespec-p) 'tracespec)
                                        ((tracespec2 tracespec-p) 'tracespec2))
  (b* (((when (atom x)) t))
    (and (stmt-tracespec-subsetp (car x))
         (stmtlist-tracespec-subsetp (cdr x))))
  ///
  (defthm stmtlist-tracespec-subsetp-of-append
    (iff (stmtlist-tracespec-subsetp (append x y))
         (and (stmtlist-tracespec-subsetp x)
              (stmtlist-tracespec-subsetp y))))

  (defthm stmtlist-tracespec-subsetp-of-nil
    (equal (stmtlist-tracespec-subsetp nil) t))

  (defthm stmtlist-tracespec-subsetp-of-cons
    (iff (stmtlist-tracespec-subsetp (cons x y))
         (and (stmt-tracespec-subsetp x)
              (stmtlist-tracespec-subsetp y))))

  (local
   (defthm stmtlist-tracespec-subsetp-implies-member
     (implies (and (stmtlist-tracespec-subsetp x)
                   (member-equal c x))
              (stmt-tracespec-subsetp c))))
  (defthm stmtlist-tracespec-subsetp-when-subsetp
    (implies (and (stmtlist-tracespec-subsetp x)
                  (subsetp y x))
             (stmtlist-tracespec-subsetp y))
    :hints(("Goal" :in-theory (enable subsetp))))

  (defcong acl2::set-equiv equal (stmtlist-tracespec-subsetp x) 1
    :hints(("Goal" :in-theory (enable acl2::set-equiv)
            :cases ((stmtlist-tracespec-subsetp x)))))

  (defthm stmtlist-tracespec-subsetp-of-stmtlist-filter-traced
    (iff (stmtlist-tracespec-subsetp (stmtlist-filter-traced x (union-tracespecs tracespec tracespec2)))
         (stmtlist-tracespec-subsetp x))
    :hints(("Goal" :in-theory (enable stmtlist-filter-traced))))

  (defthm stmtlist-tracespec-subsetp-norm-1
    (implies (syntaxp (not (and (equal cls1 ''nil)
                                (equal cls2 ''nil))))
             (equal (stmtlist-tracespec-subsetp x :tracespec (tracespec sts1 cls1 sts2 cls2))
                    (stmtlist-tracespec-subsetp x :tracespec (tracespec sts1 nil sts2 nil)))))

  (defthm stmtlist-tracespec-subsetp-norm-2
    (implies (syntaxp (not (and (equal cls1 ''nil)
                                (equal cls2 ''nil))))
             (equal (stmtlist-tracespec-subsetp x :tracespec2 (tracespec sts1 cls1 sts2 cls2))
                    (stmtlist-tracespec-subsetp x :tracespec2 (tracespec sts1 nil sts2 nil)))))

  (defthm stmtlist-tracespec-subsetp-norm-1-acc
    (equal (stmtlist-tracespec-subsetp x :tracespec (tracespec (tracespec->stmt-specs-permanent tracespec)
                                                           cls1
                                                           (tracespec->stmt-specs-transient tracespec)
                                                           cls2))
           (stmtlist-tracespec-subsetp x)))

  (defthm stmtlist-tracespec-subsetp-norm-2-acc
    (equal (stmtlist-tracespec-subsetp x :tracespec2 (tracespec (tracespec->stmt-specs-permanent tracespec2)
                                                            cls1
                                                            (tracespec->stmt-specs-transient tracespec2)
                                                            cls2))
           (stmtlist-tracespec-subsetp x)))


  (defthm stmtlist-tracespec-subsetp-of-call-interior-tracespec
    (implies (and (stmtlist-tracespec-subsetp x)
                  (call-sig-tracespec-subsetp y))
             (b* (((call-sig y))
                  (entry1 (find-call-tracespec y.fn y.pos tracespec))
                  (entry2 (find-call-tracespec y.fn y.pos tracespec2))
                  (tracespec (call-interior-tracespec entry1 tracespec))
                  (tracespec2 (call-interior-tracespec entry2 tracespec2)))
               (stmtlist-tracespec-subsetp x))))

  (defthm stmtlist-tracespec-subsetp-of-stmt-interior-tracespec
    (implies (and (stmtlist-tracespec-subsetp x)
                  (stmt-tracespec-subsetp y))
             (b* ((entry1 (find-stmt-tracespec y tracespec))
                  (entry2 (find-stmt-tracespec y tracespec2))
                  (tracespec (stmt-interior-tracespec entry1 tracespec))
                  (tracespec2 (stmt-interior-tracespec entry2 tracespec2)))
               (stmtlist-tracespec-subsetp x))))

  (defthm stmtlist-tracespec-subsetp-of-call-interior-tracespec-nil
    (implies (stmtlist-tracespec-subsetp x)
             (b* ((tracespec (call-interior-tracespec nil tracespec))
                  (tracespec2 (call-interior-tracespec nil tracespec2)))
               (stmtlist-tracespec-subsetp x))))

  (defthm stmtlist-tracespec-subsetp-of-stmt-interior-tracespec-nil
    (implies (stmtlist-tracespec-subsetp x)
             (b* ((tracespec (stmt-interior-tracespec nil tracespec))
                  (tracespec2 (stmt-interior-tracespec nil tracespec2)))
               (stmtlist-tracespec-subsetp x)))))
             



(defmacro def-trace-subset-x (type)
  (acl2::template-subst
   '(with-output
      :off (event)
      (define trace-subset-<type>-p ((x <type>-p)
                                     &key
                                     ((static-env static_env_global-p) 'static-env)
                                     ((tracespec tracespec-p) 'tracespec)
                                     ((tracespec2 tracespec-p) 'tracespec2))
        (b* ((fns (all-subfunctions-list (all-callsigs-<type> x) static-env))
             (union (union-tracespecs tracespec tracespec2)))
          (and (call-siglist-tracespec-subsetp
                (traced-callsigs-<type> x (union-tracespecs tracespec tracespec2)))
               (call-siglist-tracespec-subsetp
                (traced-callsigs-fnnames fns static-env union))
               (stmtlist-tracespec-subsetp
                (traced-stmts-fnnames fns static-env union))))))
   :str-alist `(("<TYPE>" . ,(symbol-name type)))
   :pkg-sym 'asl-pkg))





(def-trace-subset-x expr)
(def-trace-subset-x expr_desc)
(def-trace-subset-x exprlist)
(def-trace-subset-x pattern_desc)
(def-trace-subset-x pattern)
(def-trace-subset-x patternlist)
(def-trace-subset-x slice)
(def-trace-subset-x slicelist)
(def-trace-subset-x call)
(def-trace-subset-x type_desc)
(def-trace-subset-x ty)
(def-trace-subset-x tylist)
(def-trace-subset-x int_constraint)
(def-trace-subset-x int_constraintlist)
(def-trace-subset-x constraint_kind)
(def-trace-subset-x array_index)
(def-trace-subset-x named_expr)
(def-trace-subset-x named_exprlist)
(def-trace-subset-x maybe-ty)
(def-trace-subset-x typed_identifier)
(def-trace-subset-x typed_identifierlist)
(def-trace-subset-x lexpr)
(def-trace-subset-x lexpr_desc)
(def-trace-subset-x lexprlist)
(def-trace-subset-x maybe-expr)
(def-trace-subset-x ty-timeframe)
(def-trace-subset-x ty-timeframe-imap)

(local (defthm consp-under-iff
         (implies (true-listp x)
                  (iff (consp x) x))))


;; (defthm all-callsigs-fnnames-of-nil
;;   (equal (all-callsigs-fnnames nil static-env) nil)
;;   :hints(("Goal" :in-theory (enable all-callsigs-fnnames))))

(defthmd trace-subset-expr_desc-p-decomp
  (iff (trace-subset-expr_desc-p x)
       (expr_desc-case x
         :e_atc (and (trace-subset-expr-p x.expr)
                     (trace-subset-ty-p x.type))
         :e_binop (and (trace-subset-expr-p x.arg1)
                       (trace-subset-expr-p x.arg2))
         :e_unop (trace-subset-expr-p x.arg)
         :e_call (trace-subset-call-p x.call)
         :e_slice (and (trace-subset-expr-p x.expr)
                       (trace-subset-slicelist-p x.slices))
         :e_cond (and (trace-subset-expr-p x.test)
                      (trace-subset-expr-p x.then)
                      (trace-subset-expr-p x.else))
         :e_getarray (and (trace-subset-expr-p x.base)
                          (trace-subset-expr-p x.index))
         :e_getenumarray (and (trace-subset-expr-p x.base)
                              (trace-subset-expr-p x.index))
         :e_getfield (trace-subset-expr-p x.base)
         :e_getfields (trace-subset-expr-p x.base)
         :e_getitem (trace-subset-expr-p x.base)
         :e_record (and (trace-subset-ty-p x.type)
                        (trace-subset-named_exprlist-p x.fields))
         :e_tuple (trace-subset-exprlist-p x.exprs)
         :e_array (and (trace-subset-expr-p x.length)
                       (trace-subset-expr-p x.value))
         :e_enumarray (trace-subset-expr-p x.value)
         :e_arbitrary (trace-subset-ty-p x.type)
         :e_pattern (and (trace-subset-expr-p x.expr)
                         (trace-subset-pattern-p x.pattern))
         :otherwise t))
  :hints(("Goal" :in-theory (enable trace-subset-expr_desc-p
                                    trace-subset-expr-p
                                    trace-subset-ty-p
                                    trace-subset-call-p
                                    trace-subset-slicelist-p
                                    trace-subset-named_exprlist-p
                                    trace-subset-exprlist-p
                                    trace-subset-pattern-p
                                    ;; traced-callsigs-fnnames-in-terms-of-all-callsigs
                                    )
          :expand ((all-callsigs-expr_desc x))))
  :rule-classes :definition)


(defthmd trace-subset-expr-p-decomp
  (iff (trace-subset-expr-p x)
       (and (b* (((expr x)))
              (expr_desc-case x.desc
                :e_call (and (call-sig-tracespec-subsetp
                              (call-sig (call->name x.desc.call) x.pos_start))
                             (b* ((subfunctions (all-subfunctions (call-sig (call->name x.desc.call)
                                                                            x.pos_start)
                                                                  static-env))
                                  (union (union-tracespecs tracespec tracespec2)))
                               (and (call-siglist-tracespec-subsetp
                                     (traced-callsigs-fnnames subfunctions static-env union))
                                    (stmtlist-tracespec-subsetp
                                      (traced-stmts-fnnames subfunctions static-env union)))))
                :otherwise t))
            (trace-subset-expr_desc-p (expr->desc x))))
  :hints(("Goal" :in-theory (enable trace-subset-expr-p
                                    trace-subset-expr_desc-p)
          :expand ((all-callsigs-expr x)
                   (all-callsigs-expr-aux x)
                   (:free (a b tracespec) (traced-stmts-fnnames (cons a b) static-env tracespec))
                   (:free (a b tracespec) (traced-callsigs-fnnames (cons a b) static-env tracespec)))))
  :rule-classes :definition)

(defthmd trace-subset-exprlist-p-decomp
  (iff (trace-subset-exprlist-p x)
       (if (atom x)
           t
         (and (trace-subset-expr-p (car x))
              (trace-subset-exprlist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-subset-exprlist-p
                                    trace-subset-expr-p)
          :expand ((all-callsigs-exprlist x))))
  :rule-classes :definition)

(defthmd trace-subset-pattern_desc-p-decomp
  (iff (trace-subset-pattern_desc-p x)
       (pattern_desc-case x
         :pattern_any (trace-subset-patternlist-p x.patterns)
         :pattern_geq (trace-subset-expr-p x.expr)
         :pattern_leq (trace-subset-expr-p x.expr)
         :pattern_not (trace-subset-pattern-p x.pattern)
         :pattern_range (and (trace-subset-expr-p x.lower)
                             (trace-subset-expr-p x.upper))
         :pattern_single (trace-subset-expr-p x.expr)
         :pattern_tuple (trace-subset-patternlist-p x.patterns)
         :otherwise t))
  :hints(("Goal" :in-theory (enable trace-subset-patternlist-p
                                    trace-subset-pattern-p
                                    trace-subset-expr-p
                                    trace-subset-pattern_desc-p)
          :expand ((all-callsigs-pattern_desc x))))
  :rule-classes :definition)

(defthmd trace-subset-pattern-p-decomp
  (iff (trace-subset-pattern-p x)
       (trace-subset-pattern_desc-p (pattern->desc x)))
  :hints(("Goal" :in-theory (enable trace-subset-pattern_desc-p
                                    trace-subset-pattern-p)
          :expand ((all-callsigs-pattern x))))
  :rule-classes :definition)

(defthmd trace-subset-patternlist-p-decomp
  (iff (trace-subset-patternlist-p x)
       (if (atom x)
           t
         (and (trace-subset-pattern-p (car x))
              (trace-subset-patternlist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-subset-patternlist-p
                                    trace-subset-pattern-p)
          :expand ((all-callsigs-patternlist x))))
  :rule-classes :definition)

(defthmd trace-subset-slice-p-decomp
  (iff (trace-subset-slice-p x)
       (slice-case x
         :slice_single (trace-subset-expr-p x.index)
         :slice_range (and (trace-subset-expr-p x.end)
                           (trace-subset-expr-p x.start))
         :slice_length (and (trace-subset-expr-p x.start)
                            (trace-subset-expr-p x.length))
         :slice_star (and (trace-subset-expr-p x.factor)
                          (trace-subset-expr-p x.length))))
  :hints(("Goal" :in-theory (enable trace-subset-expr-p
                                    trace-subset-slice-p)
          :expand ((all-callsigs-slice x))))
  :rule-classes :definition)

(defthmd trace-subset-slicelist-p-decomp
  (iff (trace-subset-slicelist-p x)
       (if (atom x)
           t
         (and (trace-subset-slice-p (car x))
              (trace-subset-slicelist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-subset-slicelist-p
                                    trace-subset-slice-p)
          :expand ((all-callsigs-slicelist x))))
  :rule-classes :definition)


(defthmd trace-subset-call-p-decomp
  (iff (trace-subset-call-p x)
       (b* (((call x)))
         (and (trace-subset-exprlist-p x.params)
              (trace-subset-exprlist-p x.args))))
  :hints(("Goal" :in-theory (enable trace-subset-call-p
                                    trace-subset-exprlist-p)
          :expand ((all-callsigs-call x))))
  :rule-classes :definition)


(defthmd trace-subset-type_desc-p-decomp
  (iff (trace-subset-type_desc-p x)
       (type_desc-case x
         :t_int (trace-subset-constraint_kind-p x.constraint)
         :t_bits (trace-subset-expr-p x.expr)
         :t_tuple (trace-subset-tylist-p x.types)
         :t_array (and (trace-subset-array_index-p x.index)
                       (trace-subset-ty-p x.type))
         :t_record (trace-subset-typed_identifierlist-p x.fields)
         :t_exception (trace-subset-typed_identifierlist-p x.fields)
         :t_collection (trace-subset-typed_identifierlist-p x.fields)
         :otherwise t))
  :hints(("Goal" :in-theory (enable trace-subset-type_desc-p
                                    trace-subset-constraint_kind-p
                                    trace-subset-expr-p
                                    trace-subset-tylist-p
                                    trace-subset-ty-p
                                    trace-subset-array_index-p
                                    trace-subset-typed_identifierlist-p)
          :expand ((all-callsigs-type_desc x))))
  :rule-classes :definition)

(defthmd trace-subset-ty-p-decomp
  (iff (trace-subset-ty-p x)
       (trace-subset-type_desc-p (ty->desc x)))
  :hints(("Goal" :in-theory (enable trace-subset-ty-p
                                    trace-subset-type_desc-p)
          :expand ((all-callsigs-ty x))))
  :rule-classes :definition)

(defthmd trace-subset-tylist-p-decomp
  (iff (trace-subset-tylist-p x)
       (if (atom x)
           t
         (and (trace-subset-ty-p (car x))
              (trace-subset-tylist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-subset-tylist-p
                                    trace-subset-ty-p)
          :expand ((all-callsigs-tylist x))))
  :rule-classes :definition)

(defthmd trace-subset-int_constraint-p-decomp
  (iff (trace-subset-int_constraint-p x)
       (int_constraint-case x
         :constraint_exact (trace-subset-expr-p x.val)
         :constraint_range (and (trace-subset-expr-p x.from)
                                (trace-subset-expr-p x.to))))
  :hints(("Goal" :in-theory (enable trace-subset-int_constraint-p
                                    trace-subset-expr-p)
          :expand ((all-callsigs-int_constraint x))))
  :rule-classes :definition)


(defthmd trace-subset-int_constraintlist-p-decomp
  (iff (trace-subset-int_constraintlist-p x)
       (if (atom x)
           t
         (and (trace-subset-int_constraint-p (car x))
              (trace-subset-int_constraintlist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-subset-int_constraintlist-p
                                    trace-subset-int_constraint-p)
          :expand ((all-callsigs-int_constraintlist x))))
  :rule-classes :definition)


(defthmd trace-subset-constraint_kind-p-decomp
  (iff (trace-subset-constraint_kind-p x)
       (constraint_kind-case x
         :wellconstrained (trace-subset-int_constraintlist-p x.constraints)
         :otherwise t))
  :hints(("Goal" :in-theory (enable trace-subset-constraint_kind-p
                                    trace-subset-int_constraintlist-p)
          :expand ((all-callsigs-constraint_kind x))))
  :rule-classes :definition)


(defthmd trace-subset-array_index-p-decomp
  (iff (trace-subset-array_index-p x)
       (array_index-case x
         :arraylength_expr (trace-subset-expr-p x.length)
         :otherwise t))
  :hints(("Goal" :in-theory (enable trace-subset-array_index-p
                                    trace-subset-expr-p)
          :expand ((all-callsigs-array_index x))))
  :rule-classes :definition)


(defthmd trace-subset-named_expr-p-decomp
  (iff (trace-subset-named_expr-p x)
       (b* (((named_expr x)))
         (trace-subset-expr-p x.expr)))
  :hints(("Goal" :in-theory (enable trace-subset-named_expr-p
                                    trace-subset-expr-p)
          :expand ((all-callsigs-named_expr x))))
  :rule-classes :definition)

(defthmd trace-subset-named_exprlist-p-decomp
  (iff (trace-subset-named_exprlist-p x)
       (if (atom x)
           t
         (and (trace-subset-named_expr-p (car x))
              (trace-subset-named_exprlist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-subset-named_exprlist-p
                                    trace-subset-named_expr-p)
          :expand ((all-callsigs-named_exprlist x))))
  :rule-classes :definition)

(defthmd trace-subset-maybe-ty-decomp
  (iff (trace-subset-maybe-ty-p x)
       (or (not x)
           (trace-subset-ty-p x)))
  :hints(("Goal" :in-theory (enable trace-subset-ty-p
                                    trace-subset-maybe-ty-p
                                    maybe-ty-some->val)
          :expand ((all-callsigs-maybe-ty x))))
  :rule-classes :definition)


(defthmd trace-subset-typed_identifier-p-decomp
  (iff (trace-subset-typed_identifier-p x)
       (b* (((typed_identifier x)))
         (trace-subset-ty-p x.type)))
  :hints(("Goal" :in-theory (enable trace-subset-typed_identifier-p
                                    trace-subset-ty-p)
          :expand ((all-callsigs-typed_identifier x))))
  :rule-classes :definition)

(defthmd trace-subset-typed_identifierlist-p-decomp
  (iff (trace-subset-typed_identifierlist-p x)
       (if (atom x)
           t
         (and (trace-subset-typed_identifier-p (car x))
              (trace-subset-typed_identifierlist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-subset-typed_identifierlist-p
                                    trace-subset-typed_identifier-p)
          :expand ((all-callsigs-typed_identifierlist x))))
  :rule-classes :definition)

(defthmd trace-subset-lexpr_desc-p-decomp
  (iff (trace-subset-lexpr_desc-p x)
       (lexpr_desc-case x
         :le_slice (and (trace-subset-lexpr-p x.base)
                        (trace-subset-slicelist-p x.slices))
         :le_setarray (and (trace-subset-lexpr-p x.base)
                           (trace-subset-expr-p x.index))
         :le_setenumarray (and (trace-subset-lexpr-p x.base)
                               (trace-subset-expr-p x.index))
         :le_setfield (trace-subset-lexpr-p x.base)
         :le_setfields (trace-subset-lexpr-p x.base)
         :le_destructuring (trace-subset-lexprlist-p x.elts)
         :otherwise t))
  :hints(("Goal" :in-theory (enable trace-subset-lexpr_desc-p
                                    trace-subset-lexpr-p
                                    trace-subset-expr-p
                                    trace-subset-slicelist-p
                                    trace-subset-lexprlist-p)
          :expand ((all-callsigs-lexpr_desc x))))
  :rule-classes :definition)


(defthmd trace-subset-lexpr-p-decomp
  (iff (trace-subset-lexpr-p x)
       (trace-subset-lexpr_desc-p (lexpr->desc x)))
  :hints(("Goal" :in-theory (enable trace-subset-lexpr_desc-p
                                    trace-subset-lexpr-p)
          :expand ((all-callsigs-lexpr x))))
  :rule-classes :definition)

(defthmd trace-subset-lexprlist-p-decomp
  (iff (trace-subset-lexprlist-p x)
       (if (atom x)
           t
         (and (trace-subset-lexpr-p (car x))
              (trace-subset-lexprlist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-subset-lexprlist-p
                                    trace-subset-lexpr-p)
          :expand ((all-callsigs-lexprlist x))))
  :rule-classes :definition)

(defthmd trace-subset-maybe-expr-p-decomp
  (iff (trace-subset-maybe-expr-p x)
       (or (not x)
           (trace-subset-expr-p x)))
  :hints(("Goal" :in-theory (enable trace-subset-expr-p
                                    trace-subset-maybe-expr-p
                                    maybe-expr-some->val)
          :expand ((all-callsigs-maybe-expr x))))
  :rule-classes :definition)


(define trace-subset-fnname-p ((x identifier-p)
                               &key
                               ((static-env static_env_global-p) 'static-env)
                               ((tracespec tracespec-p) 'tracespec)
                               ((tracespec2 tracespec-p) 'tracespec2))
  (b* ((union (union-tracespecs tracespec tracespec2)))
    (and (call-siglist-tracespec-subsetp
          (traced-callsigs-fnname x static-env union))
         (stmtlist-tracespec-subsetp
          (traced-stmts-fnname x static-env union))
         (b* ((fns (all-subfunctions-list (all-callsigs-fnname x static-env) static-env)))
           (and (call-siglist-tracespec-subsetp
                 (traced-callsigs-fnnames fns static-env union))
                (stmtlist-tracespec-subsetp
                 (traced-stmts-fnnames fns static-env union)))))))


(defthm trace-subset-fnname-p-when-trace-subset-callexpr-p
  (implies (and (trace-subset-expr-p x)
                (expr_desc-case (expr->desc x) :e_call))
           (trace-subset-fnname-p (call->name (e_call->call (expr->desc x)))))
  :hints(("Goal" :in-theory (enable trace-subset-fnname-p
                                    trace-subset-expr-p)
          :expand ((all-callsigs-expr x)
                   (all-callsigs-expr-aux x)
                   (ALL-CALLSIGS-EXPR_DESC (EXPR->DESC X))
                   (ALL-CALLSIGS-CALL (E_CALL->CALL (EXPR->DESC X)))
                   (:free (a b tracespec) (traced-callsigs-fnnames (cons a b) static-env tracespec))
                   (:free (a b tracespec) (traced-stmts-fnnames (cons a b) static-env tracespec))))))

(defthm trace-subset-fnname-p-implies-recurse-limit
  (b* ((look (hons-assoc-equal (identifier-fix name)
                               (static_env_global->subprograms static-env))))
    (implies (and (trace-subset-fnname-p name
                                         ;; :tracespec (combine-tracespecs t nil tracespec)
                                         ;; :tracespec2 (combine-tracespecs t nil tracespec2)
                                         )
                  look)
             (trace-subset-maybe-expr-p
              (func->recurse_limit
               (func-ses->fn
                (cdr look))))))
  :hints (("goal" :in-theory (enable trace-subset-fnname-p
                                     trace-subset-expr-p
                                     trace-subset-maybe-expr-p
                                     all-callsigs-fnname
                                     all-callsigs-func
                                     all-callsigs-maybe-expr
                                     traced-callsigs-fnname
                                     traced-callsigs-func
                                     maybe-expr-some->val))))

           




(defthm trace-subset-ty-timeframe-imap-implies-lookup
  (implies (and (trace-subset-ty-timeframe-imap-p x)
                (ty-timeframe-imap-p x)
                (hons-assoc-equal name x)
                (identifier-p name))
           (trace-subset-ty-p (ty-timeframe->ty
                             (cdr (hons-assoc-equal name x)))))
  :hints(("Goal" :induct (hons-assoc-equal name x)
          :in-theory (enable trace-subset-ty-p
                             trace-subset-ty-timeframe-imap-p)
          :expand ((all-callsigs-ty-timeframe-imap x)
                   (all-callsigs-ty-timeframe (cdar x))
                   (traced-callsigs-ty-timeframe-imap x tracespec)
                   (traced-callsigs-ty-timeframe (cdar x) tracespec)))))






(defmacro def-trace-subset-s (type)
  (acl2::template-subst
   '(with-output
      :off (event)
      (define trace-subset-<type>-p ((x <type>-p)
                                     &key
                                     ((static-env static_env_global-p) 'static-env)
                                     ((tracespec tracespec-p) 'tracespec)
                                     ((tracespec2 tracespec-p) 'tracespec2))
        (b* ((union (union-tracespecs tracespec tracespec2)))
          (and (stmtlist-tracespec-subsetp (traced-stmts-<type> x union))
               (call-siglist-tracespec-subsetp (traced-callsigs-<type> x union))
               (b* ((fns (all-subfunctions-list (all-callsigs-<type> x) static-env)))
                 (and (stmtlist-tracespec-subsetp (traced-stmts-fnnames fns static-env union))
                      (call-siglist-tracespec-subsetp (traced-callsigs-fnnames fns static-env union))))))))
   :str-alist `(("<TYPE>" . ,(symbol-name type)))
   :pkg-sym 'asl-pkg))

(def-trace-subset-s stmt_desc)
(def-trace-subset-s stmt)
(def-trace-subset-s maybe-stmt)
(def-trace-subset-s catcher)
(def-trace-subset-s catcherlist)

(defthmd trace-subset-stmt_desc-p-decomp
  (iff (trace-subset-stmt_desc-p x)
       (stmt_desc-case x
         :s_seq (and (trace-subset-stmt-p x.first)
                     (trace-subset-stmt-p x.second))
         :s_decl (and (trace-subset-maybe-ty-p x.ty)
                      (trace-subset-maybe-expr-p x.expr))
         :s_assign (and (trace-subset-lexpr-p x.lexpr)
                        (trace-subset-expr-p x.expr))
         :s_call (trace-subset-call-p x.call)
         :s_return (trace-subset-maybe-expr-p x.expr)
         :s_cond (and (trace-subset-expr-p x.test)
                      (trace-subset-stmt-p x.then)
                      (trace-subset-stmt-p x.else))
         :s_assert (trace-subset-expr-p x.expr)
         :s_for (and (trace-subset-expr-p x.start_e)
                     (trace-subset-expr-p x.end_e)
                     (trace-subset-stmt-p x.body)
                     (trace-subset-maybe-expr-p x.limit))
         :s_while (and (trace-subset-expr-p x.test)
                       (trace-subset-maybe-expr-p x.limit)
                       (trace-subset-stmt-p x.body))
         :s_repeat (and (trace-subset-stmt-p x.body)
                        (trace-subset-expr-p x.test)
                        (trace-subset-maybe-expr-p x.limit))
         :s_throw (and (trace-subset-expr-p x.val)
                       (trace-subset-maybe-ty-p x.ty))
         :s_try (and (trace-subset-stmt-p x.body)
                     (trace-subset-catcherlist-p x.catchers)
                     (trace-subset-maybe-stmt-p x.otherwise))
         :s_print (trace-subset-exprlist-p x.args)
         :s_pragma (trace-subset-exprlist-p x.exprs)
         :otherwise t))
  :hints(("Goal" :in-theory (enable trace-subset-exprlist-p
                                    trace-subset-maybe-stmt-p
                                    trace-subset-catcherlist-p
                                    trace-subset-stmt-p
                                    trace-subset-maybe-ty-p
                                    trace-subset-expr-p
                                    trace-subset-maybe-expr-p
                                    trace-subset-call-p
                                    trace-subset-lexpr-p
                                    trace-subset-stmt_desc-p)
          :expand ((all-callsigs-stmt_desc x)
                   (all-stmts-stmt_desc x))))
  :rule-classes :definition)

(defthmd trace-subset-stmt-p-decomp
  (iff (trace-subset-stmt-p x)
       (and (stmt-tracespec-subsetp x)
            (b* (((stmt x)))
              (stmt_desc-case x.desc
                :s_call (and (call-sig-tracespec-subsetp
                              (call-sig (call->name x.desc.call) x.pos_start))
                             (b* ((subfunctions (all-subfunctions (call-sig (call->name x.desc.call)
                                                                            x.pos_start)
                                                                  static-env))
                                  (union (union-tracespecs tracespec tracespec2)))
                               (and (call-siglist-tracespec-subsetp
                                      (traced-callsigs-fnnames subfunctions static-env union))
                                    (stmtlist-tracespec-subsetp
                                     (traced-stmts-fnnames subfunctions static-env union)))))
                :otherwise t))
            (trace-subset-stmt_desc-p (stmt->desc x))))
  :hints(("Goal" :in-theory (enable trace-subset-stmt-p
                                    trace-subset-stmt_desc-p)
          :expand ((all-callsigs-stmt x)
                   (all-stmts-stmt x)
                   (all-callsigs-stmt-aux x)
                   (all-stmts-stmt-aux x)
                   (:free (a b tracespec) (traced-callsigs-fnnames (cons a b) static-env tracespec))
                   (:free (a b tracespec) (traced-stmts-fnnames (cons a b) static-env tracespec)))))
  :rule-classes :definition)


(defthm trace-subset-fnname-p-implies-body
  (b* ((look (hons-assoc-equal (identifier-fix name)
                               (static_env_global->subprograms static-env))))
    (implies (and (trace-subset-fnname-p name)
                  look
                  (subprogram_body-case (func->body (func-ses->fn (cdr look))) :sb_asl))
             (trace-subset-stmt-p
              (sb_asl->stmt
               (func->body (func-ses->fn (cdr look)))))))
  :hints (("goal" :in-theory (enable trace-subset-fnname-p
                                     trace-subset-stmt-p
                                     all-callsigs-fnname
                                     all-callsigs-func
                                     traced-callsigs-fnname
                                     traced-callsigs-func
                                     traced-stmts-fnname
                                     traced-stmts-func))))

(defthm trace-subset-fnname-p-when-trace-subset-callstmt-p
  (implies (and (trace-subset-stmt-p x)
                (stmt_desc-case (stmt->desc x) :s_call))
           (trace-subset-fnname-p (call->name (s_call->call (stmt->desc x)))))
  :hints(("Goal" :in-theory (enable trace-subset-fnname-p
                                    trace-subset-stmt-p)
          :expand ((all-callsigs-stmt x)
                   (all-callsigs-stmt-aux x)
                   (ALL-CALLSIGS-STMT_DESC (STMT->DESC X))
                   (ALL-CALLSIGS-CALL (S_CALL->CALL (STMT->DESC X)))
                   (:free (a b tracespec) (traced-callsigs-fnnames (cons a b) static-env tracespec))
                   (:free (a b tracespec) (traced-stmts-fnnames (cons a b) static-env tracespec))))))



(defthmd trace-subset-maybe-stmt-p-decomp
  (iff (trace-subset-maybe-stmt-p x)
       (or (not x)
           (trace-subset-stmt-p x)))
  :hints(("Goal" :in-theory (enable trace-subset-stmt-p
                                    trace-subset-maybe-stmt-p
                                    maybe-stmt-some->val)
          :expand ((all-callsigs-maybe-stmt x)
                   (all-stmts-maybe-stmt x))))
  :rule-classes :definition)

(defthmd trace-subset-catcher-p-decomp
  (iff (trace-subset-catcher-p x)
       (b* (((catcher x)))
         (and (trace-subset-ty-p x.ty)
              (trace-subset-stmt-p x.stmt))))
  :hints(("Goal" :in-theory (enable trace-subset-catcher-p
                                    trace-subset-ty-p
                                    trace-subset-stmt-p)
          :expand ((all-callsigs-catcher x)
                   (all-stmts-catcher x))))
  :rule-classes :definition)


(defthmd trace-subset-catcherlist-p-decomp
  (iff (trace-subset-catcherlist-p x)
       (if (atom x)
           t
         (and (trace-subset-catcher-p (car x))
              (trace-subset-catcherlist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-subset-catcherlist-p
                                    trace-subset-catcher-p)
          :expand ((all-callsigs-catcherlist x)
                   (all-stmts-catcherlist x))))
  :rule-classes :definition)



(local
 (with-output
   ;; makes it so it won't take forever to print the induction scheme
   :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
   :off (event)
   (make-event
    (b* (((std::defines-guts guts)
          (cdr (assoc 'asl-interpreter-mutual-recursion-*t (std::get-defines-alist (w state))))))
      `(flag::make-flag asl-interpreter-mutual-recursion-*t-flag
                        eval_expr-*t-fn
                        :flag-mapping ,guts.flag-mapping)))))
                             



(local (include-book "centaur/vl/util/default-hints" :dir :system))

(local (in-theory (acl2::disable* asl-*t-equals-original-rules)))
(local (in-theory (enable ev_error->desc-when-wrong-kind)))

(local (defthm ev_error->desc-of-v_to_bool
         (not (equal (ev_error->desc (v_to_bool x))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable v_to_bool)))))

(local (defthm ev_error->desc-of-v_to_int
         (not (equal (ev_error->desc (v_to_int x))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable v_to_int)))))

(local (defthm ev_error->desc-of-v_to_label
         (not (equal (ev_error->desc (v_to_label x))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable v_to_label)))))

(local (defthm ev_error->desc-of-env-find-global
         (not (equal (ev_error->desc (env-find-global v env))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable env-find-global)))))

(local (defthm ev_error->desc-of-tick_loop_limit
         (not (equal (ev_error->desc (tick_loop_limit x))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable tick_loop_limit)))))

(local (defthm ev_error->desc-of-rethrow_implicit
         (equal (ev_error->desc (rethrow_implicit throw blkres bt))
                (ev_error->desc blkres))
         :hints(("Goal" :in-theory (enable rethrow_implicit)))))

(local (defthm ev_error->desc-of-bitvec_fields_to_record!
         (not (equal (ev_error->desc (bitvec_fields_to_record! fields slices rec bv width))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable bitvec_fields_to_record!
                                           (:i bitvec_fields_to_record!))
                 :induct t))))

(local (defthm ev_error->desc-of-bitvec_fields_to_record
         (not (equal (ev_error->desc (bitvec_fields_to_record fields pairs res v))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable bitvec_fields_to_record)))))

(local (defthm ev_error->desc-of-check_two_ranges_non_overlapping
         (not (equal (ev_error->desc (check_two_ranges_non_overlapping x y))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable check_two_ranges_non_overlapping)))))

(local (defthm ev_error->desc-of-check_non_overlapping_slices-1
         (not (equal (ev_error->desc (check_non_overlapping_slices-1 x y))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable check_non_overlapping_slices-1)
                 :induct t))))

(local (defthm ev_error->desc-of-check_non_overlapping_slices
         (not (equal (ev_error->desc (check_non_overlapping_slices x))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable check_non_overlapping_slices)
                 :induct t))))

(local (defthm ev_error->desc-of-vbv-to-int
         (not (equal (ev_error->desc (vbv-to-int vec))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable vbv-to-int)))))

(local (defthm ev_error->desc-of-check-bad-slices
         (not (equal (ev_error->desc (check-bad-slices width slices))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable check-bad-slices)
                 :induct t))))

(local (defthm ev_error->desc-of-check_recurse_limit
         (not (equal (ev_error->desc (check_recurse_limit env name res))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable check_recurse_limit)))))

(local (defthm ev_error->desc-of-write_to_bitvector
         (not (equal (ev_error->desc (write_to_bitvector pairs vec val))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable write_to_bitvector)))))

(local (defthm ev_error->desc-of-eval_primitive
         (not (equal (ev_error->desc (eval_primitive name params args))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable eval_primitive)))))

(local (defthm ev_error->desc-of-eval_binop
         (not (equal (ev_error->desc (eval_binop op arg1 arg2))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable eval_binop)))))

(local (defthm ev_error->desc-of-eval_unop
         (not (equal (ev_error->desc (eval_unop op arg))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable eval_unop)))))

(local (defthm ev_error->desc-of-eval_pattern_mask
         (not (equal (ev_error->desc (eval_pattern_mask val mask))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable eval_pattern_mask)))))

(local (defthm ev_error->desc-of-get_field!
         (not (equal (ev_error->desc (get_field! field rec))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable get_field!)))))

(local (defthm ev_error->desc-of-get_field
         (not (equal (ev_error->desc (get_field field rec))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable get_field)))))

(local (defthm ev_error->desc-of-map-get_field!
         (not (equal (ev_error->desc (map-get_field! field rec))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable map-get_field!)))))

(local (defthm ev_error->desc-of-map-get_field
         (not (equal (ev_error->desc (map-get_field field rec))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable map-get_field)))))

(local (defthm ev_error->desc-of-concat_bitvectors
         (not (equal (ev_error->desc (concat_bitvectors vals))
                     "Trace abort"))
         :hints(("Goal" :in-theory (enable concat_bitvectors)))))

(local
 (with-output
   ;; makes it so it won't take forever to print the induction scheme
   :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
   :off (event)
   (make-event
    (b* (((std::defines-guts guts)
          (cdr (assoc 'asl-interpreter-mutual-recursion (std::get-defines-alist (w state))))))
      `(flag::make-flag asl-interpreter-mutual-recursion-flag
                        eval_expr-ff
                        :flag-mapping ,guts.flag-mapping)))))



(with-output
  ;; makes it so it won't take forever to print the induction scheme
  :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
  :off (event)

  (std::defret-mutual-generate <fn>-not-trace-abort
    :mutual-recursion asl-interpreter-mutual-recursion
                
    :rules ((t (:add-concl (not (equal (ev_error->desc res) "Trace abort")))))
    :hints ((vl::big-mutrec-default-hint 'eval_expr-fn id nil world))))




;; (define combine/perm-trans ((perm tracespec-p)
;;                             (trans tracespec-p))
;;   (b* (((tracespec perm))
;;        ((tracespec trans)))
;;     (make-tracespec :call-specs-transient trans.call-specs-transient
;;                     :stmt-specs-transient trans.stmt-specs-transient
;;                     :call-specs-permanent perm.call-specs-permanent
;;                     :stmt-specs-permanent perm.stmt-specs-permanent)))

(defthm interp-result-trace-subsetp-reflexive
  (iff (interp-result-trace-subsetp x trace x trace2)
       (or (equal (ev_error->desc x) "Trace abort")
           (tracelist-subsetp trace trace2)))
  :hints(("Goal" :in-theory (enable interp-result-trace-subsetp))))

;; (defthm interp-result-trace-subsetp-reflexive
;;   (interp-result-trace-subsetp x trace x trace)
;;   :hints(("Goal" :in-theory (enable interp-result-trace-subsetp))))

(defthm interp-result-trace-subsetp-by-unequal-kinds
  (implies (and (interp-result-trace-subsetp z tr y tr2)
                (not (equal (eval_result-kind z) (eval_result-kind y))))
           (interp-result-trace-subsetp x trx y tr2))
  :hints(("Goal" :in-theory (enable interp-result-trace-subsetp))))

;; (defthm interp-result-trace-subsetp-when-not-trace-abort
;;   (implies (not (equal (ev_error->desc y) "Trace abort"))
;;            (equal (interp-result-trace-subsetp x y)
;;                   (and (eval_result-equiv x y)
;;                        (tracelist-subsetp tr1 tr2))))
;;   :hints(("Goal" :in-theory (enable interp-result-trace-subsetp))))

(defthm interp-result-trace-subsetp-when-not-error
  (implies (not (eval_result-case y :ev_error))
           (equal (interp-result-trace-subsetp x tr1 y tr2)
                  (and (eval_result-equiv x y)
                       (tracelist-subsetp tr1 tr2))))
  :hints(("Goal" :in-theory (enable interp-result-trace-subsetp))))


;; (defthm interp-result-trace-subsetp-when-not-error
;;   (implies (not (eval_result-case x :ev_error))
;;            (interp-result-trace-subsetp x y))
;;   :hints(("Goal" :in-theory (enable interp-result-trace-subsetp
;;                                     ev_error->desc-when-wrong-kind))))

;; (defthm interp-result-trace-subsetp-when-not-trace-abort
;;   (implies (not (equal (ev_error->desc x) "Trace abort"))
;;            (interp-result-trace-subsetp x y))
;;   :hints(("Goal" :in-theory (enable interp-result-trace-subsetp))))

(defthm tracelist-subsetp-of-append
  (iff (tracelist-subsetp (append x y) z)
       (and (tracelist-subsetp x z)
            (tracelist-subsetp y z)))
  :hints(("Goal" :in-theory (enable tracelist-subsetp))))

(defthm find-trace-subset-of-append
  (iff (find-trace-subset x (append y z))
       (or (find-trace-subset x y)
           (find-trace-subset x z)))
  :hints(("Goal" :in-theory (enable find-trace-subset))))

(defthm tracelist-subsetp-of-append-2
  (implies (or (tracelist-subsetp x y)
               (tracelist-subsetp x z))
           (tracelist-subsetp x (append y z)))
  :hints(("Goal" ;;
          :in-theory (enable tracelist-subsetp)
          :expand ((:free (y) (tracelist-subsetp x y)))
          :induct (len x))))

(local
 (defthm trace-subset-stmt-p-of-find-catcher
   (b* ((c (find_catcher static-env throw catchers)))
     (implies (and (trace-subset-catcherlist-p catchers)
                   c)
              (trace-subset-stmt-p (catcher->stmt c))))
   :hints(("Goal" :in-theory (enable find_catcher)
           :induct t
           :expand ((trace-subset-catcherlist-p catchers)
                    (trace-subset-catcher-p (car catchers)))))))



;; (defthmd trace-subset-fnname-p-when-trace-subset-callstmt-p-special
;;   (implies (and (trace-subset-stmt-p x :tracespec2 (combine/perm-trans tracespec2 tracespec))
;;                 (stmt_desc-case (stmt->desc x) :s_call))
;;            (trace-subset-fnname-p (call->name (s_call->call (stmt->desc x)))
;;                                   :tracespec (change-tracespec tracespec :call-specs-transient nil :stmt-specs-transient nil)
;;                                   :tracespec2 (change-tracespec tracespec2 :call-specs-transient nil :stmt-specs-transient nil)))
;;   :hints(("Goal" :use ((:instance trace-subset-fnname-p-when-trace-subset-callstmt-p
;;                         (tracespec2 (combine/perm-trans tracespec2 tracespec))))
;;           :in-theory (e/d (combine/perm-trans)
;;                           (trace-subset-fnname-p-when-trace-subset-callstmt-p)))))

;; (defthmd trace-subset-fnname-p-when-trace-subset-callexpr-p-special
;;   (implies (and (trace-subset-expr-p x :tracespec2 (combine/perm-trans tracespec2 tracespec))
;;                 (expr_desc-case (expr->desc x) :e_call))
;;            (trace-subset-fnname-p (call->name (e_call->call (expr->desc x)))
;;                                   :tracespec (change-tracespec tracespec :call-specs-transient nil :stmt-specs-transient nil)
;;                                   :tracespec2 (change-tracespec tracespec2 :call-specs-transient nil :stmt-specs-transient nil)))
;;   :hints(("Goal" :use ((:instance trace-subset-fnname-p-when-trace-subset-callexpr-p
;;                         (tracespec2 (combine/perm-trans tracespec2 tracespec))))
;;           :in-theory (e/d (combine/perm-trans)
;;                           (trace-subset-fnname-p-when-trace-subset-callexpr-p)))))


(defthm-expr_of_lexpr-flag
  (defthm trace-subset-expr-p-of-expr_of_lexpr
    (implies (trace-subset-lexpr-p x)
             (trace-subset-expr-p (expr_of_lexpr x)))
    :hints ('(:expand ((expr_of_lexpr x)
                       (trace-subset-lexpr-p x)
                       (trace-subset-lexpr_desc-p (lexpr->desc x))
                       (:free (x pos) (trace-subset-expr-p (expr x pos))))
              :in-theory (enable trace-subset-expr_desc-p-decomp)))
    :flag expr_of_lexpr)
  (defthm trace-subset-expr-p-of-expr_of_lexprlist
    (implies (trace-subset-lexprlist-p x)
             (trace-subset-exprlist-p (exprlist_of_lexprlist x)))
    :hints ('(:expand ((exprlist_of_lexprlist x)
                       (trace-subset-lexprlist-p x)
                       (trace-subset-exprlist-p nil)
                       (:free (a b) (trace-subset-exprlist-p (cons a b))))))
    :flag exprlist_of_lexprlist))

(local (defthm assoc-equal-is-hons-assoc-equal
         (implies k
                  (equal (assoc-equal k x)
                         (hons-assoc-equal k x)))))

;; (local (include-book "interp-mods"))

;; (local
;;  (defun make-eval_*t-result-rw (new-fn old-fn wrld)
;;    (Declare (xargs :mode :program))
;;    (let* ((macro-args (macro-args new-fn wrld))
;;           (nonkey-args (take (- (len macro-args)
;;                                 (len (member '&key macro-args)))
;;                              macro-args))
;;           (new-fn-equals-original (intern-in-package-of-symbol
;;                                    (concatenate 'string (symbol-name new-fn) "-EQUALS-ORIGINAL")
;;                                    new-fn)))
;;      `((defret <fn>-result-rw
;;          (equal (mv-nth 0 (,new-fn . ,nonkey-args))
;;                 (let ((r0 (hide (mv-nth 0 (,new-fn . ,nonkey-args)))))
;;                   (if (equal (ev_error->desc r0) "Trace abort")
;;                       (ev_error "Trace abort"
;;                                 (ev_error->data r0)
;;                                 (ev_error->backtrace r0))
;;                     (mv-nth 0 (,old-fn . ,(subst '(env-replace-static static-env env) 'env nonkey-args))))))
;;          :fn ,new-fn
;;          :hints (("goal" :use ,new-fn-equals-original
;;                   :expand ((:free (x) (hide x)))
;;                   :in-theory (disable ,new-fn-equals-original))))
;;        (defret <fn>-orac-rw
;;          (equal (mv-nth 1 (,new-fn . ,nonkey-args))
;;                 (let ((r0 (hide (mv-nth 0 (,new-fn . ,nonkey-args))))
;;                       (r1 (hide (mv-nth 1 (,new-fn . ,nonkey-args)))))
;;                   (if (equal (ev_error->desc r0) "Trace abort")
;;                       r1
;;                     (mv-nth 1 (,old-fn . ,(subst '(env-replace-static static-env env) 'env nonkey-args))))))
;;          :fn ,new-fn
;;          :hints (("goal" :use ,new-fn-equals-original
;;                   :in-theory (disable ,new-fn-equals-original
;;                                       <fn>-result-rw)
;;                   :expand ((:free (x) (hide x))))))))))

;; (local
;;  (defun make-eval_*t-result-rws (pairs wrld)
;;    (Declare (xargs :mode :program))
;;    (if (atom pairs)
;;        nil
;;      (append (make-eval_*t-result-rw (cdar pairs) (caar pairs) wrld)
;;              (make-eval_*t-result-rws (cdr pairs) wrld)))))


;; (local (defun def-eval_*t-result-rws-fn (wrld)
;;          (Declare (xargs :mode :program))
;;          (cons 'progn
;;                (make-eval_*t-result-rws
;;                 (list* (cons 'eval_subprogram 'eval_subprogram-*t1)
;;                        (cons 'eval_stmt 'eval_stmt-*t1)
;;                        (pair-suffixed *asl-interp-fns* '-*t))
;;                 wrld))))


;; (encapsulate nil
;;   (local (deflabel before-result-rws))
;;   (make-event
;;    (def-eval_*t-result-rws-fn (w state)))
;;   (acl2::def-ruleset! asl-*t-result-rws
;;     (set-difference-theories (current-theory :here)
;;                              (current-theory 'before-result-rws))))
         

(local (defthmd eval_stmt-*t-result-rw
         (equal (mv-nth 0 (eval_stmt-*t env s))
                (let ((r1 (hide (mv-nth 0 (eval_stmt-*t env s)))))
                  (if (equal (ev_error->desc r1) "Trace abort")
                      r1
                    (mv-nth 0 (eval_stmt (env-replace-static static-env env) s)))))
         :hints (("goal" :use eval_stmt-*t-equals-original
                  :expand ((:free (x) (hide x)))))))

(local (defthmd eval_subprogram-*t-result-rw
         (equal (mv-nth 0 (eval_subprogram-*t env name vparams vargs))
                (let ((r1 (hide (mv-nth 0 (eval_subprogram-*t env name vparams vargs)))))
                  (if (equal (ev_error->desc r1) "Trace abort")
                      r1
                    (mv-nth 0 (eval_subprogram (env-replace-static static-env env) name vparams vargs)))))
         :hints (("goal" :use eval_subprogram-*t-equals-original
                  :expand ((:free (x) (hide x)))))))

(local (defthmd eval_subprogram-*t1-result-rw
         (equal (mv-nth 0 (eval_subprogram-*t1 env name vparams vargs))
                (let ((r1 (hide (mv-nth 0 (eval_subprogram-*t1 env name vparams vargs)))))
                  (if (equal (ev_error->desc r1) "Trace abort")
                      r1
                    (mv-nth 0 (eval_subprogram (env-replace-static static-env env) name vparams vargs)))))
         :hints (("goal" :use eval_subprogram-*t1-equals-original
                  :expand ((:free (x) (hide x)))))))

;; (local (defthmd resolve-constraint_kind-*t-result-rw
;;          (equal (mv-nth 0 (resolve-constraint_kind-*t env x pos))
;;                 (let ((r1 (hide (mv-nth 0 (resolve-constraint_kind-*t env x pos)))))
;;                   (if (equal (ev_error->desc r1) "Trace abort")
;;                       r1
;;                     (mv-nth 0 (resolve-constraint_kind (env-replace-static static-env env) x pos)))))
;;          :hints (("goal" :use resolve-constraint_kind-*t-equals-original
;;                   :expand ((:free (x) (hide x)))))))

(local
 (defthm trace-subset-constraint-kind-p-of-deparameterized
  (TRACE-SUBSET-CONSTRAINT_KIND-P
   (WELLCONSTRAINED
    (LIST (CONSTRAINT_EXACT (EXPR (E_VAR (PARAMETRIZED->NAME X))
                                  '((FNAME . "<none>")
                                    (LNUM . 0)
                                    (BOL . 0)
                                    (CNUM . 0)))))
    '(:PRECISION_FULL)))
  :hints(("Goal" :in-theory (enable trace-subset-constraint_kind-p
                                    all-callsigs-constraint_kind
                                    all-callsigs-int_constraintlist
                                    all-callsigs-int_constraint
                                    all-callsigs-expr
                                    all-callsigs-expr-aux
                                    all-callsigs-expr_desc)))))

(local
 (defthm trace-subset-exprlist-p-of-named_exprlist->exprs
   (implies (trace-subset-named_exprlist-p x)
            (trace-subset-exprlist-p (named_exprlist->exprs x)))
   :hints(("Goal" :in-theory (enable named_exprlist->exprs
                                     trace-subset-named_exprlist-p-decomp
                                     trace-subset-exprlist-p-decomp
                                     trace-subset-named_expr-p-decomp)))))

;; (local (in-theory (acl2::enable*
;;                    interp-result-trace-subsetp
;;                    asl-*t-equals-original-rules)))


;; (local
;;  (defthm combine/perm-trans-of-combine-tracespecs
;;    (equal (combine/perm-trans tracespec2 (combine-tracespecs t nil tracespec))
;;           (change-tracespec tracespec2 :call-specs-transient nil :stmt-specs-transient nil))
;;    :hints(("Goal" :in-theory (enable combine-tracespecs combine/perm-trans)))))

;; (local
;;  (defthm combine-tracespecs-of-combine/perm-trans
;;    (equal (combine-tracespecs t nil (combine/perm-trans tracespec2 tracespec))
;;           (combine-tracespecs t nil tracespec2))
;;    :hints(("Goal" :in-theory (enable combine-tracespecs combine/perm-trans)))))

;; (local
;;  (defthm permanent-of-combine-perm/trans
;;    (and (equal (tracespec->stmt-specs-permanent
;;                 (combine/perm-trans perm trans))
;;                (tracespec->stmt-specs-permanent perm))
;;         (equal (tracespec->call-specs-permanent
;;                 (combine/perm-trans perm trans))
;;                (tracespec->call-specs-permanent perm)))
;;    :hints(("Goal" :in-theory (enable combine/perm-trans)))))

;; (local
;;  (defthm combine-tracespecs-nil
;;    (equal (combine-tracespecs t nil tracespec2)
;;           (change-tracespec tracespec2
;;                             :call-specs-transient nil
;;                             :stmt-specs-transient nil))
;;    :hints(("Goal" :in-theory (enable combine-tracespecs)))))

;; (local
;;  (defthm trace-subset-fnname-p-of-combine-tracespecs
;;    (let ((ts-perm (change-tracespec tracespec :call-specs-transient nil :stmt-specs-transient nil))
;;          (ts2-perm (change-tracespec tracespec2 :call-specs-transient nil :stmt-specs-transient nil)))
;;      (implies (trace-subset-fnname-p name :tracespec ts-perm :tracespec2 ts2-perm)
;;               (trace-subset-fnname-p name
;;                                      :tracespec (combine-tracespecs t nil tracespec)
;;                                      :tracespec2 ts2-perm)))
;;    :hints(("Goal" :in-theory (enable combine-tracespecs combine/perm-trans)))))


;; (defthm call-sig-tracespec-subsetp-implies-subsetp-when-transient-empty
;;   (implies (and (call-sig-tracespec-subsetp name)
;;                 (equal (tracespec->call-specs-transient tracespec)
;;                        (tracespec->call-specs-transient tracespec2))
;;                 (equal (tracespec->stmt-specs-transient tracespec)
;;                        (tracespec->stmt-specs-transient tracespec2)))
;;            (call-sig-tracespec-subsetp name
;;                                   :tracespec (change-tracespec tracespec :call-specs-transient call-trans :stmt-specs-transient stmt-trans)
;;                                   :tracespec2 (change-tracespec tracespec2 :call-specs-transient call-trans :stmt-specs-transient stmt-trans)))
;;   :hints(("Goal" :in-theory (enable call-sig-tracespec-subsetp
;;                                     find-call-tracespec))))


;; (defthm trace-subset-fnname-p-implies-empty-transient
;;   (implies (trace-subset-fnname-p name)
;;            (trace-subset-fnname-p name
;;                                   :tracespec (change-tracespec tracespec :call-specs-transient call-trans :stmt-specs-transient stmt-trans)
;;                                   :tracespec2 (change-tracespec tracespec2 :call-specs-transient call-trans :stmt-specs-transient stmt-trans)))
;;   :hints(("Goal" :in-theory (enable trace-subset-fnname-p))))



;; (defthm trace-subset-fnname-p-norm-1
;;   (implies (syntaxp (not (and (equal sts2 ''nil)
;;                               (equal cls2 ''nil))))
;;            (equal (trace-subset-fnname-p name :tracespec (tracespec sts1 cls1 sts2 cls2))
;;                   (trace-subset-fnname-p name :tracespec (tracespec sts1 cls1 nil nil))))
;;   :hints(("Goal" :in-theory (enable trace-subset-fnname-p))))

;; (defthm trace-subset-fnname-p-norm-2
;;   (implies (syntaxp (not (and (equal sts2 ''nil)
;;                               (equal cls2 ''nil))))
;;            (equal (trace-subset-fnname-p name :tracespec2 (tracespec sts1 cls1 sts2 cls2))
;;                   (trace-subset-fnname-p name :tracespec2 (tracespec sts1 cls1 nil nil))))
;;   :hints(("Goal" :in-theory (enable trace-subset-fnname-p))))

;; (defthm trace-subset-fnname-p-norm-1-var
;;   (equal (trace-subset-fnname-p name :tracespec (tracespec (tracespec->stmt-specs-permanent tracespec)
;;                                                            (tracespec->call-specs-permanent tracespec) sts2 cls2))
;;          (trace-subset-fnname-p name))
;;   :hints(("Goal" :in-theory (enable trace-subset-fnname-p))))

;; (defthm trace-subset-fnname-p-norm-2-var
;;   (equal (trace-subset-fnname-p name :tracespec2 (tracespec (tracespec->stmt-specs-permanent tracespec2)
;;                                                             (tracespec->call-specs-permanent tracespec2) sts2 cls2))
;;          (trace-subset-fnname-p name))
;;   :hints(("Goal" :in-theory (enable trace-subset-fnname-p))))






(encapsulate nil
  (local (include-book "interp-mods"))
  (local
   (defconst *eval_subprogram-*tt-def*
     '(define eval_subprogram-*tt ((env env-p)
                                   (name identifier-p)
                                   (vparams vallist-p)
                                   (vargs vallist-p)
                                   &key
                                   ((clk natp) 'clk)
                                   (orac 'orac)
                                   ((pos posn-p) 'pos)
                                   ((static-env static_env_global-p) 'static-env)
                                   ((tracespec tracespec-p) 'tracespec)
                                   ((tracespec2 tracespec-p) 'tracespec2))
        :short "Tracing version of @(see eval_subprogram); see @(see
asl-interpreter-mutual-recursion-*t) for overview."
        :guard (equal (global-env->static (env->global env)) static-env)
        :measure (nats-measure clk 1 0 1)
        :returns (mv (res func_eval_result-p) new-orac
                     (trace asl-tracelist-p))
        (b* ((ts-entry (find-call-tracespec name pos tracespec))
             (ts-entry2 (find-call-tracespec name pos tracespec2))
           ((when (eq (maybe-call-tracespec->abort ts-entry) :before))
            (b* ((trace (call-trace-abort-before-output ts-entry name vparams vargs pos)))
              (pass-error-*t
               (ev_error "Trace abort" ts-entry (list (posn-fix pos))))))
             (tracespec (call-interior-tracespec ts-entry tracespec))
             (tracespec2 (call-interior-tracespec ts-entry2 tracespec2))
             ((mv res orac trace) (eval_subprogram-*tt1 env name vparams vargs))
           (trace (call-trace-output
                   ts-entry name vparams vargs pos res trace))
           ((when (call-abort-after ts-entry res))
            (pass-error-*t
             (ev_error "Trace abort" ts-entry (list (posn-fix pos))))))
        (mv res orac trace)))))

  (local
   (defconst *eval_stmt-*tt-def*
     '(define eval_stmt-*tt ((env env-p)
                             (s stmt-p)
                             &key
                             ((clk natp) 'clk)
                             (orac 'orac)
                             ((static-env static_env_global-p) 'static-env)
                             ((tracespec tracespec-p) 'tracespec)
                             ((tracespec2 tracespec-p) 'tracespec2))
        :short "Tracing version of @(see eval_stmt); see @(see
asl-interpreter-mutual-recursion-*t) for overview."
        :guard (equal (global-env->static (env->global env)) static-env)
        :measure (nats-measure clk 0 (stmt-count* s) 1)
        :returns (mv (res stmt_eval_result-p) new-orac
                     (trace asl-tracelist-p))
        (b* ((ts-entry (find-stmt-tracespec s tracespec))
             (ts-entry2 (find-stmt-tracespec s tracespec2))
             ((when (eq (maybe-stmt-tracespec->abort ts-entry) :before))
              (b* ((trace (stmt-trace-abort-before-output ts-entry env s)))
                (pass-error-*t
                 (ev_error "Trace abort" ts-entry (list (stmt->pos_start s))))))
             (tracespec (stmt-interior-tracespec ts-entry tracespec))
             (tracespec2 (stmt-interior-tracespec ts-entry2 tracespec2))
             ((mv res orac trace) (eval_stmt-*tt1 env s))
           (trace (stmt-trace-output ts-entry env s res trace))
           ((when (stmt-abort-after ts-entry res))
            (pass-error-*t
             (ev_error "Trace abort" ts-entry (list (stmt->pos_start s))))))
        (mv res orac trace)))))

  (local
   (defconsts *eval-trace-substitution-tt*
     (append (pair-suffixed (append *asl-interp-fns*
                                    '(asl-interpreter-mutual-recursion))
                            '-*tt)
             (pair-suffixed '(evo_normal pass-error evo_error evo_throwing evo-return
                                         evbind evbind-nonrec evoo evo evob evs evtailcall)
                            '-*t))))
  (local (in-theory (disable (:t eval_result-kind)
                             (:t append)
                             (:t pass-error)
                             (:t val-kind)
                             (:t acl2::true-listp-append)
                             (:t eval_expr)
                             (:t expr_result->env)
                             (:t expr_result->val)
                             (:t ev_error)
                             (:t pass-error-*t)
                             (:t ev_normal)
                             default-car
                             default-cdr
                             static_env_global-fix-when-static_env_global-p
                             global-replace-static-with-self
                             env-replace-static-with-self)))

  (local
   (defun replace-static-envs (x)
     (if (atom x)
         x
       (case-match x
         (('global-env->static ('env->global . &) . &) 'static-env)
         (& (cons (replace-static-envs (car x))
                  (replace-static-envs (cdr x))))))))

  (local
   (defun add-trace-to-returns (x)
     (if (atom x)
         x
       (case-match x
         ((':returns x . rest)
          `(:returns (,@x (trace asl-tracelist-p)) . ,rest))
         (& (cons (add-trace-to-returns (car x))
                  (add-trace-to-returns (cdr x))))))))

  (local (defun defines-add-flag (x)
           (declare (xargs :mode :program))
           (if (atom x)
               x
             (case-match x
               (('define name formals . rest)
                `(define ,name ,formals
                   :flag ,(intern-in-package-of-symbol
                           (str::strsubst "-*TT" "-*T" (symbol-name name))
                           name)
                   . ,rest))
               (& (cons (defines-add-flag (car x))
                        (defines-add-flag (cdr x))))))))


  (local
   (defun return-equiv-thm (name-old name-new nonkey-formals no-expand-name)
     (let ((thmname '<fn>-equals-original))
       `(defret ,thmname
          (equal (,name-new . ,nonkey-formals)
                 (,name-old . ,nonkey-formals))
          :hints ((let ((expand (acl2::just-expand-cp-parse-hints
                                 '((:free (,@nonkey-formals clk orac) (,name-old . ,nonkey-formals))
                                   ,@(and (not no-expand-name)
                                          `((:free (,@nonkey-formals clk orac) (,name-new . ,nonkey-formals)))))
                                 world)))
                    `(:computed-hint-replacement
                      ((acl2::expand-marked))
                      :clause-processor (acl2::mark-expands-cp
                                         clause
                                         '(t ;; last-only
                                           t ;; lambdas
                                           ,expand))
                      :do-not-induct t))
                  (and stable-under-simplificationp
                       '(:in-theory (enable cons-equal))))
          ;; :rule-classes nil
          :fn ,name-new))))


  (local
   (defun eval-return-equiv-thms (names wrld)
     (if (atom names)
         nil
       (b* ((name (car names))
            (name-new (intern-in-package-of-symbol
                       (concatenate 'string (symbol-name name) "-"
                                    (if (or (eq name 'eval_subprogram)
                                            (eq name 'eval_stmt))
                                        "*TT1"
                                      "*TT"))
                       name))
            (name-old (intern-in-package-of-symbol
                       (concatenate 'string (symbol-name name) "-"
                                    (if (or (eq name 'eval_subprogram)
                                            (eq name 'eval_stmt))
                                        "*T1"
                                      "*T"))
                       name))
            (macro-args (macro-args name-old wrld))
            (nonkey-formals (take (- (len macro-args)
                                     (len (member '&key macro-args)))
                                  macro-args))
            (thm
             (return-equiv-thm name-old name-new nonkey-formals nil)))
         (cons thm (eval-return-equiv-thms (cdr names) wrld))))))

  (local
   (defun equals-original-thm (wrld)
     (b* ((main-thms
           (eval-return-equiv-thms *asl-interp-fns* wrld))
          (eval_sub-main-thm
           (return-equiv-thm
            'eval_subprogram-*t 'eval_subprogram-*tt '(env name vparams vargs) nil))
          (eval_stmt-main-thm
           (return-equiv-thm
            'eval_stmt-*t 'eval_stmt-*tt '(env s) nil)))
       `(encapsulate nil
          (local (deflabel before-equals-original))
          (std::defret-mutual *tt-equals-original
            ,eval_sub-main-thm
            ,eval_stmt-main-thm
            . ,main-thms)
          (acl2::def-ruleset! asl-*tt-equals-original-rules
            (set-difference-theories (current-theory :here)
                                     (current-theory 'before-equals-original)))))))

  (local (in-theory (disable cons-equal)))
  ;; ---------------------------------------------------------------------------
  ;; Definition of the Tracing ASL Interpreter (suffixed with *t)
  (with-output
    :off (event)
    (make-event
     (b* ((form *asl-interpreter-mutual-recursion-command*)
          ;; Strip out the events after the /// (theorem about resolved-p-of-resolve-ty)
          (form (strip-post-/// form))
          ;; Strip out xdoc
          (form (strip-xdoc form))
          ;; Add xdoc topic for mutual recursion
          (form (add-mutrec-xdoc '(:parents (asl-tracing)
                                   :short "Modified version of ASL interpreter for a double-tracespec induction scheme") form))
          ;; Add xdoc topic for each function
          (form (add-define-xdoc
                 "Double tracespec induction version of @(see <NAME>)"
                 form))
          ;; Replace '(define eval_subprogram ...' with '(define eval_subprogram-*ft1'
          ;; since it's going to be wrapped in a call that deals with collecting the trace data.
          (form (find-def-and-rename 'eval_subprogram '*tt form))
          (form (find-def-and-rename 'eval_stmt '*tt form))
          ;; Substitute function names with their -*t suffixed forms.
          (form (sublis *eval-trace-substitution-tt* form))
          ;; Replace all invocations of (global-env->static (env->global env)) with the variable static-env.
          (form (replace-static-envs form))
          ;; Add guard saying static-env equals the one in env.
          (form (add-define-guard '(equal (global-env->static (env->global env)) static-env) form))
          ;; Wrap each define body in a call of evbody-*t.
          (form (wrap-define-bodies 'evbody-*t form))
          (form (wrap-define-bodies 'bind-env-with-static form))
          ;; Add (trace asl-tracelist-p) to all the :returns forms.
          (form (add-trace-to-returns form))
          ;; Replace all invocations of (global-env->static (env->global env)) with the variable static-env.
          ;; (form (replace-static-envs form))
          ;; Add the tracespec formal to each define form.
          (form (add-define-formals '(((static-env static_env_global-p) 'static-env)
                                      ((tracespec tracespec-p) 'tracespec)
                                      ((tracespec2 tracespec-p) 'tracespec2)) form))
          ;; Add the definition of eval_subprogram-*t which wraps around eval_subprogram-*t1.
          (form (add-define-to-defines *eval_subprogram-*tt-def* form))
          (form (add-define-to-defines *eval_stmt-*tt-def* form))
          (form (defines-add-flag form))
          ;; Disable the functions, prove the non-trace return values equal to the originals, and verify guards.
          (form (insert-after-///
                 (list
                  '(make-event
                    `(in-theory (disable . ,(fgetprop 'eval_expr-*tt-fn 'acl2::recursivep nil (w state)))))
                  (equals-original-thm (w state))
                  ;; '(verify-guards eval_expr-*t-fn)
                  )
                 form))
          )
       `(progn (defconst *asl-interpreter-mutual-recursion-*tt-form* ',form)
               ,form)))))


;; (local
;;  (defthm stmt-tracespec-subsetp-of-empty-transient
;;    (implies (stmt-tracespec-subsetp s)
;;             (STMT-TRACESPEC-SUBSETP
;;              s
;;           :TRACESPEC (TRACESPEC (TRACESPEC->STMT-SPECS-PERMANENT TRACESPEC)
;;                                 (TRACESPEC->CALL-SPECS-PERMANENT TRACESPEC)
;;                                 NIL NIL)
;;           :TRACESPEC2 (TRACESPEC (TRACESPEC->STMT-SPECS-PERMANENT TRACESPEC2)
;;                                  (TRACESPEC->CALL-SPECS-PERMANENT TRACESPEC2)
;;                                  NIL NIL)))
;;    :hints(("Goal" :in-theory (enable stmt-tracespec-subsetp)))))


;; (local
;;  (defthm trace-subset-fnname-p-of-empty-transient
;;    (implies (trace-subset-fnname-p name)
;;             (TRACE-SUBSET-FNNAME-P
;;           NAME
;;           :TRACESPEC (TRACESPEC (TRACESPEC->STMT-SPECS-PERMANENT TRACESPEC)
;;                                 (TRACESPEC->CALL-SPECS-PERMANENT TRACESPEC)
;;                                 NIL NIL)
;;           :TRACESPEC2 (TRACESPEC (TRACESPEC->STMT-SPECS-PERMANENT TRACESPEC2)
;;                                  (TRACESPEC->CALL-SPECS-PERMANENT TRACESPEC2)
;;                                  NIL NIL)))
;;    :hints(("Goal" :in-theory (enable trace-subset-fnname-p)))))


(defthm trace-subset-fnname-p-of-call-interior-tracespec
    (implies (and (trace-subset-fnname-p name)
                  (call-sig-tracespec-subsetp y))
             (b* (((call-sig y))
                  (entry1 (find-call-tracespec y.fn y.pos tracespec))
                  (entry2 (find-call-tracespec y.fn y.pos tracespec2))
                  (tracespec (call-interior-tracespec entry1 tracespec))
                  (tracespec2 (call-interior-tracespec entry2 tracespec2)))
               (trace-subset-fnname-p name)))
    :hints(("Goal" :in-theory (enable trace-subset-fnname-p
                                      TRACED-CALLSIGS-FNNAMES-IN-TERMS-OF-ALL-CALLSIGS
                                      TRACED-CALLSIGS-FNNAME-IN-TERMS-OF-ALL-CALLSIGS
                                      traced-stmts-fnnames-in-terms-of-all-stmts
                                      traced-stmts-fnname-in-terms-of-all-stmts))))

(defthm trace-subset-fnname-p-of-call-interior-tracespec-special
  (implies (and (trace-subset-fnname-p name)
                (bind-free '((pos . pos)) (pos))
                (call-sig-tracespec-subsetp (call-sig name pos))
                (equal entry1 (find-call-tracespec name pos tracespec))
                (equal entry2 (find-call-tracespec name pos tracespec2)))
           (b* ((tracespec (call-interior-tracespec entry1 tracespec))
                (tracespec2 (call-interior-tracespec entry2 tracespec2)))
             (trace-subset-fnname-p name)))
  :hints (("goal" :use ((:instance trace-subset-fnname-p-of-call-interior-tracespec
                         (y (call-sig name pos))))
           :in-theory (disable trace-subset-fnname-p-of-call-interior-tracespec))))


(defthm trace-subset-ty-timeframe-imap-p-of-call-interior-tracespec
    (implies (and (trace-subset-ty-timeframe-imap-p map)
                  (call-sig-tracespec-subsetp y))
             (b* (((call-sig y))
                  (entry1 (find-call-tracespec y.fn y.pos tracespec))
                  (entry2 (find-call-tracespec y.fn y.pos tracespec2))
                  (tracespec (call-interior-tracespec entry1 tracespec))
                  (tracespec2 (call-interior-tracespec entry2 tracespec2)))
               (trace-subset-ty-timeframe-imap-p map)))
    :hints(("Goal" :in-theory (enable trace-subset-ty-timeframe-imap-p
                                      TRACED-CALLSIGS-FNNAMES-IN-TERMS-OF-ALL-CALLSIGS
                                      TRACED-CALLSIGS-FNNAME-IN-TERMS-OF-ALL-CALLSIGS
                                      traced-stmts-fnnames-in-terms-of-all-stmts
                                      traced-stmts-fnname-in-terms-of-all-stmts))))

(defthm trace-subset-ty-timeframe-imap-p-of-call-interior-tracespec-special
  (implies (and (trace-subset-ty-timeframe-imap-p map)
                (call-sig-tracespec-subsetp (call-sig fn pos))
                (equal entry1 (find-call-tracespec fn pos tracespec)))
           (b* ((entry2 (find-call-tracespec fn pos tracespec2))
                (tracespec (call-interior-tracespec entry1 tracespec))
                (tracespec2 (call-interior-tracespec entry2 tracespec2)))
             (trace-subset-ty-timeframe-imap-p map)))
  :hints (("goal" :use ((:instance trace-subset-ty-timeframe-imap-p-of-call-interior-tracespec
                         (y (call-sig fn pos))))
           :in-theory (disable trace-subset-ty-timeframe-imap-p-of-call-interior-tracespec))))

(defthm trace-subset-ty-timeframe-imap-p-of-call-interior-tracespec-special2
  (implies (and (trace-subset-ty-timeframe-imap-p map)
                (call-sig-tracespec-subsetp (call-sig fn pos))
                (equal entry2 (find-call-tracespec fn pos tracespec2)))
           (b* ((entry1 (find-call-tracespec fn pos tracespec))
                (tracespec (call-interior-tracespec entry1 tracespec))
                (tracespec2 (call-interior-tracespec entry2 tracespec2)))
             (trace-subset-ty-timeframe-imap-p map)))
  :hints (("goal" :use ((:instance trace-subset-ty-timeframe-imap-p-of-call-interior-tracespec
                         (y (call-sig fn pos))))
           :in-theory (disable trace-subset-ty-timeframe-imap-p-of-call-interior-tracespec))))

(defthm trace-subset-ty-timeframe-imap-p-of-call-interior-tracespec-nil
  (implies (and (trace-subset-ty-timeframe-imap-p map))
           (b* ((tracespec (call-interior-tracespec nil tracespec))
                (tracespec2 (call-interior-tracespec nil tracespec2)))
             (trace-subset-ty-timeframe-imap-p map)))
  :hints(("Goal" :in-theory (enable trace-subset-ty-timeframe-imap-p
                                    TRACED-CALLSIGS-FNNAMES-IN-TERMS-OF-ALL-CALLSIGS
                                    TRACED-CALLSIGS-FNNAME-IN-TERMS-OF-ALL-CALLSIGS
                                    traced-stmts-fnnames-in-terms-of-all-stmts
                                    traced-stmts-fnname-in-terms-of-all-stmts))))



(defthm trace-subset-ty-timeframe-imap-p-of-stmt-interior-tracespec
    (implies (and (trace-subset-ty-timeframe-imap-p map)
                  (stmt-tracespec-subsetp y))
             (b* ((entry1 (find-stmt-tracespec y tracespec))
                  (entry2 (find-stmt-tracespec y tracespec2))
                  (tracespec (stmt-interior-tracespec entry1 tracespec))
                  (tracespec2 (stmt-interior-tracespec entry2 tracespec2)))
               (trace-subset-ty-timeframe-imap-p map)))
    :hints(("Goal" :in-theory (enable trace-subset-ty-timeframe-imap-p
                                      TRACED-CALLSIGS-FNNAMES-IN-TERMS-OF-ALL-CALLSIGS
                                      TRACED-CALLSIGS-FNNAME-IN-TERMS-OF-ALL-CALLSIGS
                                      traced-stmts-fnnames-in-terms-of-all-stmts
                                      traced-stmts-fnname-in-terms-of-all-stmts))))


(defthm trace-subset-stmt-p-of-stmt-interior-tracespec-special
  (implies (trace-subset-stmt-p s)
           (b* ((entry1 (find-stmt-tracespec s tracespec))
                (entry2 (find-stmt-tracespec s tracespec2))
                (tracespec (stmt-interior-tracespec entry1 tracespec))
                (tracespec2 (stmt-interior-tracespec entry2 tracespec2)))
             (trace-subset-stmt-p s)))
  :hints(("Goal" :in-theory (enable trace-subset-stmt-p
                                      TRACED-CALLSIGS-FNNAMES-IN-TERMS-OF-ALL-CALLSIGS
                                      TRACED-CALLSIGS-FNNAME-IN-TERMS-OF-ALL-CALLSIGS
                                      traced-stmts-fnnames-in-terms-of-all-stmts
                                      traced-stmts-fnname-in-terms-of-all-stmts)
          :expand ((all-stmts-stmt s)))))

;; (defthm trace-subset-ty-timeframe-imap-p-of-stmt-interior-tracespec-special
;;   (implies (and (trace-subset-ty-timeframe-imap-p map)
;;                 (stmt-tracespec-subsetp (call-sig fn pos))
;;                 (equal entry1 (find-stmt-tracespec fn pos tracespec)))
;;            (b* ((entry2 (find-stmt-tracespec fn pos tracespec2))
;;                 (tracespec (stmt-interior-tracespec entry1 tracespec))
;;                 (tracespec2 (stmt-interior-tracespec entry2 tracespec2)))
;;              (trace-subset-ty-timeframe-imap-p map)))
;;   :hints (("goal" :use ((:instance trace-subset-ty-timeframe-imap-p-of-stmt-interior-tracespec
;;                          (y (call-sig fn pos))))
;;            :in-theory (disable trace-subset-ty-timeframe-imap-p-of-stmt-interior-tracespec))))

;; (defthm trace-subset-ty-timeframe-imap-p-of-stmt-interior-tracespec-special2
;;   (implies (and (trace-subset-ty-timeframe-imap-p map)
;;                 (stmt-tracespec-subsetp (call-sig fn pos))
;;                 (equal entry2 (find-stmt-tracespec fn pos tracespec2)))
;;            (b* ((entry1 (find-stmt-tracespec fn pos tracespec))
;;                 (tracespec (stmt-interior-tracespec entry1 tracespec))
;;                 (tracespec2 (stmt-interior-tracespec entry2 tracespec2)))
;;              (trace-subset-ty-timeframe-imap-p map)))
;;   :hints (("goal" :use ((:instance trace-subset-ty-timeframe-imap-p-of-stmt-interior-tracespec
;;                          (y (call-sig fn pos))))
;;            :in-theory (disable trace-subset-ty-timeframe-imap-p-of-stmt-interior-tracespec))))

;; (defthm trace-subset-ty-timeframe-imap-p-of-stmt-interior-tracespec-nil
;;   (implies (and (trace-subset-ty-timeframe-imap-p map))
;;            (b* ((tracespec (stmt-interior-tracespec nil tracespec))
;;                 (tracespec2 (stmt-interior-tracespec nil tracespec2)))
;;              (trace-subset-ty-timeframe-imap-p map)))
;;   :hints(("Goal" :in-theory (enable trace-subset-ty-timeframe-imap-p
;;                                     TRACED-CALLSIGS-FNNAMES-IN-TERMS-OF-ALL-CALLSIGS
;;                                     TRACED-CALLSIGS-FNNAME-IN-TERMS-OF-ALL-CALLSIGS
;;                                     traced-stmts-fnnames-in-terms-of-all-stmts
;;                                     traced-stmts-fnname-in-terms-of-all-stmts))))


(defthm find-trace-subset-of-rev
  (iff (find-trace-subset x (acl2::rev y))
       (find-trace-subset x y))
  :hints(("Goal" :in-theory (enable acl2::rev)
          :induct t
          :expand ((find-trace-subset x y)
                   (find-trace-subset x nil)
                   (:free (y z) (find-trace-subset x (cons y z)))))))

(defthm tracelist-subsetp-of-wrap-calltrace
  (implies (tracelist-subsetp tr1 tr2)
           (tracelist-subsetp tr1 (list (calltrace
                                         name fn params args (acl2::rev tr2) result pos))))
  :hints (("goal" :induct (len tr1)
           :in-theory (enable asl-trace->subtraces)
           :expand ((tracelist-subsetp tr1 (list (calltrace
                                                  name fn params args (acl2::rev tr2) result pos)))
                    (tracelist-subsetp tr1 tr2)
                    (:free (a b c) (find-trace-subset a (cons b c)))))))

(defthm tracelist-subsetp-of-wrap-stmttrace
  (implies (tracelist-subsetp tr1 tr2)
           (tracelist-subsetp tr1 (list (stmttrace
                                     name s initvars (acl2::rev tr2) result finalvars))))
  :hints (("goal" :induct (len tr1)
           :in-theory (enable asl-trace->subtraces)
           :expand ((tracelist-subsetp tr1 (list (stmttrace
                                                  name s initvars (acl2::rev tr2) result finalvars)))
                    (tracelist-subsetp tr1 tr2)
                    (:free (a b c) (find-trace-subset a (cons b c)))))))

(defthm tracelist-subsetp-of-rev-1
  (iff (tracelist-subsetp (acl2::rev x) y)
       (tracelist-subsetp x y))
  :hints(("Goal" :in-theory (enable acl2::rev
                                    tracelist-subsetp))))

(defthm tracelist-subsetp-of-non-cons
  (implies (not (consp y))
           (iff (tracelist-subsetp x y)
                (not (consp x))))
  :hints(("Goal" :expand ((tracelist-subsetp x y)
                          (find-trace-subset (car x) y)))))

(defthm tracelist-subsetp-of-rev-2
  (iff (tracelist-subsetp x (acl2::rev y))
       (tracelist-subsetp x y))
  :hints(("Goal" :in-theory (enable tracelist-subsetp)
          :induct (len x))))

(defthm tracelist-subsetp-of-wrap-calltrace-both
  (implies (tracelist-subsetp tr1 tr2)
           (tracelist-subsetp (list (calltrace
                                     name fn params args (acl2::rev tr1) result pos))
                              (list (calltrace
                                     name fn params args (acl2::rev tr2) result pos))))
  :hints (("goal" 
           :in-theory (enable asl-trace->subtraces)
           :expand ((:free (x y) (tracelist-subsetp (list x) (list y)))
                    (:free (x y) (find-trace-subset x (list y)))
                    (:free (x y) (trace-subsetp x y))))))


(defthm tracelist-subsetp-of-wrap-stmttrace-both
  (implies (tracelist-subsetp tr1 tr2)
           (tracelist-subsetp (list (stmttrace
                                     name s initvars (acl2::rev tr1) result finalvars))
                              (list (stmttrace
                                     name s initvars (acl2::rev tr2) result finalvars))))
  :hints (("goal" 
           :in-theory (enable asl-trace->subtraces)
           :expand ((:free (x y) (tracelist-subsetp (list x) (list y)))
                    (:free (x y) (find-trace-subset x (list y)))
                    (:free (x y) (trace-subsetp x y))))))


(defthm call-sig-tracespec-subsetp-implies-find
  (implies (and (CALL-SIG-TRACESPEC-SUBSETP (CALL-SIG NAME POS))
                (find-call-tracespec name pos tracespec))
           (equal (find-call-tracespec name pos tracespec2)
                  (find-call-tracespec name pos tracespec)))
  :hints(("Goal" :in-theory (enable call-sig-tracespec-subsetp
                                    find-call-tracespec))))


(defthm tracelist-subsetp-of-call-trace-output
  (implies (and (CALL-SIG-TRACESPEC-SUBSETP (CALL-SIG NAME POS))
                (equal entry1 (find-call-tracespec name pos tracespec))
                (equal entry2 (find-call-tracespec name pos tracespec2))
                (tracelist-subsetp trace1 trace2))
           (tracelist-subsetp
            (call-trace-output
             entry1 name vparams vargs pos res trace1)
            (call-trace-output
             entry2 name vparams vargs pos res trace2)))
  :hints(("Goal" :in-theory (enable call-sig-tracespec-subsetp
                                    find-call-tracespec
                                    call-trace-output))))

(defthm tracelist-subsetp-of-stmt-trace-output
  (implies (and (STMT-TRACESPEC-SUBSETP s)
                (equal entry1 (find-stmt-tracespec s tracespec))
                (equal entry2 (find-stmt-tracespec s tracespec2))
                (tracelist-subsetp trace1 trace2))
           (tracelist-subsetp
            (stmt-trace-output
             entry1 env s res trace1)
            (stmt-trace-output
             entry2 env s res trace2)))
  :hints(("Goal" :in-theory (enable stmt-tracespec-subsetp
                                    find-stmt-tracespec
                                    stmt-trace-output))))


(defthm call-abort-after-when-call-sig-tracespec-subsetp
  (implies (and (call-sig-tracespec-subsetp (call-sig name pos))
                (call-abort-after (find-call-tracespec name pos tracespec) res))
           (call-abort-after (find-call-tracespec name pos tracespec2) res))
  :hints(("Goal" :in-theory (enable call-abort-after
                                    call-sig-tracespec-subsetp
                                    find-call-tracespec))))

(defthm stmt-abort-after-when-stmt-tracespec-subsetp
  (implies (and (stmt-tracespec-subsetp s)
                (stmt-abort-after (find-stmt-tracespec s tracespec) res))
           (stmt-abort-after (find-stmt-tracespec s tracespec2) res))
  :hints(("Goal" :in-theory (enable stmt-abort-after
                                    stmt-tracespec-subsetp
                                    find-stmt-tracespec))))

(defthm maybe-stmt-abort-before-when-stmt-tracespec-subsetp
  (implies (and (stmt-tracespec-subsetp s)
                (equal (maybe-stmt-tracespec->abort (find-stmt-tracespec s tracespec)) :before))
           (equal (maybe-stmt-tracespec->abort (find-stmt-tracespec s tracespec2)) :before))
  :hints(("Goal" :in-theory (enable stmt-tracespec-subsetp
                                    find-stmt-tracespec))))
           

(local (in-theory (disable floor loghead mod xor not cons-equal
                           ;; equal-of-ev_normal
                           ;; equal-of-ev_error
                           ;; equal-of-ev_throwing
                           (tau-system)
                           true-listp
                           consp-under-iff
                           acl2::append-when-not-consp
                           append
                           tracelist-subsetp-when-subset 
                           default-car default-cdr
                           tracelist-subsetp-of-non-cons
                           global-replace-static-with-self
                           len
                           (:t eval_result-kind)
                           acl2::true-listp-append
                           (:t append)
                           (:t val-kind)
                           (:t interp-result-trace-subsetp)
                           (:t pass-error-*t)
                           (:t trace-subset-expr-p)
                           (:t expr_desc-kind)
                           (:t trace-subset-ty-timeframe-imap-p)
                           (:t eval_expr-*t)
                           (:t tracelist-subsetp)
                           acl2::list-fix-when-true-listp
                           (:t trace-free-ty-timeframe-imap-p)
                           acl2::list-fix-when-len-zero
                           eval_expr-*t-no-trace-when-trace-free
                           hons-assoc-equal
                           set::sets-are-true-lists-cheap
                           
                           env-replace-static-with-self)))

;; (local (in-theory (acl2::disable* asl-*t-result-rws)))

(with-output
  ;; makes it so it won't take forever to print the induction scheme
  :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
  :off (event ;; prove
        )

  (std::defret-mutual-generate <fn>-no-trace-when-trace-subset
    :mutual-recursion asl-interpreter-mutual-recursion-*t
    :formal-hyps (((expr-p x) (trace-subset-expr-p x))
                  ((int_constraintlist-p x) (trace-subset-int_constraintlist-p x))
                  ((constraint_kind-p x) (trace-subset-constraint_kind-p x))
                  ((tylist-p x) (trace-subset-tylist-p x))
                  ((typed_identifierlist-p x) (trace-subset-typed_identifierlist-p x))
                  ((ty-p x) (trace-subset-ty-p x))
                  ((pattern-p x) (trace-subset-pattern-p x))
                  ((patternlist-p x) (trace-subset-patternlist-p x))
                  ((exprlist-p x) (trace-subset-exprlist-p x))
                  ((lexpr-p x) (trace-subset-lexpr-p x))
                  ((lexprlist-p x) (trace-subset-lexprlist-p x))
                  ((maybe-expr-p x) (trace-subset-maybe-expr-p x))
                  ((stmt-p x) (trace-subset-stmt-p x))
                  ((catcherlist-p x) (trace-subset-catcherlist-p x))
                  ((maybe-stmt-p x) (trace-subset-maybe-stmt-p x))
                  ((slice-p x) (trace-subset-slice-p x))
                  ((slicelist-p x) (trace-subset-slicelist-p x)))
                
    :rules ((t
             (:add-bindings (((mv result2 & trace2)
                              (let ((tracespec tracespec2)) <call>))))
             (:add-concl (interp-result-trace-subsetp res trace result2 trace2))
             (:add-hyp (trace-subset-ty-timeframe-imap-p
                        (static_env_global->declared_types static-env))))
            ((:fnname is_val_of_type-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (trace-subset-ty-p ty))
                                              (:free (tracespec2) (trace-subset-type_desc-p (ty->desc ty)))
                                              (:free (tracespec2) (TRACE-SUBSET-CONSTRAINT_KIND-P (T_INT->CONSTRAINT (TY->DESC TY)))))))))
            ((:fnname check_int_constraints-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (TRACE-SUBSET-INT_CONSTRAINTLIST-P CONSTRS))
                                              (:free (tracespec2) (TRACE-SUBSET-INT_CONSTRAINT-P (CAR CONSTRS))))))))
            ((:fnname is_val_of_type_tuple-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (trace-subset-tylist-p types)))))))
            ((:fnname eval_slice_list-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (trace-subset-slicelist-p sl)))))))
            ((:fnname eval_slice-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (trace-subset-slice-p s)))))))
            ((:fnname eval_catchers-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (TRACE-SUBSET-MAYBE-STMT-P OTHERWISE)))))))
            ((:fnname eval_stmt-*t1)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (TRACE-SUBSET-stmt-p s))
                                              (:free (tracespec2) (trace-subset-stmt_desc-p (stmt->desc s)))
                                              (:free (tracespec2) (TRACE-SUBSET-MAYBE-EXPR-P (S_RETURN->EXPR (STMT->DESC S))))
                                              (:free (tracespec2) (TRACE-SUBSET-MAYBE-EXPR-P (S_DECL->EXPR (STMT->DESC S))))
                                              (:free (tracespec2) (TRACE-SUBSET-EXPR-P (S_RETURN->EXPR (STMT->DESC S))))
                                              (:free (tracespec2) (TRACE-SUBSET-EXPR_DESC-P (EXPR->DESC (S_RETURN->EXPR (STMT->DESC S)))))
                                              (:free (tracespec2) (TRACE-SUBSET-CALL-P (S_CALL->CALL (STMT->DESC S)))))))))
            ((:fnname eval_limit-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (TRACE-SUBSET-MAYBE-EXPR-P X)))))))
            ((:fnname eval_lexpr_list-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (TRACE-SUBSET-lexprlist-p lx)))))))
            ((:fnname eval_lexpr-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (TRACE-SUBSET-lexpr-p lx))
                                              (:free (tracespec2) (TRACE-SUBSET-lexpr_desc-p (lexpr->desc lx))))))))
            ((:fnname eval_call-*t)
             (:add-hyp (and (trace-subset-fnname-p name)
                            (call-sig-tracespec-subsetp (call-sig name pos)))))
            ((:fnname eval_subprogram-*t)
             (:add-hyp (and (trace-subset-fnname-p name)
                            (call-sig-tracespec-subsetp (call-sig name pos)))))
            ((:fnname eval_subprogram-*t1)
             (:add-hyp (trace-subset-fnname-p name)))
            ((:fnname eval_expr_list-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (trace-subset-exprlist-p e)))))))
            ((or (:fnname eval_pattern-any-*t)
                 (:fnname eval_pattern_tuple-*t))
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (trace-subset-patternlist-p p)))))))
            ((:fnname eval_pattern-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (trace-subset-pattern-p p))
                                              (:free (tracespec2) (trace-subset-pattern_desc-p (pattern->desc p))))))))
            ((:fnname resolve-ty-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (trace-subset-ty-p x))
                                              (:free (tracespec2) (trace-subset-type_desc-p (ty->desc x)))
                                              (:free (tracespec2) (TRACE-SUBSET-ARRAY_INDEX-P (T_ARRAY->INDEX (TY->DESC X)))))))))
            ((:fnname resolve-typed_identifierlist-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (trace-subset-typed_identifierlist-p x))
                                              (:free (tracespec2) (TRACE-SUBSET-TYPED_IDENTIFIER-P (CAR X))))))))
            ((:fnname resolve-tylist-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (trace-subset-tylist-p x)))))))
            ((:fnname resolve-constraint_kind-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (trace-subset-constraint_kind-p x)))))))
            ((:fnname resolve-int_constraints-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (trace-subset-int_constraintlist-p x))
                                              (:free (tracespec2) (trace-subset-int_constraint-p (car x))))))))
            ((:fnname eval_expr-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (trace-subset-expr-p e))
                                              (:free (tracespec2) (trace-subset-expr_desc-p (expr->desc e)))
                                              (:free (tracespec2) (TRACE-SUBSET-CALL-P (E_CALL->CALL (EXPR->DESC E)))))
                                     ;; :use ((:instance trace-subset-fnname-p-when-trace-subset-callexpr-p-special
                                     ;;        (x e)))
                                     ))))
            ((:fnname eval_stmt-*t)
             (:add-keyword :hints ('(:expand ((:free (tracespec2) (trace-subset-stmt-p s))
                                              (:free (tracespec2) (trace-subset-stmt_desc-p (stmt->desc s)))
                                              (:free (tracespec2) (TRACE-SUBSET-CALL-P (s_CALL->CALL (stmt->DESC s))))))))))
    :hints (("Goal" :induct
             (asl-interpreter-mutual-recursion-*tt-flag
              FLAG VAL P E PARAMS ARGS POS
              NAME VPARAMS VARGS LX CATCHERS OTHERWISE
              THROW BACKTRACE S SL INDEX_NAME
              V_START DIR V_END IS_WHILE LIMIT E_COND
              BODY X VALS TYPES I CONSTRS ENV V TY CLK
              ORAC STATIC-ENV TRACESPEC TRACESPEC2))
            (and stable-under-simplificationp
                 '(:in-theory (acl2::e/d*
                               (interp-result-trace-subsetp
                                asl-*t-equals-original-rules
                                eval_stmt-*t-result-rw
                                eval_subprogram-*t-result-rw
                                eval_subprogram-*t1-result-rw
                                ))
                   :do-not '(generalize)))

            
            (vl::big-mutrec-default-hint 'eval_expr-*t-fn id t world)
            (let ((flagval (flag::find-flag-is-hyp clause)))
              (and flagval
                   (progn$
                    (cw "~x0~%" flagval)
                    '(:no-op t))))
            )))

