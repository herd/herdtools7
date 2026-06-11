;;****************************************************************************;;
;;                              ACL2 ASL Library                              ;;
;;****************************************************************************;;
;;
;; SPDX-FileCopyrightText: Copyright 2026 Arm Limited and/or its affiliates <open-source-office@arm.com>
;; SPDX-License-Identifier: BSD-3-Clause
;; 
;; ****************************************************************************;;



(in-package "ASL")
(include-book "centaur/fgl/def-fgl-rewrite" :dir :system)
(include-book "centaur/fgl/annotation" :dir :system)
(include-book "centaur/fgl/ctrex-utils" :dir :system)
(include-book "defs")
(include-book "trace-interp" :dir :acl2asl)



;; ----------------------------------------------------------------------------------
;; Oracle abstraction

;; All we really want to know about the oracle is that every time we read it we
;; get some value satisfying the specified type.  We express the value
;; resulting from an oracle read as ty-fix-val of ty-oracle-val->val and the
;; oracle resulting from some number of possible reads as orac-apply-updates of
;; a list giving possible read types and the conditions under which they were
;; read.

;; We deal with counterexamples a little unconventionally. We express the
;; oracle, for counterexample purposes, as just a list of the values that were
;; actually read, in the order they were read, and override the counterexample
;; execution of the definition used to read from the oracle to just return the
;; car and cdr.


(define ty-oracle-val->val (x orac)
  :non-executable t
  :verify-guards nil
  (mv-nth 0 (ty-oracle-val x orac)))

(define ty-oracle-val->orac (x orac)
  :non-executable t
  :verify-guards nil
  (mv-nth 1 (ty-oracle-val x orac)))

(define fake-ty-oracle-val (ty oracl)
  (declare (ignorable ty))
  :verify-guards nil
  (mv (cdar oracl) (cdr oracl)))

(table fgl::magitastic-ev-definitions
       'ty-oracle-val 
       (list '(x orac)
             '(fake-ty-oracle-val x orac)))

(table fgl::magitastic-ev-definitions
       'ty-oracle-val->orac
       (list '(x orac)
             '(mv-nth 1 (fake-ty-oracle-val x orac))))

(table fgl::magitastic-ev-definitions
       'ty-oracle-val->val
       (list '(x orac)
             '(mv-nth 0 (fake-ty-oracle-val x orac))))




(local (defthm ty-oracle-val-is-mv
         (equal (list (mv-nth 0 (ty-oracle-val x orac))
                      (mv-nth 1 (ty-oracle-val x orac)))
                (ty-oracle-val x orac))
         :hints(("Goal" :expand ((ty-oracle-val x orac))))))

                                 
(fgl::def-fgl-rewrite ty-oracle-val-fgl
  (implies (and ;; (syntaxp (fgl::fgl-object-case x :g-concrete))
                (ty-satisfiable x))
           (equal (ty-oracle-val x orac)
                  (mv (ty-fix-val (ty-oracle-val->val x orac) x)
                      (ty-oracle-val->orac x orac))))
  :hints(("Goal" :in-theory (enable ty-oracle-val->orac
                                    ty-oracle-val->val))))
         

(fgl::remove-fgl-rewrites ty-oracle-val
                          ty-oracle-val->val
                          ty-oracle-val->orac)



(define orac-apply-updates (alist orac)
  :verify-guards nil
  (b* (((when (atom alist)) orac)
       (orac (orac-apply-updates (cdr alist) orac))
       ((list* test ty &) (car alist)))
    (if test (ty-oracle-val->orac ty orac) orac))
  ///
  (fgl::def-fgl-rewrite orac-apply-updates-of-orac-apply-updates
    (equal (orac-apply-updates x (orac-apply-updates y orac))
           (orac-apply-updates (append x y) orac)))

  (defthm orac-apply-updates-of-append
    (equal (orac-apply-updates (append x y) orac)
           (orac-apply-updates x (orac-apply-updates y orac))))

  (fgl::def-fgl-rewrite ty-oracle-val->orac-to-orac-apply-updates
    (equal (ty-oracle-val->orac x orac)
           (orac-apply-updates (list (list* t x (ty-fix-val (ty-oracle-val->val x orac) x))) orac)))

  (fgl::remove-fgl-rewrite orac-apply-updates)

  (defthm orac-apply-updates-of-true-list-fix
    (equal (orac-apply-updates (true-list-fix alist) orac)
           (orac-apply-updates alist orac)))

  (defthm orac-apply-updates-of-nil
    (equal (orac-apply-updates nil orac) orac)))


(define orac-updates-conditionalize (cond alist)
  :verify-guards nil
  (b* (((when (atom alist)) nil)
       ((list* test ty val) (car alist)))
    (cons (list* (and cond test) ty val)
          (orac-updates-conditionalize cond (cdr alist))))
  ///
  (defthm orac-updates-conditionalize-correct
    (equal (orac-apply-updates (orac-updates-conditionalize cond alist) orac)
           (if cond (orac-apply-updates alist orac) orac))
    :hints(("Goal" :in-theory (enable orac-apply-updates)))))


;; (fgl::def-fgl-branch-merge merge-oracle-base
;;   (equal (if test (ty-oracle-val->orac x orac) orac)
;;          (orac-apply-updates (list (cons test x)) orac))
;;   :hints(("Goal" :in-theory (enable orac-apply-updates))))



(fgl::def-fgl-branch-merge merge-orac-updates-with-orac
  (equal (if test (orac-apply-updates alist orac) orac)
         (orac-apply-updates (orac-updates-conditionalize test alist) orac)))

(local (defthm append-of-take-with-suffix
         (implies (acl2::suffixp x y)
                  (equal (append (take (- (len y) (len x)) y) x)
                         y))
         :hints(("Goal" :in-theory (enable acl2::suffixp)))))

(encapsulate nil
  

  (local (defthm orac-apply-updates-of-take
           (implies (acl2::suffixp x y)
                    (equal (orac-apply-updates (take (- (len y) (len x)) y)
                                               (orac-apply-updates x orac))
                           (orac-apply-updates y orac)))
           :hints (("goal" :use ((:instance append-of-take-with-suffix))
                    :in-theory (disable append-of-take-with-suffix)))))

  (local (defthm orac-apply-updates-of-take-id
           (equal (orac-apply-updates (take (len al) al) orac)
                  (orac-apply-updates al orac))
           :hints(("Goal" :in-theory (enable orac-apply-updates)))))
  
  (fgl::def-fgl-branch-merge merge-orac-updates-with-updates
    (equal (if test
               (orac-apply-updates alist1 orac)
             (orac-apply-updates alist2 orac))
           (b* ((suff (common-suffix-binder suff alist1 alist2))
                (alist1-prefix (take (- (len alist1) (len suff)) alist1))
                (alist2-prefix (take (- (len alist2) (len suff)) alist2))
                (new-alist (append (orac-updates-conditionalize test alist1-prefix)
                                   (orac-updates-conditionalize (not test) alist2-prefix)
                                   suff)))
             (orac-apply-updates new-alist orac)))
    :hints(("Goal" :in-theory (enable common-suffix-binder)))))

(fgl::disable-if-merge-args orac-apply-updates)

(fgl::def-fgl-branch-merge merge-orac-updates-with-updates-of-other
  (implies (syntaxp (not (equal orac orac2)))
           (equal
            (if test (orac-apply-updates alist1 orac) (orac-apply-updates alist2 orac2))
            (fgl::if! test (fgl::fgl-hide (orac-apply-updates alist1 orac))
                      (fgl::fgl-hide (orac-apply-updates alist2 orac2)))))
  :hints(("Goal" :in-theory (enable fgl::if!))))

(fgl::def-fgl-branch-merge merge-orac-updates-with-other
  (implies (syntaxp (and (not (equal orac orac2))
                         (fgl::fgl-object-case orac2
                           :g-apply (not (eq orac2.fn 'orac-apply-updates))
                           :otherwise t)))
           (equal
            (if test (orac-apply-updates alist1 orac) orac2)
            (fgl::if! test (fgl::fgl-hide (orac-apply-updates alist1 orac)) orac2)))
  :hints(("Goal" :in-theory (enable fgl::if!))))

;; ----------------------------------------------------------------------------------
;; Counterexample generation for the oracle abstraction

;; In a symbolic simulation, we're going to explore all of the reachable paths
;; and generate symbolic values for any oracle reads anywhere we find one.  In
;; our oracle abstraction, we keep this cheap by blurring things somewhat.  E.g.,
;; if we have:
;; if test1
;;    var1 = arbitrary : bits(5);
;; end;
;; var2 = arbitrary : bits(2);

;; then we only evaluate the assignment to var2 once (merging together the two
;; possible paths that might have gotten us there) and produce a single
;; symbolic value that logically represents two different possible oracle reads
;; of bits(2): one where it's the first oracle read, and another where a read
;; of a bits(5) has been done before. I.e., one is
;; (mv-nth 0 (ty-oracle-val '(:t_bits 2) orac)) and the other is
;; (mv-nth 0 (ty-oracle-val '(:t_bits 2)
;;                          (mv-nth 1 (ty-oracle-val (:t_bits 5) orac)))).

;; Whereas the representation of this value in FGL's abstraction is
;; (ty-oracle-val->val '(:t_bits 2) (orac-apply-updates (list (cons test1
;; '(:t_bits 5))) orac)) The tricky thing for counterexample generation is that
;; we don't know whether a given read of the oracle actually happened in our
;; counterexample. E.g., the FGL representation of the value for the first
;; oracle read was just (ty-oracle-val->val '(:t_bits 5) orac) -- we don't
;; know, looking at this value, whether it will actually be read from the
;; oracle in our given counterexample or not.

;; This makes things tricky because what we'd really like to do is just record
;; the ordered list of objects that will be read from the oracle on the
;; execution path that the counterexample actually takes.

;; If we can ensure that the final oracle (after all possible reads have
;; happened) appears in the bvar-db, then perhaps we can generate a good oracle
;; value from that. To do so, we assume that the top level call is a call of
;; eval_subprogram and we use an annotation trick to run our rule without
;; disturbing the usual evaluation of eval_subprogram.  We integrate with the
;; eval_subprogram-print rule below. We first check that there is no
;; (annotated) call of eval_subprogram outside the current one (i.e. we're
;; rewriting the outermost call). Then we check for an annotation on the
;; current call, make sure it doesn't have a :oracle-saved keyword/value in the
;; annotation, add that annotation around the new call of eval_subprogram.
;; After rewriting eval_subprogram, use a trivial if test to ensures that the
;; bvar-db will have the final oracle expression in it.

;; NOTE: This (and other functions that use annotations) should be placed LAST
;; in the file relative to other rules rewriting eval_subprogram, so that they
;; will be tried first when rewriting an eval_subprogram call.


(defun has-saved-oracle (annot)
  (and (assoc-keyword :oracle-saved annot) t))

(fgl::remove-fgl-rewrite oracle-marker)

;; NOTE: This (and other functions that use annotations) should be placed LAST
;; in the file relative to other rules rewriting eval_subprogram, so that they
;; will be tried first when rewriting an eval_subprogram call. I think this
;; should work in either order with eval_subprogram-print.
(fgl::def-fgl-rewrite save-oracle-on-outermost-eval_subprogram
  (implies (and ;; makes sure this is works only on the outermost call of eval_subprogram
            (not (fgl::syntax-bind prev-call (fgl::interp-st-scan-for-nth-fnsym-occ 0 1 'eval_subprogram-fn 'interp-st)))
                
                ;; (fgl::bind-fn-annotation annot 'eval_subprogram-fn)
                ;; (equal (printed-annotation-index annot) 0)
                ;; (not (has-saved-oracle annot))
                )
           (equal (eval_subprogram env fn params args)
                  (fgl::fgl-prog2
                   (cw "saving oracle after this eval_subprogram~%")
                   (let* ((res ;; (fgl::annotate `(:oracle-saved t . ,annot)
                           (eval_subprogram env fn params args))
                          (orac (mv-nth 1 res))
                          (ignore (and (oracle-marker orac) t)))
                     (declare (ignore ignore))
                     (fgl::fgl-prog2 (cw "saved oracle~%")
                                     res))))))

(fgl::def-fgl-rewrite save-oracle-on-outermost-eval_subprogram-*t
  (implies (and ;; makes sure this is works only on the outermost call of eval_subprogram-*t
            (not (fgl::syntax-bind prev-call (fgl::interp-st-scan-for-nth-fnsym-occ 0 1 'eval_subprogram-*t-fn 'interp-st)))
                
                ;; (fgl::bind-fn-annotation annot 'eval_subprogram-*t-fn)
                ;; (equal (printed-annotation-index annot) 0)
                ;; (not (has-saved-oracle annot))
                )
           (equal (eval_subprogram-*t env fn params args)
                  (fgl::fgl-prog2
                   (cw "saving oracle after this eval_subprogram-*t~%")
                   (let* ((res ;; (fgl::annotate `(:oracle-saved t . ,annot)
                           (eval_subprogram-*t env fn params args))
                          (orac (mv-nth 1 res))
                          (ignore (and (oracle-marker orac) t)))
                     (declare (ignore ignore))
                     (fgl::fgl-prog2 (cw "saved oracle~%")
                                     res))))))
  
;; #|
;; (assign :fgl-trace-rewrites nil)
;; (assign :fgl-trace-rule-alist '(((:formula save-oracle-on-outermost-eval_subprogram))
;;                                 ((:formula eval_subprogram-print))
;; (assign :fgl-trace-evisc-tuple '(nil 5 8 nil))

;; |#


(define orac-updates-to-list (updates acc)
  :verify-guards nil
  (b* (((when (atom updates)) acc)
       ((list* test ?ty val) (car updates)))
    (orac-updates-to-list
     (cdr updates)
     (if test (cons (cons (ty-remove-bitfields ty) ;; this just makes the type smaller to print
                          val) acc) acc))))

(define orac-updates-to-list-top (updates)
  :verify-guards nil
  (orac-updates-to-list updates nil))

(fgl::def-ctrex-rule oracle-marker-ctrex-rule-1
  :match ((val (oracle-marker (orac-apply-updates updates orac))))
  :assigned-var orac
  :assign (orac-updates-to-list-top updates)
  :ruletype :elim)


(defun count-true-cars (pairs acc)
  (if (atom pairs)
      acc
    (count-true-cars (cdr pairs)
                     (if (caar pairs) (+ 1 acc) acc))))

(define update-oracle (rulename ty pairs val oracl)
  (declare (ignorable rulename ty))
  :verify-guards nil
  (update-nth (count-true-cars pairs 0) val oracl))



(fgl::ctrex-no-implicit oracle-marker)
(fgl::ctrex-no-implicit orac-apply-updates)
(fgl::ctrex-no-implicit ty-oracle-val->val)
(fgl::ctrex-no-implicit ty-oracle-val->orac)
