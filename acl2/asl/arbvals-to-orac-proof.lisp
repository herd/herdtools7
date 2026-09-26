;;****************************************************************************;;
;;                                ASLRef                                      ;;
;;****************************************************************************;;
;; SPDX-FileCopyrightText: Copyright 2025 Arm Limited and/or its affiliates <open-source-office@arm.com>
;; SPDX-License-Identifier: BSD-3-Clause

(in-package "ASL")

(include-book "arbvals-to-orac")
(local (std::add-default-post-define-hook :fix))


(local (in-theory (acl2::disable* asl-*ato-equals-*a-rules
                                  (tau-system))))

(defun ato-append-induct (vals1 types1)
  (if (consp types1)
      (ato-append-induct (cdr vals1) (cdr types1))
    vals1))

(defthm tuple-type-satisfied-of-append-*ato
  (implies (and (vallist-p vals1)
                (tylist-p types1)
                (tuple-type-satisfied vals1 types1)
                (tuple-type-satisfied vals2 types2))
           (tuple-type-satisfied (append vals1 vals2)
                                 (append types1 types2)))
  :hints (("Goal" :induct (ato-append-induct vals1 types1)
           :in-theory (enable tuple-type-satisfied))))

(defthm tuple-type-satisfied-of-arbmap-lookup-*ato
  (implies (ty-satisfiable ty)
           (tuple-type-satisfied (list (arbmap-lookup addr ty arbmap))
                                 (list ty)))
  :hints (("Goal" :in-theory (enable tuple-type-satisfied))))

(defthm arbmap-lookup-under-iff
  (iff (arbmap-lookup addr ty arbmap)
       (ty-satisfiable ty))
  :hints(("Goal" :in-theory (enable arbmap-lookup))))

(with-output
  :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
  (std::defret-mutual-generate <fn>-logs-satisfied
    :rules ((t (:add-concl (tuple-type-satisfied ato-vals ato-types)))
            ((not (:fnname eval_limit-*ato))
             (:add-keyword
              :hints ((and stable-under-simplificationp
                           '(:expand ((:free (pos otherwise is_while clk) <call>)))))))
            ((:fnname eval_limit-*ato)
             (:add-keyword
              :hints ((and stable-under-simplificationp
                           '(:expand ((:free (pos otherwise is_while x) <call>))))))))
    :mutual-recursion asl-interpreter-mutual-recursion-*ato))


(defun ato-call-to-orig-fn (ato-call)
  (declare (xargs :mode :program))
  (b* (((cons ato-fn args) ato-call)
       (name (symbol-name ato-fn))
       (fnp (str::strsuffixp "-FN" name))
       (a-fn (intern-in-package-of-symbol
              (concatenate 'string
                           (subseq name 0 (- (length name) (if fnp 8 5)))
                           (if fnp "-FN" ""))
              ato-fn))
       (new-args (append (set-difference-equal args '(arbaddr arbmap arboffset loop-iter loop-idx))
                         '(orac))))
    (cons a-fn new-args)))

(defmacro ato-call-to-orig (ato-call)
  (ato-call-to-orig-fn ato-call))


(local (include-book "std/lists/append" :dir :system))

(encapsulate nil
  (local (defun my-ind (x y)
           (if (atom y)
               x
             (my-ind (cdr x) (cdr y)))))
  
  (defthm typed-vallist-to-oracle-of-append
    (implies (tuple-type-satisfied vals1 tys1)
             (equal (typed-vallist-to-oracle (append tys1 tys2)
                                             (append vals1 vals2))
                    (append (typed-vallist-to-oracle tys1 vals1)
                            (typed-vallist-to-oracle tys2 vals2))))
    :hints(("Goal" :in-theory (enable typed-vallist-to-oracle append tuple-type-satisfied)
            :induct (my-ind vals1 tys1)))))
                          




;; (local (defthm check_int_constraints-*ato-of-ifix
;;          (equal (check_int_constraints-*ato env (ifix i) constrs)
;;                 (check_int_constraints-*ato env i constrs))
;;          :hints (("goal" :expand ((check_int_constraints-*ato env (ifix i) constrs)
;;                                   (check_int_constraints-*ato env i constrs))))))

(encapsulate nil
  ;; (local (include-book "std/lists/nth" :dir :system))
  ;; (local (include-book "std/lists/update-nth" :dir :system))

  (local (in-theory (disable 
                                 ;; performance optimizing
                                 acl2::prefixp-when-equal-lengths
                                 acl2::prefixp-when-prefixp
                                 ;; acl2::nth-when-too-large-cheap
                                 len not
                                 acl2::len-when-prefixp
                                 acl2::nth-when-prefixp
                                 acl2::prefixp-one-way-or-another
                                 acl2::prefixp-of-append-arg1
                                 acl2::prefixp-transitive
                                 acl2::prefixp-when-not-consp-right)))

  (local
   (defthm my-prefixp-of-append
     (equal (acl2::prefixp (append x y) z)
            (and (acl2::prefixp x z)
                 (acl2::prefixp y (nthcdr (len x) z))))
     :hints(("Goal" :in-theory (enable acl2::prefixp nthcdr len)))))
  
  (local (defthm nthcdr-of-nthcdr
           (equal (nthcdr n (nthcdr m x))
                  (nthcdr (+ (nfix n) (nfix m)) x))))


  (local (defthm oracle-st-of-nthcdr-orac
           (equal (nth 1 (nthcdr-orac n orac))
                  (nthcdr n (nth 1 orac)))
           :hints(("Goal" :in-theory (enable nthcdr-orac)))))

  (local (defthm oracle-mode-of-nthcdr-orac
           (equal (nth 0 (nthcdr-orac n orac))
                  (nth 0 orac))
           :hints(("Goal" :in-theory (enable nthcdr-orac)))))

  (local (defthm redundant-update-nth
           (equal (update-nth n v (update-nth n v1 x))
                  (update-nth n v x))
           :hints(("Goal" :in-theory (enable update-nth)))))
  
  (local (defthm nthcdr-orac-of-nthcdr-orac
           (equal (nthcdr-orac n (nthcdr-orac m orac))
                  (nthcdr-orac (+ (nfix n) (nfix m)) orac))
           :hints(("Goal" :in-theory (enable nthcdr-orac)))))

  (local (defthm nthcdr-orac-of-0
           (equal (nthcdr-orac 0 orac) orac)
           :hints(("Goal" :in-theory (enable nthcdr-orac)))))

  (local (defthm typed-vallist-to-oracle-of-singletons
           (equal (typed-vallist-to-oracle (list ty) (list val))
                  (typed-val-to-oracle ty val))
           :hints (("goal" :expand ((typed-vallist-to-oracle (list ty) (list val)))))))

  (local (defthm typed-val-to-oracle-correct-match
           (implies (and (equal orac-st (acl2::oracle-st orac))
                         (acl2::prefixp (typed-val-to-oracle ty val) orac-st)
                         (equal (acl2::oracle-mode orac) 3)
                         (ty-satisfied val ty))
                    (equal (ty-oracle-val ty orac)
                           (mv (val-fix val)
                               (nthcdr-orac (len (typed-val-to-oracle ty val)) orac))))
           :hints (("goal" :use ((:instance typed-val-to-oracle-correct
                                  (x ty)))
                    :in-theory (disable typed-val-to-oracle-correct)))))

  (local (in-theory (acl2::e/d* ()
                                ;; these congruences each mess up the matching of one of the defun-sks
                                (CHECK_INT_CONSTRAINTS-FN-OF-IFIX-I
                                 CHECK_INT_CONSTRAINTS-FN-INT-EQUIV-CONGRUENCE-ON-I
                                 EVAL_LOOP-FN-IFF-CONGRUENCE-ON-IS_WHILE
                                 eval_expr-fn-nat-equiv-congruence-on-clk
                                 eval_expr-fn-of-nfix-clk
                                 eval_expr_list-fn-nat-equiv-congruence-on-clk
                                 eval_expr_list-fn-of-nfix-clk
                                 EVAL_LOOP-FN-NAT-EQUIV-CONGRUENCE-ON-CLK
                                 eval_loop-fn-of-nfix-clk
                                 EVAL_BLOCK-FN-NAT-EQUIV-CONGRUENCE-ON-CLK
                                 eval_block-fn-of-nfix-clk
                                 EVAL_CALL-FN-NAT-EQUIV-CONGRUENCE-ON-CLK
                                 eval_call-fn-of-nfix-clk
                                 resolve-int_constraints-fn-posn-equiv-congruence-on-pos
                                 resolve-int_constraints-fn-of-posn-fix-pos

                                 ;; don't expand nth/update-nth
                                 nth update-nth
                                 ;; acl2::update-nth-of-cons
                                 ;; acl2::update-nth-when-zp
                                 ;; acl2::nth-when-zp
                                 nth-add1
                             
                                 ;; avoid case splitting, just need the rules below
                                 nthcdr-orac

                                 nthcdr append
                                 
                                 (:rules-of-class :type-prescription :here)
                                 (:rules-of-class :congruence :here)
                                 (:rules-of-class :rewrite-quoted-constant :here))
                                ((:t len)))))

  (with-output
    :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
    :off (event warning prove)
    :summary-off :all :summary-on (acl2::form time)
    (std::defret-mutual-generate original-replicates-<fn>
      :rules ((t (:add-concl
                  (b* ((orac-prefix (typed-vallist-to-oracle ato-types ato-vals)))
                    (implies (and (equal (acl2::oracle-mode orac) 3)
                                  (acl2::prefixp orac-prefix
                                                 (acl2::oracle-st orac)))
                             (b* (((mv orig-res orig-orac)
                                   (ato-call-to-orig <call>)))
                               (and (equal orig-res res)
                                    (equal orig-orac (nthcdr-orac (len orac-prefix) orac))))))))
              ((not (:fnname eval_limit-*ato))
               (:add-keyword
                :hints ((and stable-under-simplificationp
                             `(:expand (,(car (last clause)))))
                        (and stable-under-simplificationp
                             '(:expand ((:free (pos otherwise is_while clk) <call>)
                                        (:free (pos otherwise is_while clk orac)
                                         (ato-call-to-orig <call>)))
                               :do-not-induct t)))))
              ((:fnname eval_limit-*ato)
               (:add-keyword
                :hints ((and stable-under-simplificationp
                             `(:expand (,(car (last clause)))))
                        (and stable-under-simplificationp
                             '(:expand ((:free (pos otherwise is_while x) <call>)
                                        (:free (pos otherwise is_while x orac)
                                         (ato-call-to-orig <call>)))
                               :do-not-induct t))))))
      :universally-quantify (orac)
      :mutual-recursion asl-interpreter-mutual-recursion-*ato)))
