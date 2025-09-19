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

(in-package "CAT")

(include-book "interp-types")
(include-book "tools/templates" :dir :system)
(local (include-book "std/lists/sets" :dir :System))

(local (std::add-default-post-define-hook :fix))


(define evt->loc ((x evt-p))
  :returns (loc acl2::maybe-natp :rule-classes :type-prescription)
  (b* (((evt x)))
    (evttype-case x.data
      :evt-r x.data.addr
      :evt-w x.data.addr
      :otherwise nil)))


(define evtlist-filter-type ((type evtkind-p)
                             (x evtlist-p))
  :returns (new-x evtlist-p)
  (if (atom x)
      nil
    (if (eq (evttype-kind (evt->data (car x))) (evtkind-fix type))
        (cons (evt-fix (car x))
              (evtlist-filter-type type (cdr x)))
      (evtlist-filter-type type (cdr x)))))

(define evtlist-filter-init-writes ((x evtlist-p))
  :returns (new-x evtlist-p)
  (if (atom x)
      nil
    (if (b* (((evt x1) (car x)))
          (evttype-case x1.data
            :evt-w x1.data.initp
            :otherwise nil))
        (cons (evt-fix (car x))
              (evtlist-filter-init-writes (cdr x)))
      (evtlist-filter-init-writes (cdr x)))))




(fty::deflist evtpairlist :elt-type evtpair :true-listp t)

(define pair-evts ((x evtlist-p)
                   (y evtlist-p))
  :guard (eql (len x) (len y))
  :returns (pairlist evtpairlist-p)
  (if (atom x)
      nil
    (cons (evtpair (car x) (car y))
          (pair-evts (cdr x) (cdr y)))))

(define evtlist-id-pairs ((x evtlist-p))
  :returns (pairlist evtpairlist-p)
  (if (atom x)
      nil
    (cons (evtpair (car x) (car x))
          (evtlist-id-pairs (cdr x))))
  ///
  (defret member-of-<fn>
    (iff (member-equal pair pairlist)
         (and (evtpair-p pair)
              (equal (evtpair->from pair) (evtpair->to pair))
              (member-equal (evtpair->from pair) (evtlist-fix x))))))


(defthm relation-p-of-sort-evtpairlist
  (implies (evtpairlist-p x)
           (relation-p (mergesort x)))
  :hints(("Goal" :in-theory (enable mergesort))))




(define empty-relation ()
  :returns (rel relation-p)
  nil)

(define id-relation ((x evtlist-p))
  :returns (rel relation-p)
  (mergesort (evtlist-id-pairs x))
  ///
  (defret in-of-<fn>
    (iff (in pair rel)
         (and (evtpair-p pair)
              (equal (evtpair->from pair) (evtpair->to pair))
              (member-equal (evtpair->from pair) (evtlist-fix x))))))

;; (define cartesian1 ((x evt-p)
;;                                (y evtlist-p))
;;   :returns (pairs evtpairlist-p)
;;   (if (atom y)
;;       nil
;;     (cons (evtpair x (car y))
;;           (cartesian1 x (cdr y))))
;;   ///
;;   (defretd member-of-<fn>
;;     (iff (member-equal pair pairs)
;;          (and (evtpair-p pair)
;;               (Equal (evtpair->from pair) (evt-fix x))
;;               (member-equal (evtpair->to pair) (evtlist-fix y))))))

;; (define cartesian ((x evtlist-p)
;;                    (y evtlist-p))
;;   :returns (rel relation-p)
;;   (if (atom x)
;;       nil
;;     (union (mergesort (cartesian1 (car x) y))
;;            (cartesian (cdr x) y)))
;;   ///
;;   (defretd member-of-<fn>
;;     (iff (in pair rel)
;;          (and (evtpair-p pair)
;;               (member-equal (evtpair->from pair) (evtlist-fix x))
;;               (member-equal (evtpair->to pair) (evtlist-fix y))))
;;     :hints(("Goal" :in-theory (enable member-of-cartesian1)))))

(encapsulate nil
  (local
   (progn
     (defthm member-of-evtlist-fix
       (implies (member-equal k y)
                (member-equal (evt-fix k) (evtlist-fix y))))

     (defthm subsetp-of-evtlist-fix
       (implies (subsetp-equal x y)
                (subsetp-equal (evtlist-fix x) (evtlist-fix y)))
       :hints(("Goal" :in-theory (enable evtlist-fix subsetp-equal))))))

  (defcong acl2::set-equiv acl2::set-equiv (evtlist-fix x) 1
    :hints(("Goal" :in-theory (enable acl2::set-equiv)))))

(defmacro defrelation (name cond)
  (acl2::template-subst
   '(progn
      (define <name>1-product ((x evt-p) (y evtlist-p))
        :returns (pairs evtpairlist-p)
        (if (atom y)
            nil
          (if (b* ((?y (car y)))
                <cond>)
              (cons (evtpair (evt-fix x)
                             (evt-fix (car y)))
                    (<name>1-product x (cdr y)))
            (<name>1-product x (cdr y))))
        ///
        (defretd member-of-<fn>
          (iff (member-equal pair pairs)
               (and (evtpair-p pair)
                    (equal (evtpair->from pair) (evt-fix x))
                    (member-equal (evtpair->to pair) (evtlist-fix y))
                    (b* ((?x (evtpair->from pair))
                         (?y (evtpair->to pair)))
                      <cond>))))

        (defcong acl2::set-equiv acl2::set-equiv (<name>1-product x y) 2
          :hints(("Goal" :in-theory (enable member-of-<name>1-product
                                            acl2::set-unequal-witness-rw)))))

      (define <name>-product ((x evtlist-p) (y evtlist-p))
        :returns (rel relation-p)
        (if (atom x)
            nil
          (union (mergesort (<name>1-product (car x) y))
                 (<name>-product (cdr x) y)))
        ///
        (defretd member-of-<fn>
          (iff (in pair rel)
               (and (evtpair-p pair)
                    (member-equal (evtpair->from pair) (evtlist-fix x))
                    (member-equal (evtpair->to pair) (evtlist-fix y))
                    (b* ((?x (evtpair->from pair))
                         (?y (evtpair->to pair)))
                      <cond>)))
          :hints(("Goal" :in-theory (enable member-of-<name>1-product))))

        (defcong acl2::set-equiv equal (<name>-product x y) 2)

        (defcong acl2::set-equiv equal (<name>-product x y) 1
          :hints(("Goal" :in-theory (enable member-of-<name>-product
                                            set::double-containment-no-backchain-limit
                                            pick-a-point-subset-strategy)))))

      (define <name>-relation ((x evtlist-p))
        :returns (rel relation-p)
        (<name>-product x x)
        ///
        (defret member-of-<fn>
          (iff (in pair rel)
               (and (evtpair-p pair)
                    (member-equal (evtpair->from pair) (evtlist-fix x))
                    (member-equal (evtpair->to pair) (evtlist-fix x))
                    (b* ((?x (evtpair->from pair))
                         (?y (evtpair->to pair)))
                      <cond>)))
          :hints(("Goal" :in-theory (enable member-of-<name>-product))))

        (defcong acl2::set-equiv equal (<name>-relation x) 1)))
   :str-alist `(("<NAME>" . ,(symbol-name name)))
   :atom-alist `((<cond> . ,cond))
   :pkg-sym 'cat-pkg))

(defrelation cartesian t)

(defrelation loc (equal (evt->loc x) (evt->loc y)))

(defrelation ext (not (equal (evt->procid x) (evt->procid y))))

(defrelation po (and (equal (evt->procid x) (evt->procid y))
                     (< (evt->po-index x) (evt->po-index y))))

(defrelation rf (b* (((evt x))
                     ((evt y)))
                  (evttype-case y.data
                         :evt-r (equal y.data.from x.uid)
                         :otherwise nil)))



;; For any pair (dst, dst2) in x, includes (src, dst2) in result.
(define compose1 ((src evt-p)
                  (dst evt-p)
                  (x relation-p))
  :returns (compose evtpairlist-p)
  :measure (acl2-count (relation-fix x))
  (b* ((x (relation-fix x)))
    (if (emptyp x)
        nil
      (if (equal (evt-fix dst) (evtpair->from (head x)))
          (cons (evtpair src (evtpair->to (head x)))
                (compose1 src dst (tail x)))
        (compose1 src dst (tail x)))))
  ///
  (defretd member-of-<fn>
    (iff (member-equal pair compose)
         (and (evtpair-p pair)
              (equal (evtpair->from pair) (evt-fix src))
              (in (evtpair dst (evtpair->to pair))
                  (relation-fix x))))
    :hints(("Goal" :in-theory (enable in)))))

(define compose ((x relation-p)
                 (y relation-p))
  :returns (compose relation-p)
  :measure (acl2-count (relation-fix x))
  :verify-guards nil
  (b* ((x (relation-fix x)))
    (if (emptyp x)
      nil
      (union (mergesort (compose1 (evtpair->from (head x))
                                  (evtpair->to (head x))
                                  y))
             (compose (tail x) y))))
  ///
  (verify-guards compose)
  (defretd member-of-compose-suff
    (implies (and (evtpair-p pair)
                  (in (evtpair (evtpair->from pair) mid) (relation-fix x))
                  (in (evtpair mid (evtpair->to pair)) (relation-fix y)))
             (in pair compose))
    :hints(("Goal" :in-theory (enable member-of-compose1))))

  (defretd member-of-compose-suff-rw
    (implies (and (in (evtpair src mid) (relation-fix x))
                  (in (evtpair mid dst) (relation-fix y)))
             (in (evtpair src dst) compose))
    :hints(("Goal" :in-theory (enable member-of-compose1))))

  (defretd member-of-compose-suff-rw2
    (implies (and (in (evtpair mid dst) (relation-fix y))
                  (in (evtpair src mid) (relation-fix x)))
             (in (evtpair src dst) compose))
    :hints(("Goal" :in-theory (enable member-of-compose-suff-rw)))))

(local (defthm member-equal-when-relation-p
         (implies (relation-p x)
                  (iff (member-equal k x)
                       (in k x)))
         :hints(("Goal" :in-theory (enable set::in-to-member)))))

(local (include-book "std/util/termhints" :dir :system))

(define compose-midpoint ((src evt-p)
                          (dst evt-p)
                          (x relation-p)
                          (y relation-p))
  :returns (mid (iff (evt-p mid) mid))
  :measure (acl2-count (relation-fix x))
  ;; Witness for compose membership. If (src . dst) are in the composition of x
  ;; and y, then (compose-midpoint src dst x y) produces mid such that (src
  ;; . mid) is in x and (mid . dst) is in y.
  (b* ((x (relation-fix x)))
    (if (emptyp x)
        nil
      (if (and (equal (evt-fix src) (evtpair->from (head x)))
               (in (evtpair (evtpair->to (head x)) dst)
                   (relation-fix y)))
          (evt-fix (evtpair->to (head x)))
        (compose-midpoint src dst (tail x) y))))
  ///
  ;; (local (defthm member-cons-relation
  ;;          (implies (And (relation-p y)
  ;;                        (not (and (evt-p dst)
  ;;                                  (evt-p src))))
  ;;                   (not (member-equal (cons src dst) y)))))

  ;; (local (defthm member-relation-not-consp
  ;;          (implies (And (relation-p y)
  ;;                        (not (evtpair-p pair)))
  ;;                   (not (member-equal pair y)))))

  (defret compose-midpoint-when-in-compose
    (implies (in (evtpair src dst) (compose x y))
             mid)
    :hints(("Goal" :in-theory (enable compose
                                      member-of-compose1))))
  
  (defret compose-midpoint-witnesses
    (implies (and (in (evtpair src mid1) (relation-fix x))
                  (in (evtpair mid1 dst) (relation-fix y)))
             (and (in (evtpair src mid) (relation-fix x))
                  (in (evtpair mid dst) (relation-fix y))))
    :hints(("Goal" :in-theory (enable in)
            :cases ((and (evt-p src) (evt-p mid1) (evt-p dst))))
           (and stable-under-simplificationp
                '(:induct (compose-midpoint src dst x y))))
    :otf-flg t)

  (defretd member-of-compose-necc
    :pre-bind ((src (evtpair->from pair))
               (dst (evtpair->to pair)))
    (implies (not (and (in (evtpair (evtpair->from pair) mid) (relation-fix x))
                       (in (evtpair mid (evtpair->to pair)) (relation-fix y))))
             (not (in pair (compose x y))))
    :hints(("Goal" :in-theory (enable compose
                                      in
                                      member-of-compose-suff
                                      member-of-compose1)
            :cases ((and (evt-p (evtpair->from pair)) (evt-p (evtpair->to pair))))))
    :otf-flg t)

  (fty::deffixequiv compose-midpoint)
  
  (defretd member-of-compose-implies-fix
    :pre-bind ((pair (evtpair src dst)))
    (implies (and (in pair (compose x y)))
             (and (in (evtpair src mid) (relation-fix x))
                  (in (evtpair mid dst) (relation-fix y))))
    :hints(("Goal" :use ((:instance member-of-compose-necc
                          (pair (evtpair src dst)))))))
  
  (defretd member-of-compose-implies
    :pre-bind ((pair (evtpair src dst)))
    (implies (and (in pair (compose x y)))
             (and (implies (relation-p x)
                           (in (evtpair src mid) x))
                  (implies (relation-p y)
                           (in (evtpair mid dst) y))))
    :hints(("Goal" :use ((:instance member-of-compose-necc
                          (pair (evtpair src dst)))))))
                         
  (defretd member-of-compose
    :pre-bind ((src (evtpair->from pair))
               (dst (evtpair->to pair)))
    (iff (in pair (compose x y))
         (and (evtpair-p pair)
              (in (evtpair (evtpair->from pair) mid) (relation-fix x))
              (in (evtpair mid (evtpair->to pair)) (relation-fix y))))
    :hints(("Goal" :in-theory (enable member-of-compose-necc
                                      member-of-compose-suff))))

  (defretd member-of-compose-rw
    :pre-bind ((src (evtpair->from pair))
               (dst (evtpair->to pair)))
    (implies (acl2::rewriting-negative-literal `(in ,pair (compose ,x ,y)))
             (iff (in pair (compose x y))
                  (and (evtpair-p pair)
                       (in (evtpair (evtpair->from pair) mid) (relation-fix x))
                       (in (evtpair mid (evtpair->to pair)) (relation-fix y)))))
    :hints(("Goal" :in-theory (enable member-of-compose))))


  (defthm compose-associative
    (equal (compose (compose x y) z)
           (compose x (compose y z)))
    :hints(("Goal" :in-theory (enable pick-a-point-subset-strategy
                                      set::double-containment-no-backchain-limit))
           (SET::PICK-A-POINT-SUBSET-HINT ID acl2::CLAUSE
                                          WORLD STABLE-UNDER-SIMPLIFICATIONP)
           (and stable-under-simplificationp
                (acl2::use-termhint
                 (b* ((elem set::arbitrary-element)
                      ((evtpair elem)))
                   (if (in elem (compose (compose x y) z))
                       (b* ((step2 (compose-midpoint elem.from elem.to (compose x y) z))
                            (step1 (compose-midpoint elem.from step2 x y))
                            (pair1 (evtpair step1 elem.to)))
                         `(:use ((:instance acl2::mark-clause-is-true (x '(in elem (compose (compose x y) z))))
                                 (:instance member-of-compose-suff
                                  (x x) (y (compose y z))
                                  (pair ,(acl2::hq elem))
                                  (mid ,(acl2::hq step1)))
                                 (:instance member-of-compose-suff
                                  (x y) (y z)
                                  (pair ,(acl2::hq pair1))
                                  (mid ,(acl2::hq step2))))
                           :in-theory (enable member-of-compose-rw)))
                     (b* ((step1 (compose-midpoint elem.from elem.to x (compose y z)))
                          (step2 (compose-midpoint step1 elem.to y z))
                          (pair2 (evtpair elem.from step2)))
                       `(:use ((:instance acl2::mark-clause-is-true (x '(in elem (compose x (compose y z)))))
                               (:instance member-of-compose-suff
                                (x (compose x y)) (y z)
                                (pair ,(acl2::hq elem))
                                (mid ,(acl2::hq step2)))
                               (:instance member-of-compose-suff
                                (x x) (y y)
                                (pair ,(acl2::hq pair2))
                                (mid ,(acl2::hq step1))))
                         :in-theory (enable member-of-compose-rw))))))))))



(fty::deflist relationlist :elt-type relation :true-listp t)

(define compose-path-p ((x evtlist-p)
                                 (rels relationlist-p))
  :guard (consp rels)
  (and (consp x)
       (consp (cdr x))
       (in (evtpair (car x) (cadr x)) (relation-fix (car rels)))
       (if (atom (cdr rels))
           (atom (cddr x))
         (compose-path-p (cdr x) (cdr rels)))))
                

(define compose* ((x relationlist-p))
  :guard (consp x)
  :returns (compose relation-p)
  (if (atom (cdr x))
      (relation-fix (car x))
    (compose (car x) (compose* (cdr x))))
  ///
  (defthm in-compose*-when-compose-path
    (implies (compose-path-p evts x)
             (in (evtpair (car evts) (car (last evts)))
                 (compose* x)))
    :hints(("Goal" :in-theory (enable compose-path-p
                                      member-of-compose-suff-rw)
            :induct t))))

(define compose*-path ((src evt-p) (dst evt-p) (x relationlist-p))
  :guard (and (consp x)
              (in (evtpair src dst) (compose* x)))
  :guard-hints (("goal" :Expand ((compose* x))
                 :in-theory (enable member-of-compose-implies)))
  :returns (path evtlist-p)
  (if (atom (cdr x))
      (list (evt-fix src) (evt-fix dst))
    (let ((mid (compose-midpoint src dst (car x) (compose* (cdr x)))))
      (cons (evt-fix src)
            (compose*-path mid dst (cdr x)))))
  ///
  (defret <fn>-endpoints
    (and (equal (car path) (evt-fix src))
         (equal (car (last path)) (evt-fix dst))))
  
  (defret compose-path-p-of-<fn>
    (implies (in (evtpair src dst) (compose* x))
             (compose-path-p path x))
    :hints(("Goal" :in-theory (enable compose-path-p
                                      compose*
                                      member-of-compose-implies
                                      member-of-compose-implies-fix))))

  (defretd in-of-compose*-rw
    :pre-bind ((src (evtpair->from pair))
               (dst (evtpair->to pair)))
    (implies (acl2::rewriting-negative-literal
              `(in ,pair (compose* ,x)))
             (iff (in pair (compose* x))
                  (and (evtpair-p pair)
                       (compose-path-p path x)
                       (equal (car path) src)
                       (equal (car (last path)) dst))))
    :hints (("goal" :use ((:instance in-compose*-when-compose-path
                           (evts (compose*-path (evtpair->from pair)
                                                (evtpair->to pair) x)))))))
  
  (defret len-of-<fn>
    (equal (len path) (+ 1 (max (len x) 1)))))
  




(define domain ((x relation-p))
  :returns (dom evtlist-p)
  :measure (acl2-count (relation-fix x))
  (b* ((x (relation-fix x)))
    (if (emptyp x)
        nil
      (cons (evtpair->from (head x))
            (domain (tail x)))))
  ///
  (defretd member-of-domain-suff
    (implies (and (in (evtpair src dst) (relation-fix x))
                  (evt-p src))
             (member-equal src (domain x)))
    :hints(("Goal" :in-theory (enable in))))

  (defretd member-of-domain-suff-free
    (implies (and (in (evtpair src dst) some-rel)
                  (in (evtpair src dst) (relation-fix x))
                  (evt-p src))
             (member-equal src (domain x)))
    :hints(("Goal" :in-theory (enable member-of-domain-suff))))

  (defret member-from-of-domain
    (implies (in pair (relation-fix x))
             (member-equal (evtpair->from pair) dom))
    :hints(("Goal" :in-theory (enable in)))))

(define domain-witness ((src evt-p) (x relation-p))
  :returns (dst (iff (evt-p dst) dst))
  :measure (acl2-count (relation-fix x))
  (b* ((x (relation-fix x)))
    (if (emptyp x)
        nil
      (if (equal (evt-fix src) (evtpair->from (head x)))
          (evtpair->to (head x))
        (domain-witness src (tail x)))))
  ///

  (defret domain-witness-witnesses
    (implies (in (evtpair src dst1) (relation-fix x))
             (in (evtpair src dst) (relation-fix x)))
    :hints(("Goal" :in-theory (enable in))))
  
  (defretd member-of-domain-necc
    (implies (not (in (evtpair src dst) (relation-fix x)))
             (not (member-equal src (domain x))))
    :hints(("Goal" :in-theory (enable domain in))))

  (defretd member-of-domain
    (iff (member-equal src (domain x))
         (and (evt-p src)
              (in (evtpair src dst) (relation-fix x))))
    :hints(("Goal" :in-theory (enable member-of-domain-necc
                                      member-of-domain-suff
                                      domain)))
    :otf-flg t)

  (defretd member-of-domain-rw
    (implies (acl2::rewriting-negative-literal `(member-equal ,src (domain ,x)))
             (iff (member-equal src (domain x))
                  (and (evt-p src)
                       (in (evtpair src dst) (relation-fix x)))))
    :hints(("Goal" :in-theory (enable member-of-domain))))

  (defthm domain-of-union
    (implies (and (relation-p x)
                  (relation-p y))
             (acl2::set-equiv (domain (union x y))
                              (append (domain x) (domain y))))
    :hints(("Goal" :in-theory (enable acl2::set-unequal-witness-rw
                                      member-of-domain-rw
                                      member-of-domain-suff-free)))))


(define range ((x relation-p))
  :returns (dom evtlist-p)
  :measure (acl2-count (relation-fix x))
  (b* ((x (relation-fix x)))
    (if (emptyp x)
        nil
      (cons (evtpair->to (head x))
            (range (tail x)))))
  ///
  (defretd member-of-range-suff
    (implies (and (in (evtpair src dst) (relation-fix x))
                  (evt-p dst))
             (member-equal dst (range x)))
    :hints(("Goal" :in-theory (enable in))))

  (defretd member-of-range-suff-free
    (implies (and (in (evtpair src dst) some-rel)
                  (in (evtpair src dst) (relation-fix x))
                  (evt-p dst))
             (member-equal dst (range x)))
    :hints(("Goal" :in-theory (enable member-of-range-suff))))

  (defret member-from-of-range
    (implies (in pair (relation-fix x))
             (member-equal (evtpair->to pair) dom))
    :hints(("Goal" :in-theory (enable in)))))

(define range-witness ((dst evt-p) (x relation-p))
  :returns (src (iff (evt-p src) src))
  :measure (acl2-count (relation-fix x))
  (b* ((x (relation-fix x)))
    (if (emptyp x)
        nil
      (if (equal (evt-fix dst) (evtpair->to (head x)))
          (evtpair->from (head x))
        (range-witness dst (tail x)))))
  ///

  (defret range-witness-witnesses
    (implies (in (evtpair src1 dst) (relation-fix x))
             (in (evtpair src dst) (relation-fix x)))
    :hints(("Goal" :in-theory (enable in))))
  
  (defretd member-of-range-necc
    (implies (not (in (evtpair src dst) (relation-fix x)))
             (not (member-equal dst (range x))))
    :hints(("Goal" :in-theory (enable range in))))

  (defretd member-of-range
    (iff (member-equal dst (range x))
         (and (evt-p dst)
              (in (evtpair src dst) (relation-fix x))))
    :hints(("Goal" :in-theory (enable member-of-range-necc
                                      member-of-range-suff
                                      range)))
    :otf-flg t)

  (defretd member-of-range-rw
    (implies (acl2::rewriting-negative-literal `(member-equal ,dst (range ,x)))
             (iff (member-equal dst (range x))
                  (and (evt-p dst)
                       (in (evtpair src dst) (relation-fix x)))))
    :hints(("Goal" :in-theory (enable member-of-range-necc
                                      member-of-range-suff
                                      range)))
    :otf-flg t)

  (defthm range-of-union
    (implies (and (relation-p x)
                  (relation-p y))
             (acl2::set-equiv (range (union x y))
                              (append (range x) (range y))))
    :hints(("Goal" :in-theory (enable acl2::set-unequal-witness-rw
                                      member-of-range-rw
                                      member-of-range-suff-free)))))


(local
 (defsection transitive-closure-termination-argument

   (local (in-theory (enable pick-a-point-subset-strategy
                             set::double-containment-no-backchain-limit)))

   (defthm domain-of-compose
     (subsetp-equal (domain (compose x y))
                    (domain x))
     :hints(("Goal" :in-theory (enable acl2::subsetp-witness-rw
                                       member-of-compose
                                       member-of-domain-rw
                                       member-of-domain-suff))))

   (defthm range-of-compose
     (subsetp-equal (range (compose x y))
                    (range y))
     :hints(("Goal" :in-theory (enable acl2::subsetp-witness-rw
                                       member-of-compose
                                       member-of-range-rw
                                       member-of-range-suff))))

   (defthm subset-of-cartesian
     (implies (relation-p x)
              (subset x (cartesian-relation (append (domain x) (range x)))))
     :hints(("Goal" :in-theory (enable pick-a-point-subset-strategy))))

   (defthm cardinality-limited-by-cartesian
     (<= (cardinality (relation-fix x))
         (cardinality (cartesian-relation (append (domain x) (range x)))))
     :hints (("goal" :use ((:instance subset-of-cartesian
                            (x (relation-fix x))))
              :in-theory (disable subset-of-cartesian)))
     :rule-classes :linear)

   (defthm cardinality-of-union-increasing
     (implies (not (subset y x))
              (< (cardinality x)
                 (cardinality (union x y))))
     :hints (("goal" :use ((:instance set::proper-subset-cardinality
                            (x x) (y (union x y))))
              :in-theory (e/d (set::subset-in)
                              (set::proper-subset-cardinality))))
     :rule-classes :linear)

   (in-theory (disable acl2::commutativity-2-of-append-under-set-equiv
                       acl2::commutativity-of-append-under-set-equiv
                       set::expand-cardinality-of-union))
  
   (defthm append-under-set-equiv-when-subsetp
     (implies (subsetp-equal y x)
              (acl2::set-equiv (append x y) x)))

   (defthm append-under-set-equiv-when-subsetp-2
     (implies (subsetp-equal y x)
              (acl2::set-equiv (append x y z) (append x z)))
     :hints(("Goal" :in-theory (enable acl2::set-unequal-witness-rw))))))



(define transitive-closure ((x relation-p))
  :measure (- (cardinality (cartesian-relation
                            (append (domain x) (range x))))
              (cardinality (relation-fix x)))
  :returns (closure relation-p)
  (b* ((comp (compose x x))
       (x (relation-fix x))
       ((when (subset comp x))
        x))
    (transitive-closure (union x comp)))
  ///

  (defret transitive-closure-is-superset
    (subset (relation-fix x) (transitive-closure x))
    :hints(("Goal" :in-theory (enable set::subset-transitive))))

  (defret transitive-closure-is-closed
    (subset (compose closure closure) closure)))


(define relation-path-p ((path evtlist-p)
                         (x relation-p))
  (if (atom (cdr path))
      nil
    (and (in (evtpair (car path) (cadr path))
             (relation-fix x))
         (or (atom (cddr path))
             (relation-path-p (cdr path) x))))
  ///
  (defthm relation-path-p-of-compose
    (implies (and (relation-path-p a x)
                  (relation-path-p b x)
                  (equal (evt-fix (car b))
                         (evt-fix (car (last a)))))
             (relation-path-p (append a (cdr b)) x)))

  (defthmd relation-path-p-when-subset
    (implies (and (subset (relation-fix x)
                          (relation-fix y))
                  (relation-path-p path x))
             (relation-path-p path y))
    :hints(("Goal" :in-theory (enable relation-path-p
                                      set::subset-in))))
  
  (local (in-theory (enable evtlist-fix))))



(define transitive-path-aux ((path evtlist-p)
                             (x relation-p))
  :guard (relation-path-p path (union (relation-fix x) (compose x x)))
  :guard-hints (("goal" :in-theory (enable relation-path-p)))
  :returns (new-path evtlist-p)
  :ruler-extenders :lambdas
  ;; If path is a path in (union (relation-fix x) (compose x x)),
  ;; we derive a path in x.
  (b* ((first (evt-fix (car path)))
       (second (evt-fix (cadr path)))
       (rest (if (consp (cddr path))
                 (transitive-path-aux (cdr path) x)
               (list second)))
       ((when (in (evtpair first second) (relation-fix x)))
        (cons first rest)))
    (cons first
          (cons (evt-fix (compose-midpoint first second x x))
                rest)))
  ///
  (defret first-of-<fn>
    (equal (car new-path)
           (evt-fix (car path))))

  (defret last-of-<fn>
    (implies (relation-path-p path (union (relation-fix x) (compose x x)))
             (equal (car (last new-path))
                    (evt-fix (car (last path)))))
    :hints(("Goal" :in-theory (disable (:d transitive-path-aux))
            :induct <call>
            :expand (<call>
                     (:free (y) (relation-path-p path y))))))

  (defret relation-path-p-of-<fn>
    (implies (relation-path-p path (union (relation-fix x) (compose x x)))
             (relation-path-p new-path x))
    :hints(("Goal" :in-theory (disable (:d transitive-path-aux))
            :induct <call>
            :expand (<call>
                     (:free (y) (relation-path-p path y))
                     (:free (a b) (relation-path-p (cons a b) x))))
           (and stable-under-simplificationp
                '(:in-theory (enable member-of-compose-rw)))))

  (local (in-theory (enable evtlist-fix))))
       


(define transitive-path ((src evt-p)
                         (dst evt-p)
                         (x relation-p))
  :guard (in (evtpair src dst) (transitive-closure x))
  :returns (path evtlist-p)
  :measure (- (cardinality (cartesian-relation
                            (append (domain x) (range x))))
              (cardinality (relation-fix x)))
  :verify-guards nil
  (b* ((comp (compose x x))
       (x (relation-fix x))
       ((when (subset comp x))
        (list (evt-fix src) (evt-fix dst))))
    (transitive-path-aux (transitive-path src dst (union x comp)) x))
  ///
  (defret first-of-<fn>
    (equal (car path)
           (evt-fix src)))

  (defret transitive-path-correct
    ;; This (along with the first- and last- properties of transitive-path)
    ;; form half of the correctness statement for transitive-closure: If (src,
    ;; dst) are in (transitive-closure x), then there is a path in x from src
    ;; to dst.
    (implies (in (evtpair src dst) (transitive-closure x))
             (relation-path-p path x))
    :hints(("Goal" :in-theory (enable transitive-closure
                                      relation-path-p))))
  
  (defret last-of-<fn>
    (implies (in (evtpair src dst) (transitive-closure x))
             (equal (car (last path))
                    (evt-fix dst)))
    :hints(("Goal" :in-theory (enable transitive-closure))))
  
  (verify-guards transitive-path
    :hints (("goal" :expand ((transitive-closure x))))))
  


(defthmd transitive-when-closed-under-self-composition
  (implies (and (relation-path-p path x)
                (subset (compose x x) (relation-fix x)))
           (in (evtpair (car path)
                        (car (last path)))
               (relation-fix x)))
  :hints(("Goal" :induct (relation-path-p path x)
          :in-theory (enable relation-path-p
                             set::subset-in))
         (and stable-under-simplificationp
              '(:use ((:instance member-of-compose-suff
                       (x x) (y x)
                       (pair (evtpair (car path) (caddr path)))
                       (mid (cadr path)))
                      (:instance member-of-compose-suff
                       (x x) (y x)
                       (pair (evtpair (car path) (car (last (cdr path)))))
                       (mid (cadr path))))))))

(defthm in-transitive-closure-when-path
  ;; The other half of the correctness of transitive-closure: If there is a
  ;; path from a to b in x, then (a, b) are in the transitive closure of x.
  (implies (relation-path-p path x)
           (in (evtpair (car path) (car (last path)))
               (transitive-closure x)))
  :hints(("Goal" :use ((:instance transitive-when-closed-under-self-composition
                        (x (transitive-closure x)))
                       (:instance relation-path-p-when-subset
                        (x x) (y (transitive-closure x))))
          :in-theory (e/d (transitive-closure-is-superset)
                          (relation-path-p-when-subset)))))
                
             
(defsection transitive-closure-correctnes
  (defun-sk exists-path (src dst x)
    (exists path
            (and (relation-path-p path x)
                 (equal src (car path))
                 (equal dst (car (last path))))))

  (in-theory (Disable exists-path))

  (defthmd transitive-closure-correct
    (iff (in pair (transitive-closure x))
         (and (evtpair-p pair)
              (exists-path (evtpair->from pair)
                           (evtpair->to pair)
                           x)))
    :hints ((acl2::use-termhint
             (b* (((evtpair pair)))
               (if (in pair (transitive-closure x))
                   `(:use ((:instance exists-path-suff
                            (path ,(acl2::hq (transitive-path pair.from pair.to
                                                              x)))
                            (src ,(acl2::hq pair.from)) (dst ,(acl2::hq pair.to))))
                     :in-theory (disable exists-path-suff))
                 `(:in-theory (e/d (exists-path)
                                   (in-transitive-closure-when-path))
                   :use ((:instance in-transitive-closure-when-path
                          (path (exists-path-witness
                                 (evtpair->from pair)
                                 (evtpair->to pair) x)))))))))))




(define test-irreflexive ((x relation-p))
  :measure (acl2-count (relation-fix x))
  (b* ((x (relation-fix x)))
    (if (emptyp x)
        t
      (and (b* (((evtpair x1) (head x)))
             (not (equal x1.from x1.to)))
           (test-irreflexive (tail x)))))
  ///
  (defthm test-irreflexive-necc
    (implies (in (evtpair evt evt) (relation-fix x))
             (not (test-irreflexive x)))))

(define test-irreflexive-badguy ((x relation-p))
  :measure (acl2-count (relation-fix x))
  :returns (evt (iff (evt-p evt) evt))
  (b* ((x (relation-fix x)))
    (if (emptyp x)
        nil
      (or (b* (((evtpair x1) (head x)))
             (and (equal x1.from x1.to)
                  x1.from))
          (test-irreflexive-badguy (tail x)))))
  ///
  (local (defthm evtpair-when-from-equal-to
           (b* (((evtpair pair)))
             (implies (equal pair.from pair.to)
                      (equal (evtpair pair.from pair.from)
                             (evtpair-fix pair))))))
  
  (defret test-irreflexive-by-badguy
    (implies (not (in (evtpair evt evt) (relation-fix x)))
             (test-irreflexive x))
    :hints(("Goal" :in-theory (enable test-irreflexive))))

  
  (defret self-pair-exists-when-not-test-irreflexive
    (implies (not (test-irreflexive x))
             (in (evtpair evt evt) (relation-fix x)))
    :hints(("Goal" :in-theory (enable test-irreflexive)))))


(define test-acyclic ((x relation-p))
  (test-irreflexive (transitive-closure x)))


(define inverse ((x relation-p))
  :returns (inv relation-p)
  :measure (acl2-count (relation-fix x))
  :verify-guards nil
  (b* ((x (relation-fix x)))
    (if (emptyp x)
        nil
      (insert (b* (((evtpair x1) (head x)))
                (evtpair x1.to x1.from))
              (inverse (tail x)))))
  ///
  (verify-guards inverse))
