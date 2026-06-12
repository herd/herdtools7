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
(include-book "centaur/fgl/config" :dir :system)
(include-book "centaur/fgl/fgl-object" :dir :system)
(include-book "centaur/fgl/constraint-db" :dir :system)
(include-book "centaur/fgl/ctrex-utils" :dir :system)
(include-book "centaur/fgl/annotation" :dir :system)
(include-book "centaur/fgl/helper-utils" :dir :system)
(include-book "interp" :dir :acl2asl)
(include-book "defs")
(include-book "error-free" :dir :acl2asl)
(local (include-book "std/alists/alist-keys" :dir :system))
(local (include-book "std/lists/sets" :dir :system))
(local (include-book "centaur/bitops/ihsext-basics" :dir :system))
(local (include-book "std/lists/take" :dir :system))
(local (include-book "std/lists/repeat" :dir :system))

;; ----------------------------------------------------------------------------------
;; Abstraction for storage lookups (and maybe record lookups too, but not used for that yet)

;; We express everything in terms of val-imap-lookup, val-imap-has-key,
;; val-imap-put-pairs, and val-imap-add-pairs. Val-imap-lookup always produces
;; a val so it doesn't distinguish between bound and unbound keys (that's what
;; val-imap-has-key is for). Val-imap-put-pairs can't add a new pair to the map
;; (that's what val-imap-add-pairs is for).

;; We have three use cases in mind:

;;  - Global storage, which for proofs starts out as a free variable and only
;;    has puts, not adds, throughout a proof. (Note this assumes we're not
;;    reasoning about initialization/declaration of globals, just running
;;    functions.) This will be represented as:
;;       (val-imap-put-pairs (list of updates) storage)
;;    where whenever we do an update we cons a pair onto the list of updates.
;;
;;  - Local storage frames, which start out empty and can have variables added
;;    over time but usually not a large number of variables. We'll represent this
;;    using (val-imap-add-pairs pairs nil), and an update will modify the pairs
;;    using put-assoc-equal.
;;
;;  - Records/enumarrays.  These have keys determined by their types. We used to
;;    represent these the same way as local storage frames, always fully expanding
;;    them to have explicit key/symbolic value pairs. However, this was expensive
;;    so we're moving to a dual strategy:
;;       * Represent explicitly created values, including all local variables,
;;         using (val-imap-add-pairs pairs nil), i.e. fully elaborated.
;;       * Represent latent values, including theorem variables, global variable initial values,
;;         and arbitrary values using (val-imap-put-pairs pairs initial-rec).

;;

(fgl::remove-fgl-rewrites
 omap::lookup omap::assoc omap::delete omap::update omap::keys omap::key-ord-values
 omap::from-lists* omap::from-lists omap::restrict)

(define val-imap-restrict ((keys identifierlist-p) (x val-imap-p))
  :verify-guards nil
  :returns (map val-imap-p)
  (val-imap-fix (omap::restrict (identifierlist-fix keys) (val-imap-fix x)))
  ///
  (fty::deffixequiv val-imap-restrict)
  (fgl::remove-fgl-rewrite val-imap-restrict)
  (fgl::add-fgl-rewrite val-imap-p-of-val-imap-restrict)
  )

(define-continue val-imap-lookup
  (fty::deffixequiv val-imap-lookup)
  (fgl::remove-fgl-rewrite val-imap-lookup)
  (fgl::add-fgl-rewrite val-p-of-val-imap-lookup)

  ;; (fgl::def-fgl-rewrite val-imap-lookup-of-cons
  ;;   (equal (val-imap-lookup key (cons a b))
  ;;          (if (and (consp a)
  ;;                   (equal (car a) (identifier-fix key)))
  ;;              (val-fix (cdr a))
  ;;            (val-imap-lookup key b))))
  )

(define-continue val-imap-has-key
  (fty::deffixequiv val-imap-has-key)
  (fgl::remove-fgl-rewrite val-imap-has-key)

  ;; (fgl::def-fgl-rewrite val-imap-has-key-of-cons
  ;;   (equal (val-imap-has-key key (cons a b))
  ;;          (or (and (consp a)
  ;;                   (equal (car a) (identifier-fix key)))
  ;;              (val-imap-has-key key b))))
  )


(local
 (encapsulate nil
   (local (defthmd not-equal-cdr-when-not-equal
            (implies (and (consp a) (consp b)
                          (equal (car a) (car b)))
                     (equal (equal (cdr a) (cdr b))
                            (equal a b)))))
   (local (defthm car-of-omap-assoc
            (equal (car (omap::assoc k x))
                   (and (omap::assoc k x) k))
            :hints(("Goal" :in-theory (enable omap::assoc)))))

   (defthmd val-imap-lookup-diff-key-when-unequal
     (implies (not (equal (val-imap-fix x) (val-imap-fix y)))
              (let ((key (omap::diff-key (val-imap-fix x) (val-imap-fix y))))
                (not (and (iff (val-imap-has-key key x) (val-imap-has-key key y))
                          (equal (val-imap-lookup key x) (val-imap-lookup key y))))))
     :hints(("Goal" :in-theory (enable val-imap-has-key val-imap-lookup omap::lookup
                                       not-equal-cdr-when-not-equal)
             :use ((:instance omap::diff-key-when-unequal
                    (x (val-imap-fix x)) (y (val-imap-fix y))))
             :cases (;; (identifier-p (omap::diff-key (val-imap-fix x) (val-imap-fix y)))
                     (OMAP::ASSOC (OMAP::DIFF-KEY (VAL-IMAP-FIX X)
                                                  (VAL-IMAP-FIX Y))
                                  (VAL-IMAP-FIX X)))
             :do-not-induct t))
     :otf-flg t)))



(fgl::add-fgl-rewrite val-imap-fix-when-val-imap-p)
(fgl::add-fgl-rewrite val-imap-p-of-val-imap-fix)

;; (local (defthm val-imap-fix-of-put-assoc-equal
;;          (equal (val-imap-fix (put-assoc-equal k v x))
;;                 (if (identifier-p k)
;;                     (put-assoc-equal k (val-fix v) (val-imap-fix x))
;;                   (val-imap-fix x)))
;;          :hints(("Goal" :in-theory (enable val-imap-fix
;;                                            put-assoc-equal)))))


;; (local (defthm val-imap-fix-of-put-bad-key
;;          (implies (not (identifier-p k))
;;                   (equal (val-imap-fix (put-assoc-equal k v x))
;;                          (val-imap-fix x)))
;;          :hints(("Goal" :in-theory (enable val-imap-fix put-assoc-equal)))))

;; (local (defthm val-imap-fix-of-put-assoc-equal-val-imap-fix
;;          (equal (val-imap-fix (put-assoc-equal id val (val-imap-fix x)))
;;                 (val-imap-fix (put-assoc-equal id val x)))
;;          :hints(("Goal" :in-theory (enable put-assoc-equal val-imap-fix)))))

;; (local (defthm val-imap-fix-put-assoc-equal-val-fix
;;          (equal (val-imap-fix (put-assoc-equal key (val-fix val) x))
;;                 (val-imap-fix (put-assoc-equal key val x)))))


;; (local (defthm put-assoc-equal-swap
;;          (implies (and (or (hons-assoc-equal k1 x)
;;                            (hons-assoc-equal k2 x))
;;                        (not (equal k1 k2)))
;;                   (equal (put-assoc-equal k2 v2 (put-assoc-equal k1 v1 x))
;;                          (put-assoc-equal k1 v1 (put-assoc-equal k2 v2 x))))
;;          :hints(("Goal" :in-theory (enable put-assoc-equal
;;                                            hons-assoc-equal)))))

;; (local (defthm val-imap-p-implies-alistp
;;          (implies (val-imap-p x) (alistp x))))

(define-continue val-imap-put
  (local (in-theory (enable omap::lookup)))
  (fgl::remove-fgl-rewrite val-imap-put)
  (fgl::add-fgl-rewrite val-imap-p-of-val-imap-put)

  (defthm val-imap-lookup-of-val-imap-put
    (equal (val-imap-lookup k1 (val-imap-put k2 val x))
           (if (and (identifier-equiv k1 k2)
                    (val-imap-has-key k2 x))
               (val-fix val)
             (val-imap-lookup k1 x)))
    :hints(("Goal" :in-theory (enable val-imap-lookup
                                      val-imap-has-key))))

  (defthm val-imap-has-key-of-val-imap-put
    (iff (val-imap-has-key k1 (val-imap-put k2 val x))
         (val-imap-has-key k1 x))
    :hints(("Goal" :in-theory (enable val-imap-has-key))))

  (defthm val-imap-put-same
    (equal (val-imap-put k v1 (val-imap-put k v2 x))
           (val-imap-put k v1 x))
    :hints(("Goal" :in-theory (enable val-imap-fix
                                      val-imap-has-key))))

  (defthm val-imap-put-diff
    (implies (and (not (identifier-equiv k1 k2)))
             (equal (val-imap-put k2 v2 (val-imap-put k1 v1 x))
                    (val-imap-put k1 v1 (val-imap-put k2 v2 x))))
    :hints(("Goal" :in-theory (enable val-imap-fix
                                      val-imap-has-key))))

  (defthm val-imap-put-redundant
    (implies (and (val-imap-has-key k x)
                  (equal v (val-imap-lookup k x)))
             (equal (val-imap-put k v x)
                    (val-imap-fix x)))
    :hints(("Goal" :in-theory (enable val-imap-has-key
                                      val-imap-lookup
                                      acl2::assoc-is-hons-assoc-equal-when-key-nonnil))))

  (defthm val-imap-put-non-key
    (implies (not (val-imap-has-key k x))
             (equal (val-imap-put k v x)
                    (val-imap-fix x))))

  (fty::deffixequiv val-imap-put)

  (fgl::disable-execution val-imap-put))


(define-continue val-imap-add
  (local (in-theory (enable omap::lookup)))
  (fgl::remove-fgl-rewrite val-imap-add)
  (fgl::add-fgl-rewrite val-imap-p-of-val-imap-add)

  (defthm val-imap-lookup-of-val-imap-add
    (equal (val-imap-lookup k1 (val-imap-add k2 val x))
           (if (identifier-equiv k1 k2)
               (val-fix val)
             (val-imap-lookup k1 x)))
    :hints(("Goal" :in-theory (enable val-imap-lookup))))

  (defthm val-imap-has-key-of-val-imap-add
    (iff (val-imap-has-key k1 (val-imap-add k2 val x))
         (or (identifier-equiv k1 k2)
             (val-imap-has-key k1 x)))
    :hints(("Goal" :in-theory (enable val-imap-has-key))))

  (defthm val-imap-add-of-val-imap-put
    (implies (not (identifier-equiv k1 k2))
             (equal (val-imap-add k1 v1 (val-imap-put k2 v2 x))
                    (val-imap-put k2 v2 (val-imap-add k1 v1 x))))
    :hints(("Goal" :in-theory (enable val-imap-put
                                      val-imap-has-key))))

  (fty::deffixequiv val-imap-add)

  (fgl::disable-execution val-imap-add))

(define-continue val-imap-add-pairs
  (fgl::remove-fgl-rewrite val-imap-add-pairs)
  (fgl::add-fgl-rewrite val-imap-p-of-val-imap-add-pairs)

  (defthm val-imap-lookup-of-val-imap-add-pairs
    (equal (val-imap-lookup k (val-imap-add-pairs pairs x))
           (let ((look (hons-assoc-equal (identifier-fix k) pairs)))
             (if look
                 (val-fix (cdr look))
               (val-imap-lookup k x)))))
  (fgl::add-fgl-rewrite val-imap-lookup-of-val-imap-add-pairs)

  (defthm val-imap-has-key-of-val-imap-add-pairs
    (iff (val-imap-has-key k (val-imap-add-pairs pairs x))
         (or (hons-assoc-equal (identifier-fix k) pairs)
             (val-imap-has-key k x))))
  (fgl::add-fgl-rewrite val-imap-has-key-of-val-imap-add-pairs)

  (defthm val-imap-add-pairs-nest
    (equal (val-imap-add-pairs pairs1 (val-imap-add-pairs pairs2 x))
           (val-imap-add-pairs (append pairs1 pairs2) x)))

  (fgl::add-fgl-rewrite val-imap-add-pairs-nest)

  (fgl::def-fgl-rewrite val-imap-add-to-add-pairs
    (equal (val-imap-add k v x)
           (val-imap-add-pairs (list (cons (identifier-fix k) v)) x)))

  (fgl::disable-execution val-imap-add-pairs)

  (defthm val-imap-add-pairs-of-val-alist-fix-pairs
    (equal (val-imap-add-pairs (val-alist-fix pairs) x)
           (val-imap-add-pairs pairs x))
    :hints(("Goal" :in-theory (enable val-alist-fix)))))

(local (defthm atom-cdr-last
         (not (consp (cdr (last x))))
         :rule-classes :type-prescription))

(define-continue val-imap-put-pairs
  (fgl::remove-fgl-rewrite val-imap-put-pairs)
  (fgl::add-fgl-rewrite val-imap-p-of-val-imap-put-pairs)

  (defthmd assoc-of-val-imap-put-pairs
    (equal (omap::assoc k (val-imap-put-pairs pairs x))
           (and (identifier-p k)
                (let* ((x-look (omap::assoc k (val-imap-fix x))))
                  (and x-look
                       (let* ((pairs-look (hons-assoc-equal k pairs)))
                         (if pairs-look
                             (cons k (val-fix (cdr pairs-look)))
                           (cons k (val-fix (cdr x-look)))))))))
    :hints(("Goal" :in-theory (enable val-imap-put
                                      val-imap-has-key))))

  (defthmd assoc-of-val-imap-add-pairs
    (equal (omap::assoc k (val-imap-add-pairs pairs x))
           (and (identifier-p k)
                (let* ((pairs-look (hons-assoc-equal k pairs)))
                  (if pairs-look
                      (cons k (val-fix (cdr pairs-look)))
                    (let ((x-look (omap::assoc k (val-imap-fix x))))
                      (and x-look (cons k (val-fix (cdr x-look)))))))))
    :hints(("Goal" :in-theory (enable val-imap-add
                                      val-imap-add-pairs
                                      val-imap-has-key))))

  (defthm val-imap-has-key-of-val-imap-put-pairs
    (iff (val-imap-has-key k (val-imap-put-pairs pairs x))
         (val-imap-has-key k x)))
  (fgl::add-fgl-rewrite val-imap-has-key-of-val-imap-put-pairs)

  (defthm val-imap-lookup-of-val-imap-put-pairs
    (equal (val-imap-lookup k (val-imap-put-pairs pairs x))
           (let ((look (hons-assoc-equal (identifier-fix k) pairs)))
             (if (and look
                      (val-imap-has-key k x))
                 (val-fix (cdr look))
               (val-imap-lookup k x))))
    :hints(("Goal" :in-theory (enable hons-assoc-equal))))
  (fgl::add-fgl-rewrite val-imap-lookup-of-val-imap-put-pairs)

  (defthm val-imap-put-pairs-of-true-list-fix
    (equal (val-imap-put-pairs (true-list-fix pairs) x)
           (val-imap-put-pairs pairs x)))

  (defthm val-imap-put-pairs-of-nil
    (equal (val-imap-put-pairs nil x)
           (val-imap-fix x)))
  (fgl::add-fgl-rewrite val-imap-put-pairs-of-nil)

  (defthm val-imap-put-pairs-nest
    (equal (val-imap-put-pairs pairs1 (val-imap-put-pairs pairs2 x))
           (val-imap-put-pairs (append pairs1 pairs2) x)))
  (fgl::add-fgl-rewrite val-imap-put-pairs-nest)

  (fty::deffixequiv val-imap-put-pairs)
  (fgl::add-fgl-rewrite val-imap-put-pairs-of-val-imap-fix-x)

  (fgl::def-fgl-rewrite val-imap-put-to-put-pairs
    (equal (val-imap-put k v x)
           (val-imap-put-pairs (list (cons (identifier-fix k) v)) x)))

  (fgl::def-fgl-rewrite val-imap-put-pairs-of-add-pairs
    (implies (and (identifier-p key)
                  (hons-assoc-equal key pairs))
             (equal (val-imap-put-pairs (list (cons key val)) (val-imap-add-pairs pairs x))
                    (val-imap-add-pairs (acl2::put-assoc-equal key val pairs) x)))
    :hints(("Goal" :in-theory (enable val-imap-add-pairs
                                      val-imap-put
                                      val-imap-add
                                      val-imap-fix
                                      val-imap-has-key
                                      assoc-of-val-imap-add-pairs))))

  (defthm val-imap-put-of-put-pairs-of-put
    (equal (val-imap-put k v (val-imap-put-pairs lst (val-imap-put k v2 x)))
           (val-imap-put k v (val-imap-put-pairs lst x)))
    :hints (("goal" :induct t)
            (and stable-under-simplificationp
                 '(:cases ((identifier-equiv k (caar lst)))))))

  (defthm val-imap-put-pairs-of-put
    (equal (val-imap-put-pairs lst (val-imap-put k v x))
           (if (hons-assoc-equal (identifier-fix k) lst)
               (val-imap-put-pairs lst x)
             (val-imap-put k v (val-imap-put-pairs lst x))))
    :hints(("Goal" :in-theory (enable hons-assoc-equal))))


  (defthm val-imap-put-pairs-of-fast-alist-fork
    (equal (val-imap-put-pairs (fast-alist-fork a b) x)
           (val-imap-put-pairs b (val-imap-put-pairs a x)))
    :hints(("Goal" :in-theory (e/d (fast-alist-fork)
                                   (val-imap-put-pairs-nest))
            :induct (fast-alist-fork a b))))

  (defthm val-imap-put-pairs-of-fast-alist-clean
    (equal (val-imap-put-pairs (fast-alist-clean lst) x)
           (val-imap-put-pairs lst x)))

  (fgl::disable-execution val-imap-put-pairs)

  (defthm keys-of-val-imap-put-pairs
    (equal (omap::keys (val-imap-put-pairs lst x))
           (omap::keys (val-imap-fix x)))
    :hints(("Goal" :in-theory (enable val-imap-put
                                      val-imap-has-key
                                      omap::in-of-keys-to-assoc))))

  (local
   (defthmd val-imap-put-pairs-rewrite
     (implies (not (hons-assoc-equal (caar pairs) (cdr pairs)))
              (equal (val-imap-put-pairs pairs x)
                     (val-imap-put-pairs
                      (cdr pairs)
                      (val-imap-put-pairs (list (car pairs)) x))))
     :hints (("Goal"
              :use ((:instance val-imap-put-pairs-of-fast-alist-fork
                               (a (list (car pairs)))
                               (b (cdr pairs))))
              :in-theory (disable val-imap-put-pairs-of-fast-alist-fork)))))

  (defthmd val-imap-put-pairs-of-add-pairs-2
    (implies (and (identifier-p (caar pairs1))
                  (hons-assoc-equal (caar pairs1) pairs2)
                  (not (hons-assoc-equal (caar pairs1) (cdr pairs1))))
             (equal (val-imap-put-pairs pairs1 (val-imap-add-pairs pairs2 x))
                    (val-imap-put-pairs
                     (cdr pairs1)
                     (val-imap-add-pairs (put-assoc-equal (caar pairs1)
                                                          (cdar pairs1)
                                                          pairs2)
                                         x))))
    :hints (("Goal" :use ((:instance val-imap-put-pairs-rewrite
                                     (pairs pairs1)
                                     (x (val-imap-add-pairs pairs2 x)))
                          (:instance val-imap-put-pairs-of-add-pairs
                                     (key (caar pairs1))
                                     (val (cdar pairs1))
                                     (pairs pairs2))))))
  )

(define val-imap-pairs-conditionalize (test pairs (x val-imap-p))
  :returns (new-pairs)
  :verify-guards nil
  (b* (((when (atom pairs)) nil)
       (rest (val-imap-pairs-conditionalize test (cdr pairs) x))
       ((unless (and (consp (car pairs))
                     (identifier-p (caar pairs))
                     (val-imap-has-key (caar pairs) x)))
        rest))
    (cons (cons (caar pairs)
                (if test (val-fix (cdar pairs))
                  (val-imap-lookup (caar pairs) x)))
          rest))
  ///
  (defret hons-assoc-equal-of-<fn>
    (equal (hons-assoc-equal k new-pairs)
           (and (identifier-p k)
                (val-imap-has-key k x)
                (let ((look (hons-assoc-equal k pairs)))
                  (and look
                       (cons k (if test (val-fix (cdr look))
                                 (val-imap-lookup k x))))))))

  (defret <fn>-correct
    (equal (val-imap-put-pairs
            (val-imap-pairs-conditionalize test pairs x) x)
           (if test
               (val-imap-put-pairs pairs x)
             (val-imap-fix x)))
    :hints(("Goal" :in-theory (enable val-imap-put-pairs))))

  (defthm val-imap-pairs-conditionalize-of-true-list-fix
    (equal (val-imap-pairs-conditionalize test (true-list-fix pairs) x)
           (val-imap-pairs-conditionalize test pairs x)))

  (fty::deffixequiv val-imap-pairs-conditionalize))



(defcong iff iff (fgl::annotate annot val) 2)
(fgl::add-fgl-congruence iff-implies-iff-annotate-2)

(fgl::def-fgl-rewrite val-imap-has-key-debug
  (implies (and (fgl::bind-fn-annotation annot 'val-imap-has-key :ignore-fns '(if))
                (not (assoc-keyword :debug annot)))
           (equal (val-imap-has-key key x)
                  (b* ((res (fgl::annotate '(:debug t) ;; (let ((res (val-imap-has-key key x))) (fgl::fgl-progn (fgl::syntax-interp "after val-imap-has-key~%") res))
                                           (and (val-imap-has-key key x) t)))
                       ((when (fgl::syntax-bind concr (fgl::fgl-object-case res :g-concrete)))
                        res))
                    (fgl::fgl-prog2
                     (fgl::fgl-error :msg (msg "non-concrete val-imap-has-key ~x0 ~x1 ~x2" key x res))
                     res)))))



(fgl::def-fgl-branch-merge merge-val-imap-put-pairs-1
  (implies (val-imap-p x)
           (equal (if test (val-imap-put-pairs pairs x) x)
                  (val-imap-put-pairs
                   (val-imap-pairs-conditionalize test (fast-alist-clean pairs)
                                                  ;; pairs
                                                  x) x))))


(local (defthmd append-take-suffix
         (implies (and (acl2::suffixp suff lst)
                       (equal len (- (len lst) (len suff))))
                  (equal (append (take len lst) suff) lst))
         :hints(("Goal" :in-theory (enable acl2::suffixp)))))




(define val-imap-pairs-conditionalize2 (keys test then-pairs else-pairs (x val-imap-p))
  :returns (new-pairs)
  :verify-guards nil
  (b* (((when (atom keys)) nil)
       (key (car keys))
       ((unless (and (identifier-p key)
                     (val-imap-has-key key x)))
        (val-imap-pairs-conditionalize2 (cdr keys) test then-pairs else-pairs x))
       (val (if test
                (let ((look (hons-get key then-pairs)))
                  (if look
                      (val-fix (cdr look))
                    (val-imap-lookup key x)))
              (let ((look (hons-get key else-pairs)))
                  (if look
                      (val-fix (cdr look))
                    (val-imap-lookup key x))))))
    (cons (cons key val)
          (val-imap-pairs-conditionalize2 (cdr keys) test then-pairs else-pairs x)))
  ///
  (defret hons-assoc-equal-of-<fn>
    (equal (hons-assoc-equal k new-pairs)
           (and (identifier-p k)
                (member-equal k keys)
                (val-imap-has-key k x)
                (let* ((pairs (if test then-pairs else-pairs))
                       (look (hons-assoc-equal k pairs)))
                  (cons k
                        (if look
                            (val-fix (cdr look))
                          (val-imap-lookup k x)))))))

  (defret <fn>-correct
    (equal (val-imap-put-pairs
            (val-imap-pairs-conditionalize2 keys test then-pairs else-pairs x) x)
           (val-imap-put-pairs (acl2::fal-extract keys (if test then-pairs else-pairs)) x))
    :hints(("Goal" :in-theory (enable val-imap-put-pairs))))

  (fty::deffixequiv val-imap-pairs-conditionalize2))

(local (in-theory (disable fast-alist-clean
                           acl2::hons-union)))

(local (include-book "centaur/misc/hons-sets" :dir :system))

(local (defthm val-imap-put-pairs-of-fal-extract-superset
         (implies (subsetp-equal (acl2::alist-keys pairs) (double-rewrite keys))
                  (equal (val-imap-put-pairs (acl2::fal-extract keys pairs) x)
                         (val-imap-put-pairs pairs x)))
         :hints (("goal" :use ((:instance val-imap-lookup-diff-key-when-unequal
                                (x (val-imap-put-pairs (acl2::fal-extract keys pairs) x))
                                (y (val-imap-put-pairs pairs x))))))))

(fgl::def-fgl-branch-merge merge-val-imap-put-pairs-2
  (equal (if test (val-imap-put-pairs pairs1 x)
           (val-imap-put-pairs pairs2 x))
         (b* ((suff (common-suffix-binder suff pairs1 pairs2))
              (suff-len (len suff))
              (base (val-imap-put-pairs suff x))
              (pairs1-prefix (make-fast-alist (fast-alist-clean (take (- (len pairs1) suff-len) pairs1))))
              (pairs2-prefix (make-fast-alist (fast-alist-clean (take (- (len pairs2) suff-len) pairs2))))
              (keys (acl2::hons-union (acl2::alist-keys pairs1-prefix) (acl2::alist-keys pairs2-prefix)))
              (new-pairs (val-imap-pairs-conditionalize2 keys test pairs1-prefix pairs2-prefix base)))
           (val-imap-put-pairs new-pairs base)))
  :hints(("Goal" :in-theory (e/d (common-suffix-binder)
                                 (val-imap-put-pairs-nest)))
         (and stable-under-simplificationp
              '(:in-theory (enable val-imap-put-pairs-nest
                                   append-take-suffix)))))


(define val-imap-pairs-merge (keys test pairs1 pairs2)
  ;; Merge pairs1 with pairs2 assuming same alist keys
  :verify-guards nil
  :returns (new-pairs)
  (if (atom keys)
      nil
    (cons (cons (car keys)
                (if test
                    (cdr (hons-get (car keys) pairs1))
                  (cdr (hons-get (car keys) pairs2))))
          (val-imap-pairs-merge (cdr keys) test pairs1 pairs2)))
  ///

  (defret hons-assoc-equal-of-<fn>
    (equal (hons-assoc-equal k new-pairs)
           (and (member-equal k keys)
                (cons k (if test
                            (cdr (hons-assoc-equal k pairs1))
                          (cdr (hons-assoc-equal k pairs2)))))))

  (defthm val-imap-add-pairs-of-merge-1
    (implies (and (equal (mergesort (acl2::alist-keys pairs1))
                         (mergesort (acl2::alist-keys pairs2)))
                  test)
             (equal (val-imap-add-pairs
                     (val-imap-pairs-merge (mergesort (acl2::alist-keys pairs1))
                                           test pairs1 pairs2)
                     nil)
                    (val-imap-add-pairs pairs1 nil)))
    :hints (("goal" :use ((:instance omap::diff-key-when-unequal
                           (x (val-imap-add-pairs
                               (val-imap-pairs-merge (mergesort (acl2::alist-keys pairs1))
                                                     test pairs1 pairs2)
                               nil))
                           (y (val-imap-add-pairs pairs1 nil))))
             :in-theory (enable assoc-of-val-imap-add-pairs))))

  (defthm val-imap-add-pairs-of-merge-2
    (implies (and (equal keys ;; (mergesort (acl2::alist-keys pairs1))
                         (mergesort (acl2::alist-keys pairs2)))
                  (not test))
             (equal (val-imap-add-pairs
                     (val-imap-pairs-merge keys
                                           test pairs1 pairs2)
                     nil)
                    (val-imap-add-pairs pairs2 nil)))
    :hints (("goal" :use ((:instance omap::diff-key-when-unequal
                           (x (val-imap-add-pairs
                               (val-imap-pairs-merge (mergesort (acl2::alist-keys pairs2))
                                                     test pairs1 pairs2)
                               nil))
                           (y (val-imap-add-pairs pairs2 nil))))
             :in-theory (enable assoc-of-val-imap-add-pairs)))))

(define val-imap-pairs-merge-add/put (keys test
                                           pairs1 ;; added
                                           pairs2 ;; put
                                           base2) ;; base of put
  ;; Merge (val-imap-add-pairs pairs1 nil)
  ;; with  (val-imap-put-pairs pairs2 base2)
  :verify-guards nil
  :returns (new-pairs)
  (if (atom keys)
      nil
    (cons (cons (car keys)
                (if test
                    (cdr (hons-get (car keys) pairs1))
                  (let ((look2 (hons-get (car keys) pairs2)))
                    (if look2
                        (cdr look2)
                      (val-imap-lookup (car keys) base2)))))
          (val-imap-pairs-merge-add/put (cdr keys) test pairs1 pairs2 base2)))
  ///

  (defret hons-assoc-equal-of-<fn>
    (equal (hons-assoc-equal k new-pairs)
           (and (member-equal k keys)
                (cons k (if test
                            (cdr (hons-assoc-equal k pairs1))
                          (let ((look2 (hons-assoc-equal k pairs2)))
                            (if look2
                                (cdr look2)
                              (val-imap-lookup k base2)))))))
    :hints (("goal" :induct t
             :in-theory (disable (:d val-imap-pairs-merge-add/put))
             :expand ((:free (test) <call>)))))

  (defthm val-imap-add-pairs-of-merge-add/put-1
    (implies test
             (equal (val-imap-add-pairs
                     (val-imap-pairs-merge-add/put (mergesort (acl2::alist-keys pairs1))
                                                   test pairs1 pairs2 base2)
                     nil)
                    (val-imap-add-pairs pairs1 nil)))
    :hints (("goal" :use ((:instance omap::diff-key-when-unequal
                           (x (val-imap-add-pairs
                               (val-imap-pairs-merge-add/put (mergesort (acl2::alist-keys pairs1))
                                                     test pairs1 pairs2 base2)
                               nil))
                           (y (val-imap-add-pairs pairs1 nil))))
             :in-theory (enable assoc-of-val-imap-add-pairs))))

  (defthm val-imap-add-pairs-of-merge-add/put-2
    (implies (and (equal keys ;; (mergesort (acl2::alist-keys pairs1))
                         (val-imap-keys base2))
                  (not test))
             (equal (val-imap-add-pairs
                     (val-imap-pairs-merge-add/put keys
                                           test pairs1 pairs2 base2)
                     nil)
                    (val-imap-put-pairs pairs2 base2)))
    :hints (("goal" :use ((:instance omap::diff-key-when-unequal
                           (x (val-imap-add-pairs
                               (val-imap-pairs-merge-add/put (val-imap-keys base2)
                                                     test pairs1 pairs2 base2)
                               nil))
                           (y (val-imap-put-pairs pairs2 base2))))
             :in-theory (enable assoc-of-val-imap-add-pairs
                                assoc-of-val-imap-put-pairs
                                val-imap-has-key
                                val-imap-lookup
                                omap::lookup)))))

(fgl::def-fgl-branch-merge merge-val-imap-add-pairs
  (equal (if test (val-imap-add-pairs pairs1 nil)
           (val-imap-add-pairs pairs2 nil))
         (b* ((keys1 (acl2::alist-keys pairs1))
              ((unless (fgl::syntax-bind keys1-conc (fgl::fgl-object-case keys1 :g-concrete)))
               (fgl::fgl-prog2
                (fgl::syntax-interp (cw "Abort merge-val-imap-add-pairs: non-concrete keys1~%"))
                (fgl::abort-rewrite (if test (val-imap-add-pairs pairs1 nil)
                                      (val-imap-add-pairs pairs2 nil)))))
              (keys2 (acl2::alist-keys pairs2))
              ((unless (fgl::syntax-bind keys2-conc (fgl::fgl-object-case keys2 :g-concrete)))
               (fgl::fgl-progn
                (fgl::fgl-error :msg "non-concrete keys12")
                (fgl::syntax-interp (cw "Abort merge-val-imap-add-pairs: non-concrete keys12~%"))
                (fgl::abort-rewrite (if test (val-imap-add-pairs pairs1 nil)
                                      (val-imap-add-pairs pairs2 nil)))))
              (keys1 (set::mergesort keys1))
              (keys2 (set::mergesort keys2))
              ((unless (equal keys1 keys2))
               (fgl::fgl-progn
                (fgl::syntax-interp (cw "Merge-val-imap-add-pairs: unequal keys, creating explicit IF assuming that we're in multiple THROW merge case~%"))
                (fgl::if! test (val-imap-add-pairs pairs1 nil)
                          (val-imap-add-pairs pairs2 nil))))
              (pairs1 (make-fast-alist pairs1))
              (pairs2 (make-fast-alist pairs2))
              (ans (val-imap-add-pairs (val-imap-pairs-merge keys1 test pairs1 pairs2)
                                       nil)))
           (fgl::fgl-progn (fast-alist-free pairs1)
                           (fast-alist-free pairs2)
                           ans)))
  :hints(("Goal" :in-theory (enable fgl::if!))))

(fgl::def-fgl-branch-merge merge-val-imap-add/put-pairs
  (equal (if test (val-imap-add-pairs pairs1 nil)
           (val-imap-put-pairs pairs2 base2))
         (b* ((keys1 (acl2::alist-keys pairs1))
              ((unless (fgl::syntax-bind keys1-conc (fgl::fgl-object-case keys1 :g-concrete)))
               (fgl::fgl-prog2
                (fgl::syntax-interp (cw "Abort merge-val-imap-add/put-pairs: non-concrete keys1~%"))
                (fgl::abort-rewrite (if test (val-imap-add-pairs pairs1 nil)
                                      (val-imap-put-pairs pairs2 base2)))))
              (keys2 (val-imap-keys base2))
              ((unless (fgl::syntax-bind keys2-conc (fgl::fgl-object-case keys2 :g-concrete)))
               (fgl::fgl-progn
                (fgl::fgl-error :msg "non-concrete keys2")
                (fgl::syntax-interp (cw "Abort merge-val-imap-add/put-pairs: non-concrete keys2~%"))
                (fgl::abort-rewrite (if test (val-imap-add-pairs pairs1 nil)
                                      (val-imap-put-pairs pairs2 base2)))))
              (keys1 (set::mergesort keys1))
              (keys2 (set::mergesort keys2))
              ((unless (equal keys1 keys2))
               (fgl::fgl-progn
                (fgl::syntax-interp (cw "Merge-val-imap-add/put-pairs: unequal keys, creating explicit IF assuming that we're in multiple THROW merge case~%"))
                (fgl::if! test (val-imap-add-pairs pairs1 nil)
                          (val-imap-put-pairs pairs2 base2))))
              (pairs1 (make-fast-alist pairs1))
              (pairs2 (make-fast-alist pairs2))
              (ans (val-imap-add-pairs (val-imap-pairs-merge-add/put keys1 test pairs1 pairs2 base2)
                                       nil)))
           (fgl::fgl-progn (fast-alist-free pairs1)
                           (fast-alist-free pairs2)
                           ans)))
  :hints(("Goal" :in-theory (enable fgl::if!))))

(define pairs-equiv-on-keys (keys alist1 alist2)
  :verify-guards nil
  (if (atom keys)
      t
    (and (b* ((key (car keys)))
           (or (not (identifier-p (car keys)))
               (and (iff (hons-assoc-equal key alist1)
                         (hons-assoc-equal key alist2))
                    (equal (val-fix (cdr (hons-assoc-equal key alist1)))
                           (val-fix (cdr (hons-assoc-equal key alist2)))))))
         (pairs-equiv-on-keys (cdr keys) alist1 alist2)))
  ///
  (defthmd pairs-equiv-on-keys-implies-equal
    (implies (and (pairs-equiv-on-keys keys alist1 alist2)
                  (member-equal key keys)
                  (identifier-p key))
             (and (iff (hons-assoc-equal key alist1)
                       (hons-assoc-equal key alist2))
                  (equal (val-fix (cdr (hons-assoc-equal key alist1)))
                         (val-fix (cdr (hons-assoc-equal key alist2))))))))


(defthm identifierlist-p-of-tail
  (implies (identifierlist-p x)
           (identifierlist-p (tail x)))
  :hints(("Goal" :in-theory (enable tail sfix))))

(defthm identifier-p-of-head
  (implies (and (identifierlist-p x)
                (not (emptyp x)))
           (identifier-p (head x)))
  :hints(("Goal" :in-theory (enable head emptyp sfix))))



(define filter-imap-keys (keys x)
  :verify-guards nil
  :returns (new-keys setp)
  :measure (cardinality keys)
  :hints(("Goal" :in-theory (enable cardinality)))
  (if (emptyp keys)
      nil
    (let ((rest  (filter-imap-keys (tail keys) x)))
      (if (and (identifier-p (head keys))
               (val-imap-has-key (head keys) x))
          (insert (head keys) rest)
        rest)))
  ///
  (defthm in-of-filter-imap-keys
    (iff (in key (filter-imap-keys keys x))
         (and (val-imap-has-key key x)
              (identifier-p key)
              (in key keys))))

  (defret identifierlist-p-of-<fn>
    (identifierlist-p new-keys)))


(define pairs-equiv-to-imap-on-keys (keys alist imap)
  :verify-guards nil
  (if (atom keys)
      t
    (and (b* ((key (car keys)))
           (or (not (identifier-p key))
               (equal (val-fix (cdr (hons-assoc-equal key alist)))
                      (val-imap-lookup key imap))))
         (pairs-equiv-to-imap-on-keys (cdr keys) alist imap)))
  ///
  (defthmd pairs-equiv-to-imap-on-keys-implies
    (implies (and (pairs-equiv-to-imap-on-keys keys alist imap)
                  (identifier-p key)
                  (member-equal key keys))
             (equal (val-fix (cdr (hons-assoc-equal key alist)))
                    (val-imap-lookup key imap)))))

(define pairs-equiv-to-imap-on-keys-badguy (keys alist imap)
  :verify-guards nil
  :returns (badguy identifier-p)
  (if (atom keys)
      "a"
    (if (b* ((key (car keys)))
          (or (not (identifier-p key))
              (equal (val-fix (cdr (hons-assoc-equal key alist)))
                     (val-imap-lookup key imap))))
        (pairs-equiv-to-imap-on-keys-badguy (cdr keys) alist imap)
      (identifier-fix (car keys))))
  ///
  (defthmd pairs-equiv-to-imap-on-keys-when-badguy
    (let ((key (pairs-equiv-to-imap-on-keys-badguy keys alist imap)))
      (implies (or (not (member-equal key keys))
                   (equal (val-fix (cdr (hons-assoc-equal key alist)))
                          (val-imap-lookup key imap)))
               (pairs-equiv-to-imap-on-keys keys alist imap)))
    :hints(("Goal" :in-theory (enable pairs-equiv-to-imap-on-keys)))))


(encapsulate nil
  (local (defthm identifier-p-when-in
           (implies (and (identifierlist-p y)
                         (in h y))
                    (identifier-p h))
           :hints(("Goal" :in-theory (enable in)))))
  (defthm identifierlist-p-of-intersect
    (implies (or (identifierlist-p x)
                 (identifierlist-p y))
             (identifierlist-p (intersect x y)))
    :hints(("Goal" :in-theory (enable intersect)))))

(encapsulate nil
  (local (define pairs-equiv-on-keys-badguy (keys alist1 alist2)
           :verify-guards nil
           (if (atom keys)
               nil
             (if (b* ((key (car keys)))
                   (or (not (identifier-p key))
                       (and (iff (hons-assoc-equal key alist1)
                                 (hons-assoc-equal key alist2))
                            (equal (val-fix (cdr (hons-assoc-equal key alist1)))
                                   (val-fix (cdr (hons-assoc-equal key alist2)))))))
                 (pairs-equiv-on-keys-badguy (cdr keys) alist1 alist2)
               (car keys)))
           ///
           (defthmd pairs-equiv-on-keys-when-badguy
             (let ((key (pairs-equiv-on-keys-badguy keys alist1 alist2)))
               (implies (or (not (identifier-p key))
                            (not (member-equal key keys))
                            (and (iff (hons-assoc-equal key alist1)
                                      (hons-assoc-equal key alist2))
                                 (equal (val-fix (cdr (hons-assoc-equal key alist1)))
                                        (val-fix (cdr (hons-assoc-equal key alist2))))))
                        (pairs-equiv-on-keys keys alist1 alist2)))
             :hints(("Goal"
                     :induct (pairs-equiv-on-keys-badguy keys alist1 alist2)
                     :expand ((pairs-equiv-on-keys-badguy keys alist1 alist2)
                              (pairs-equiv-on-keys keys alist1 alist2)))))))

  (local (defthm val-imap-has-key-of-nil
           (not (val-imap-has-key k nil))
           :hints(("Goal" :in-theory (enable val-imap-has-key)))))

  (fgl::def-fgl-rewrite equal-of-val-imap-add-pairs
    (equal (equal (val-imap-add-pairs pairs1 nil)
                  (val-imap-add-pairs pairs2 nil))
           (pairs-equiv-on-keys (mergesort (append (acl2::alist-keys pairs1)
                                                   (acl2::alist-keys pairs2)))
                                pairs1 pairs2))
    :hints ((acl2::use-termhint
             (b* ((alist1 (val-imap-add-pairs pairs1 nil))
                  (alist2 (val-imap-add-pairs pairs2 nil))
                  (keys (mergesort (append (acl2::alist-keys pairs1)
                                           (acl2::alist-keys pairs2))))
                  (pairs-equiv (pairs-equiv-on-keys keys pairs1 pairs2))
                  ((when pairs-equiv)
                   (b* ((badguy (identifier-fix (omap::diff-key alist1 alist2)))
                        ((when (and (equal (val-fix (cdr (hons-assoc-equal badguy pairs1)))
                                           (val-fix (cdr (hons-assoc-equal badguy pairs2))))
                                    (iff (hons-assoc-equal badguy pairs1)
                                         (hons-assoc-equal badguy pairs2))))
                         `(:use ((:instance val-imap-lookup-diff-key-when-unequal
                                  (x ,(acl2::hq alist1))
                                  (y ,(acl2::hq alist2)))))))
                     `(:use ((:instance pairs-equiv-on-keys-implies-equal
                              (key ,(acl2::hq badguy))
                              (keys ,(acl2::hq keys))
                              (alist1 pairs1) (alist2 pairs2))))))
                  (badguy (pairs-equiv-on-keys-badguy keys pairs1 pairs2))
                  ((when (or (not (identifier-p badguy))
                             (not (member-equal badguy keys))
                             (and (iff (hons-assoc-equal badguy pairs1)
                                       (hons-assoc-equal badguy pairs2))
                                  (equal (val-fix (cdr (hons-assoc-equal badguy pairs1)))
                                         (val-fix (cdr (hons-assoc-equal badguy pairs2)))))))
                   `(:use ((:instance pairs-equiv-on-keys-when-badguy
                            (keys ,(acl2::hq keys))
                            (alist1 pairs1) (alist2 pairs2)))))
                  ((unless (iff (hons-assoc-equal badguy pairs1)
                                (hons-assoc-equal badguy pairs2)))
                   `(:use ((:instance val-imap-has-key-of-val-imap-add-pairs
                            (k ,(acl2::hq badguy)) (pairs pairs1) (x nil))
                           (:instance val-imap-has-key-of-val-imap-add-pairs
                            (k ,(acl2::hq badguy)) (pairs pairs2) (x nil)))
                     :in-theory (disable val-imap-has-key-of-val-imap-add-pairs)))
                  ((unless (equal (val-fix (cdr (hons-assoc-equal badguy pairs1)))
                                  (val-fix (cdr (hons-assoc-equal badguy pairs2)))))
                   `(:use ((:instance val-imap-lookup-of-val-imap-add-pairs
                            (k ,(acl2::hq badguy)) (pairs pairs1) (x nil))
                           (:instance val-imap-lookup-of-val-imap-add-pairs
                            (k ,(acl2::hq badguy)) (pairs pairs2) (x nil)))
                     :in-theory (disable val-imap-lookup-of-val-imap-add-pairs))))
               nil))))

  (local (defthm member-when-in
           (implies (setp x)
                    (iff (member-equal k x)
                         (in k x)))
           :hints(("Goal" :in-theory (enable in member-equal setp head tail emptyp)))))

  (local (in-theory (disable val-imap-lookup-of-val-imap-put-pairs)))

  (fgl::def-fgl-rewrite equal-of-val-imap-put-pairs
    (equal (equal (val-imap-put-pairs pairs1 base)
                  (val-imap-put-pairs pairs2 base))
           (b* ((keys1 (filter-imap-keys (mergesort (acl2::alist-keys pairs1)) base))
                (keys2 (filter-imap-keys (mergesort (acl2::alist-keys pairs2)) base))
                (both-keys (intersect keys1 keys2))
                (only1-keys (difference keys1 keys2))
                (only2-keys (difference keys2 keys1)))
             (and (pairs-equiv-on-keys both-keys pairs1 pairs2)
                  (pairs-equiv-to-imap-on-keys only1-keys pairs1 base)
                  (pairs-equiv-to-imap-on-keys only2-keys pairs2 base))))
    :hints ((acl2::use-termhint
             (b* ((alist1 (val-imap-put-pairs pairs1 base))
                  (alist2 (val-imap-put-pairs pairs2 base))(keys1 (filter-imap-keys (mergesort (acl2::alist-keys pairs1)) base))
                  (keys2 (filter-imap-keys (mergesort (acl2::alist-keys pairs2)) base))
                  (both-keys (intersect keys1 keys2))
                  (only1-keys (difference keys1 keys2))
                  (only2-keys (difference keys2 keys1))
                  (pairs-equiv (pairs-equiv-on-keys both-keys pairs1 pairs2))
                  ((unless pairs-equiv)
                   (b* ((badguy (pairs-equiv-on-keys-badguy both-keys pairs1 pairs2))
                        ((when (or (not (identifier-p badguy))
                                   (not (member-equal badguy both-keys))
                                   (and (iff (hons-assoc-equal badguy pairs1)
                                             (hons-assoc-equal badguy pairs2))
                                        (equal (val-fix (cdr (hons-assoc-equal badguy pairs1)))
                                               (val-fix (cdr (hons-assoc-equal badguy pairs2)))))))
                         `(:use ((:instance pairs-equiv-on-keys-when-badguy
                                  (keys ,(acl2::hq both-keys))
                                  (alist1 pairs1) (alist2 pairs2)))))
                        ((unless (iff (hons-assoc-equal badguy pairs1)
                                      (hons-assoc-equal badguy pairs2)))
                         `(:use ((:instance val-imap-has-key-of-val-imap-put-pairs
                                  (k ,(acl2::hq badguy)) (pairs pairs1) (x base))
                                 (:instance val-imap-has-key-of-val-imap-put-pairs
                                  (k ,(acl2::hq badguy)) (pairs pairs2) (x base)))
                           :in-theory (disable val-imap-has-key-of-val-imap-put-pairs)))
                        ((unless (equal (val-fix (cdr (hons-assoc-equal badguy pairs1)))
                                        (val-fix (cdr (hons-assoc-equal badguy pairs2)))))
                         `(:use ((:instance val-imap-lookup-of-val-imap-put-pairs
                                  (k ,(acl2::hq badguy)) (pairs pairs1) (x base))
                                 (:instance val-imap-lookup-of-val-imap-put-pairs
                                  (k ,(acl2::hq badguy)) (pairs pairs2) (x base)))
                           :in-theory (disable val-imap-lookup-of-val-imap-add-pairs))))
                     nil))
                  (only1-equiv (pairs-equiv-to-imap-on-keys only1-keys pairs1 base))
                  ((unless only1-equiv)
                   (b* ((badguy (pairs-equiv-to-imap-on-keys-badguy only1-keys pairs1 base))
                        ((when (equal (val-imap-lookup badguy alist1)
                                      (val-imap-lookup badguy alist2)))
                         `(:use ((:instance acl2::mark-clause-is-true (x 'only1-badguy-lookups-same))
                                 (:instance val-imap-lookup-of-val-imap-put-pairs
                                  (k ,(acl2::hq badguy)) (pairs pairs1) (x base))
                                 (:instance val-imap-lookup-of-val-imap-put-pairs
                                  (k ,(acl2::hq badguy)) (pairs pairs2) (x base))
                                 (:instance val-imap-has-key-of-val-imap-put-pairs
                                  (k ,(acl2::hq badguy)) (pairs pairs1) (x base))
                                 (:instance val-imap-has-key-of-val-imap-put-pairs
                                  (k ,(acl2::hq badguy)) (pairs pairs2) (x base))
                                 (:instance pairs-equiv-to-imap-on-keys-when-badguy
                                  (keys ,(acl2::hq only1-keys)) (alist pairs1) (imap base)))
                           :in-theory (disable val-imap-lookup-of-val-imap-put-pairs
                                               val-imap-has-key-of-val-imap-put-pairs))))
                     `(:use ((:instance acl2::mark-clause-is-true (x 'only1-badguy-lookups-diff))))))
                  (only2-equiv (pairs-equiv-to-imap-on-keys only2-keys pairs2 base))
                  ((unless only2-equiv)
                   (b* ((badguy (pairs-equiv-to-imap-on-keys-badguy only2-keys pairs2 base))
                        ((when (equal (val-imap-lookup badguy alist1)
                                      (val-imap-lookup badguy alist2)))
                         `(:use ((:instance acl2::mark-clause-is-true (x 'only2-badguy-lookups-same))
                                 (:instance val-imap-lookup-of-val-imap-put-pairs
                                  (k ,(acl2::hq badguy)) (pairs pairs1) (x base))
                                 (:instance val-imap-lookup-of-val-imap-put-pairs
                                  (k ,(acl2::hq badguy)) (pairs pairs2) (x base))
                                 (:instance val-imap-has-key-of-val-imap-put-pairs
                                  (k ,(acl2::hq badguy)) (pairs pairs1) (x base))
                                 (:instance val-imap-has-key-of-val-imap-put-pairs
                                  (k ,(acl2::hq badguy)) (pairs pairs2) (x base))
                                 (:instance pairs-equiv-to-imap-on-keys-when-badguy
                                  (keys ,(acl2::hq only2-keys)) (alist pairs2) (imap base)))
                           :in-theory (disable val-imap-lookup-of-val-imap-put-pairs
                                               val-imap-has-key-of-val-imap-put-pairs))))
                     `(:use ((:instance acl2::mark-clause-is-true (x 'only2-badguy-lookups-diff))))))
                  (badguy (identifier-fix (omap::diff-key alist1 alist2)))
                  ((when (and (hons-assoc-equal badguy pairs1)
                              (hons-assoc-equal badguy pairs2)))
                   `(:use ((:instance acl2::mark-clause-is-true (x 'badguy-in-both-pairs))
                           (:instance val-imap-lookup-diff-key-when-unequal
                            (x ,(acl2::hq alist1))
                            (y ,(acl2::hq alist2)))
                           (:instance pairs-equiv-on-keys-implies-equal
                              (key ,(acl2::hq badguy))
                              (keys ,(acl2::hq both-keys))
                            (alist1 pairs1) (alist2 pairs2)))
                     :in-theory (enable val-imap-lookup-of-val-imap-put-pairs)))
                  ((when (and (hons-assoc-equal badguy pairs1)
                              (not (hons-assoc-equal badguy pairs2))))
                   `(:use ((:instance acl2::mark-clause-is-true (x 'badguy-in-pairs1-only))
                           (:instance val-imap-lookup-diff-key-when-unequal
                            (x ,(acl2::hq alist1))
                            (y ,(acl2::hq alist2)))
                           (:instance pairs-equiv-to-imap-on-keys-implies
                            (key ,(acl2::hq badguy))
                            (keys ,(acl2::hq only1-keys))
                            (alist pairs1) (imap base)))
                     :in-theory (enable val-imap-lookup-of-val-imap-put-pairs)))
                  ((when (and (not (hons-assoc-equal badguy pairs1))
                              (hons-assoc-equal badguy pairs2)))
                   `(:use ((:instance acl2::mark-clause-is-true (x 'badguy-in-pairs2-only))
                           (:instance val-imap-lookup-diff-key-when-unequal
                            (x ,(acl2::hq alist1))
                            (y ,(acl2::hq alist2)))
                           (:instance pairs-equiv-to-imap-on-keys-implies
                            (key ,(acl2::hq badguy))
                            (keys ,(acl2::hq only2-keys))
                            (alist pairs2) (imap base)))
                     :in-theory (enable val-imap-lookup-of-val-imap-put-pairs)))
                  ((when (and (not (hons-assoc-equal badguy pairs1))
                              (not (hons-assoc-equal badguy pairs2))))
                   `(:use ((:instance acl2::mark-clause-is-true (x 'badguy-in-neither))
                           (:instance val-imap-lookup-diff-key-when-unequal
                            (x ,(acl2::hq alist1))
                            (y ,(acl2::hq alist2))))
                     :in-theory (enable val-imap-lookup-of-val-imap-put-pairs))))
               `(:use ((:instance acl2::mark-clause-is-true (x 'should-all-be-equal)))))))))


;; (fgl::def-fgl-branch-merge val-imap-lookup-merge
;;   (implies (val-p y)
;;            (equal (if test (val-imap-lookup k x) y)
;;                   (let ((look (val-imap-lookup k x)))
;;                     (fty::multicase
;;                       ((val-case look) (val-case y))
;;                       ((:v_bool :v_bool) (v_bool (if test look.val y.val)))
;;                       ((:v_bitvector :v_bitvector)
;;                        :when (equal look.len y.len)
;;                        (v_bitvector look.len (if test look.val y.val)))
;;                       ((:v_int :v_int)
;;                        (v_int (if test look.val y.val)))
;;                       ((:v_real :v_real) (v_real (if test look.val y.val)))
;;                       ((:v_string :v_string) (v_string (if test look.val y.val)))
;;                       ((:v_label :v_label) (v_label (choose-value-if test look.val y.val)))
;;                       ((:v_record :v_record) (v_record (if test look.rec y.rec)))
;;                       (- (fgl::abort-rewrite (if test look y)))))))
;;   :hints(("Goal" :in-theory (enable choose-value-if))))


(fgl::def-fgl-rewrite val-imaplist-assoc-of-cons-fgl
  (equal (val-imaplist-assoc name (cons frame stack))
         (if (val-imap-has-key name frame)
             (cons (identifier-fix name)
                   (val-imap-lookup name frame))
           (val-imaplist-assoc name stack)))
  :hints(("Goal" :in-theory (enable val-imaplist-assoc
                                    val-imap-has-key
                                    val-imap-lookup
                                    omap::lookup))))

(fgl::add-fgl-rewrite val-imaplist-assoc-of-nil)

(fgl::def-fgl-rewrite val-imaplist-assign-of-cons-fgl
  (equal (val-imaplist-assign name v (cons frame stack))
         (if (val-imap-has-key name frame)
             (cons (val-imap-put name v frame)
                   (val-imaplist-fix stack))
           (cons (val-imap-fix frame)
                 (val-imaplist-assign name v stack))))
  :hints(("Goal" :in-theory (enable val-imaplist-assign
                                    val-imap-put
                                    val-imap-has-key))))

(fgl::add-fgl-rewrite val-imaplist-assign-of-nil)

(fgl::def-fgl-rewrite env-assign-redef
  (equal (env-assign name v env)
         (b* (((env env))
              ((local-env env.local))
              (name (identifier-fix name))
              (local-look (val-imaplist-assoc name env.local.storage))
              ((When local-look)
               (lk_local (change-env env
                                     :local (change-local-env
                                             env.local
                                             :storage (val-imaplist-assign name v env.local.storage)))))
              ((global-env env.global))
              (global-look (val-imap-has-key name env.global.storage))
              ((When global-look)
               (lk_global (change-env env
                                      :global (change-global-env
                                               env.global
                                               :storage (val-imap-put name v env.global.storage))))))
           (lk_notfound)))
  :hints(("Goal" :in-theory (enable env-assign
                                    env-assign-local
                                    env-assign-global
                                    val-imap-put
                                    val-imap-has-key
                                    ACL2::ASSOC-IS-HONS-ASSOC-EQUAL-WHEN-KEY-NONNIL))))
(fgl::remove-fgl-rewrite env-assign)

(fgl::def-fgl-rewrite env-assign-global-redef
  (b* (((env env))
       ((global-env env.global))
       (global-look (val-imap-has-key name env.global.storage)))
    (implies
     global-look
     (equal (env-assign-global name v env)
            (change-env env
                        :global (change-global-env
                                 env.global
                                 :storage (val-imap-put name v env.global.storage)))
            )))
  :hints(("Goal" :in-theory (e/d (env-assign-global
                                  val-imap-put
                                  val-imap-has-key
                                  val-imap-fix
                                  ACL2::ASSOC-IS-HONS-ASSOC-EQUAL-WHEN-KEY-NONNIL)
                                 ((tau-system))))))

(fgl::remove-fgl-rewrite env-assign-global)

(fgl::def-fgl-rewrite env-find-redef
  (equal (env-find x env)
         (b* (((env env))
              ((local-env env.local))
              (look (val-imaplist-assoc x env.local.storage))
              ((When look)
               (lk_local (cdr look)))
              ((global-env env.global))
              ((When (val-imap-has-key x env.global.storage))
               (lk_global (val-imap-lookup x env.global.storage))))
           (lk_notfound)))
  :hints(("Goal" :in-theory (enable env-find
                                    omap::lookup
                                    val-imap-lookup
                                    val-imap-has-key
                                    acl2::assoc-is-hons-assoc-equal-when-key-nonnil))))
(fgl::remove-fgl-rewrite env-find)

(fgl::def-fgl-rewrite env-find-global-redef
  (equal (env-find-global x env)
         (b* (((env env))
              ((global-env env.global))
              ((When (val-imap-has-key x env.global.storage))
               (ev_normal (val-imap-lookup x env.global.storage))))
           (ev_error "Global variable not found" (identifier-fix x) nil)))
  :hints(("Goal" :in-theory (enable env-find-global
                                    omap::lookup
                                    val-imap-lookup
                                    val-imap-has-key
                                    acl2::assoc-is-hons-assoc-equal-when-key-nonnil))))

(fgl::remove-fgl-rewrite env-find-global)


(fgl::def-fgl-rewrite eval_expr-getenumarray-redef
  (implies (expr_desc-case (expr->desc e) :e_getenumarray)
           (equal (eval_expr env (fgl::concrete e))
                  (b* ((pos (expr->pos_start e))
                       ((e_getenumarray e) (expr->desc e))
                       ((mv (evo (expr_result arr)) orac) (eval_expr env e.base))
                       ((mv (evo (expr_result idx)) orac) (eval_expr arr.env e.index))
                       ((evo idxv) (val-case idx.val
                                     :v_label (ev_normal idx.val.val)
                                     :otherwise (ev_error "DE_BI: getenumarray non-label index" e (list pos))))
                       ((evo arrv) (val-case arr.val
                                     :v_record (ev_normal arr.val.rec)
                                     :otherwise (ev_error "getenumarray non-record value" e (list pos)))))
                    (if (val-imap-has-key idxv arrv)
                        (evo_normal (expr_result (val-imap-lookup idxv arrv) idx.env))
                      (evo_error "DE_BI: getenumarray index not found" e (list pos))))))
  :hints (("goal" :expand ((eval_expr env e))
           :in-theory (enable val-imap-lookup val-imap-has-key
                              omap::lookup))))

(fgl::def-fgl-rewrite eval_expr-*t-getenumarray-redef
  (implies (expr_desc-case (expr->desc e) :e_getenumarray)
           (equal (eval_expr-*t env (fgl::concrete e))
                  (b* ((env (env-replace-static static-env env))
                       (pos (expr->pos_start e))
                       (trace nil)
                       ((e_getenumarray e) (expr->desc e))
                       ((EVOO-*T (EXPR_RESULT ARR))
                        (EVAL_EXPR-*T ENV E.BASE))
                       ((EVOO-*T (EXPR_RESULT IDX))
                        (EVAL_EXPR-*T ARR.ENV E.INDEX))
                       ((EVO-*T IDXV)
                        (VAL-CASE IDX.VAL
                          :V_LABEL (EV_NORMAL IDX.VAL.VAL)
                          :OTHERWISE (EV_ERROR "DE_BI: getenumarray non-label index"
                                               e (LIST POS))))
                       ((EVO-*T ARRV)
                        (VAL-CASE ARR.VAL
                          :V_RECORD (EV_NORMAL ARR.VAL.REC)
                          :OTHERWISE (EV_ERROR "getenumarray non-record value"
                                               e (LIST POS)))))
                    (IF (val-imap-has-key idxv arrv)
                        (EVO_NORMAL-*T (EXPR_RESULT (val-imap-lookup idxv arrv) IDX.ENV))
                        (EVO_ERROR-*T "DE_BI: getenumarray index not found"
                                      e (LIST POS))))))
  :hints (("goal" :expand ((eval_expr-*t env e))
           :in-theory (enable val-imap-lookup val-imap-has-key
                              omap::lookup))))

(local (in-theory (acl2::disable* asl-*t-equals-original-rules)))

(fgl::def-fgl-rewrite eval_expr-*t-when-error-free-getenumarray-redef
  (implies (and (eval_expr-*t-no-error env e)
                (expr_desc-case (expr->desc e) :e_getenumarray))
           (equal (eval_expr-*t env (fgl::concrete e))
                  (b* ((env (env-replace-static static-env env))
                       (pos (expr->pos_start e))
                       (trace nil)
                       ((e_getenumarray e) (expr->desc e))
                       ((EVOO-*Tef _condvar-0 (EXPR_RESULT ARR))
                        (EVAL_EXPR-*T ENV E.BASE))
                       ((EVOO-*Tef _condvar-1 (EXPR_RESULT IDX))
                        (EVAL_EXPR-*T ARR.ENV E.INDEX))
                       ((EVO-*Tef _condvar-3 IDXV)
                        (VAL-CASE IDX.VAL
                          :V_LABEL (EV_NORMAL IDX.VAL.VAL)
                          :OTHERWISE (EV_ERROR "DE_BI: getenumarray non-label index"
                                               e (LIST POS))))
                       ((EVO-*Tef _condvar-4 ARRV)
                        (VAL-CASE ARR.VAL
                          :V_RECORD (EV_NORMAL ARR.VAL.REC)
                          :OTHERWISE (EV_ERROR "getenumarray non-record value"
                                               e (LIST POS)))))
                    (IF (val-imap-has-key idxv arrv)
                        (EVO_NORMAL-*T (EXPR_RESULT (val-imap-lookup idxv arrv) IDX.ENV))
                        (EVO_ERROR-*T "DE_BI: getenumarray index not found"
                                      e (LIST POS))))))
  :HINTS ((ACL2::JUST-EXPAND ((EVAL_EXPR-*T-NO-ERROR ENV E)))
          (ACL2::JUST-EXPAND ((EVAL_EXPR-*T ENV E)))
          (and stable-under-simplificationp
               (ACL2::JUST-EXPAND ((EVAL_EXPR-*T ENV E))))
          '(:IN-THEORY (ENABLE FGL::CONDITIONALIZE1 fgl::conditionalize2
                               val-imap-lookup val-imap-has-key
                               omap::lookup))))



(fgl::def-fgl-rewrite get_field!-redef
  (equal (get_field! field rec)
         (if (val-imap-has-key field rec)
             (ev_normal (val-imap-lookup field rec))
           (ev_error "get_field not found" (identifier-fix field) nil)))
  :hints(("Goal" :in-theory (enable get_field!
                                    val-imap-lookup val-imap-has-key
                                    omap::lookup))))

;; (define val-imap-has-keys ((keys identifierlist-p) (x val-imap-p))
;;   :verify-guards nil
;;   (if (atom keys)
;;       t
;;     (and (val-imap-has-key (car keys) x)
;;          (val-imap-has-keys (cdr keys) x))))

;; (define val-imap-has-no-keys ((keys identifierlist-p) (x val-imap-p))
;;   :verify-guards nil
;;   (if (atom keys)
;;       t
;;     (and (not (val-imap-has-key (car keys) x))
;;          (val-imap-has-no-keys (cdr keys) x))))

;; (defthm val-imap-add-pairs-of-reverse
;;   (implies (and (identifierlist-p names)
;;                 (no-duplicatesp-equal names)
;;                 (val-imap-has-no-keys names storage))
;;            (equal (val-imap-add

(defthm val-imap-p-of-from-lists*
  (implies (and (val-imap-p x)
                (vallist-p vals)
                (identifierlist-p keys)
                (equal (len keys) (len vals)))
           (val-imap-p (omap::from-lists* keys vals x))))

(defthmd val-imap-add-pairs-is-from-lists*
  (implies (and (identifierlist-p names)
                (vallist-p vals)
                (equal (len names) (len vals)))
           (equal (val-imap-add-pairs (pairlis$ names vals) x)
                  (omap::from-lists* names vals (val-imap-fix x))))
  :hints (("goal" :in-theory (e/d (val-imap-add-pairs
                                   pairlis$
                                   val-imap-add
                                     omap::from-lists*)
                                  (omap::from-lists*-in-terms-of-update*)))))

(fgl::def-fgl-rewrite declare_local_identifiers-redef
  (equal (declare_local_identifiers env names vals)
         (b* (((env env))
              ((local-env l) env.local)
              (new-storage (cons (val-imap-add-pairs
                                  (pairlis$ (identifierlist-fix names)
                                            (vallist-fix (take (len names) vals)))
                                  (car l.storage))
                                 (val-imaplist-fix (cdr l.storage)))))
           (change-env env :local (change-local-env l :storage new-storage))))
  :hints(("Goal" :in-theory (enable declare_local_identifiers
                                    val-imap-add-pairs-is-from-lists*))))

(fgl::remove-fgl-rewrite declare_local_identifiers)

(fgl::def-fgl-rewrite declare_local_identifier-redef
  (equal (declare_local_identifier env name val)
         (b* (((env env))
              ((local-env l) env.local)
              (new-storage (cons (val-imap-add name val (car l.storage))
                                 (cdr l.storage))))
           (change-env env :local (change-local-env l :storage new-storage))))
  :hints(("Goal" :in-theory (enable declare_local_identifier
                                    val-imap-add))))

(fgl::remove-fgl-rewrite declare_local_identifier)

(fgl::def-fgl-rewrite eval_expr-enumarray-redef
  (implies (expr_desc-case (expr->desc e) :e_enumarray)
           (equal (eval_expr env (fgl::concrete e))
                  (b* (((e_enumarray e) (expr->desc e))
                       ((mv (evo (expr_result v)) orac) (eval_expr env e.value))
                       (labels (set::mergesort e.labels))
                       (len (len labels))
                       (vals (make-list len :initial-element v.val))
                       (rec (pairlis$ labels vals)))
                    (evo_normal (expr_result (v_record (val-imap-add-pairs rec nil)) v.env)))))
  :hints (("goal" :expand ((eval_expr env e))
           :in-theory (enable val-imap-add-pairs-is-from-lists*))))

(fgl::def-fgl-rewrite eval_expr-*t-enumarray-redef
  (implies (expr_desc-case (expr->desc e) :e_enumarray)
           (equal (eval_expr-*t env (fgl::concrete e))
                  (b* ((trace nil)
                       ((e_enumarray desc) (expr->desc e))
                       (env (env-replace-static static-env env))
                       ((EVOO-*T (EXPR_RESULT V))
                        (EVAL_EXPR-*T ENV DESC.VALUE))
                       (LABELS (MERGESORT DESC.LABELS))
                       (LEN (LEN LABELS))
                       (VALS (MAKE-LIST LEN :INITIAL-ELEMENT V.VAL))
                       (REC (pairlis$ labels vals)))
                    (EVO_NORMAL-*T (EXPR_RESULT (V_RECORD (val-imap-add-pairs rec nil)) V.ENV)))))
  :hints (("goal" :expand ((eval_expr-*t env e))
           :in-theory (enable val-imap-add-pairs-is-from-lists*))))

(fgl::def-fgl-rewrite eval_expr-*t-when-error-free-enumarray-redef
  (implies (and (eval_expr-*t-no-error env e)
                (expr_desc-case (expr->desc e) :e_enumarray))
           (equal (eval_expr-*t env (fgl::concrete e))
                  (b* ((trace nil)
                       ((e_enumarray desc) (expr->desc e))
                       (env (env-replace-static static-env env))
                       ((EVOO-*Tef _condvar-0 (EXPR_RESULT V))
                        (EVAL_EXPR-*T ENV DESC.VALUE))
                       (LABELS (MERGESORT DESC.LABELS))
                       (LEN (LEN LABELS))
                       (VALS (MAKE-LIST LEN :INITIAL-ELEMENT V.VAL))
                       (REC (pairlis$ labels vals)))
                    (EVO_NORMAL-*T (EXPR_RESULT (V_RECORD (val-imap-add-pairs rec nil)) V.ENV)))))
  :hints ((ACL2::JUST-EXPAND ((EVAL_EXPR-*T-NO-ERROR ENV E)))
          (ACL2::JUST-EXPAND ((EVAL_EXPR-*T ENV E)))
          (and stable-under-simplificationp
               (ACL2::JUST-EXPAND ((EVAL_EXPR-*T ENV E))))
          '(:IN-THEORY (ENABLE FGL::CONDITIONALIZE1 fgl::conditionalize2
                               val-imap-add-pairs-is-from-lists*))))


(local (defthm len-of-eval_expr_list-*t
         (b* (((mv res ?new-orac ?trace)
               (eval_expr_list-*t env e)))
           (implies (eval_result-case res :ev_normal)
                    (equal (len (exprlist_result->val (ev_normal->res res)))
                           (len e))))
         :hints(("Goal" :in-theory (acl2::enable* asl-*t-equals-original-rules)))))

(fgl::def-fgl-rewrite eval_expr-record-redef
  (implies (expr_desc-case (expr->desc e) :e_record)
           (equal (eval_expr env (fgl::concrete e))
                  (b* (((e_record desc) (expr->desc e))
                       (exprs (named_exprlist->exprs desc.fields))
                       (names (named_exprlist->names desc.fields))
                       ((mv (evo (exprlist_result e)) orac) (eval_expr_list env exprs)))
                    (evo_normal (expr_result (v_record (val-imap-add-pairs (pairlis$ names e.val) nil)) e.env)))))
  :hints (("goal" :expand ((eval_expr env e))
           :in-theory (enable val-imap-add-pairs-is-from-lists*))))

(fgl::def-fgl-rewrite eval_expr-*t-record-redef
  (implies (expr_desc-case (expr->desc e) :e_record)
           (equal (eval_expr-*t env (fgl::concrete e))
                  (b* ((trace nil)
                       ((e_record desc) (expr->desc e))
                       (exprs (named_exprlist->exprs desc.fields))
                       (names (named_exprlist->names desc.fields))
                       (env (env-replace-static static-env env))
                       ((evoo-*t (exprlist_result e))
                        (eval_expr_list-*t env exprs)))
                    (evo_normal-*t (expr_result (v_record (val-imap-add-pairs (pairlis$ names e.val) nil))
                                                 e.env)))))
  :hints (("goal" :expand ((eval_expr-*t env e))
           :in-theory (enable val-imap-add-pairs-is-from-lists*))))

(fgl::def-fgl-rewrite eval_expr-*t-when-error-free-record-redef
  (implies (and (eval_expr-*t-no-error env e)
                (expr_desc-case (expr->desc e) :e_record))
           (equal (eval_expr-*t env (fgl::concrete e))
                  (b* ((trace nil)
                       ((e_record desc) (expr->desc e))
                       (exprs (named_exprlist->exprs desc.fields))
                       (names (named_exprlist->names desc.fields))
                       (env (env-replace-static static-env env))
                       ((evoo-*tef _condvar-0 (exprlist_result e))
                        (eval_expr_list-*t env exprs)))
                    (evo_normal-*t (expr_result (v_record (val-imap-add-pairs (pairlis$ names e.val) nil))
                                                 e.env)))))
  :hints ((ACL2::JUST-EXPAND ((EVAL_EXPR-*T-NO-ERROR ENV E)))
          (ACL2::JUST-EXPAND ((EVAL_EXPR-*T ENV E)))
          (and stable-under-simplificationp
               (ACL2::JUST-EXPAND ((EVAL_EXPR-*T ENV E))))
          '(:IN-THEORY (ENABLE FGL::CONDITIONALIZE1 fgl::conditionalize2
                               val-imap-add-pairs-is-from-lists*))))

(fgl::def-fgl-rewrite eval_lexpr-setenumarray-redef
  (implies (lexpr_desc-case (lexpr->desc lx) :le_setenumarray)
           (equal (eval_lexpr env (fgl::concrete lx) v)
                  (b* ((pos (lexpr->pos_start lx))
                       ((le_setenumarray lx) (lexpr->desc lx))
                       (rbase (expr_of_lexpr lx.base))
                       ((mv (evo (expr_result rbv)) orac) (eval_expr env rbase))
                       ((mv (evo (expr_result idx)) orac) (eval_expr rbv.env lx.index))
                       ((evob idxv) (v_to_label idx.val))
                       ((evo newarray)
                        (val-case rbv.val
                          :v_record (if (val-imap-has-key idxv rbv.val.rec)
                                        (ev_normal (v_record (val-imap-put idxv (val-fix v)
                                                                           rbv.val.rec)))
                                      (ev_error "DE_BI: le_setenumarray unrecognized index" lx (list pos)))
                          :otherwise (ev_error "le_setenumarray non record base" lx (list pos)))))
                    (eval_lexpr idx.env lx.base newarray))))
  :hints (("goal" :expand ((eval_lexpr env lx v))
           :in-theory (enable val-imap-has-key val-imap-put v_to_label))))

(fgl::def-fgl-rewrite eval_lexpr-*t-setenumarray-redef
  (implies (lexpr_desc-case (lexpr->desc lx) :le_setenumarray)
           (equal (eval_lexpr-*t env (fgl::concrete lx) v)
                  (b* ((trace nil)
                       (pos (lexpr->pos_start lx))
                       ((le_setenumarray lx) (lexpr->desc lx))
                       (rbase (expr_of_lexpr lx.base))
                       (env (env-replace-static static-env env))
                       ((evoo-*t (expr_result rbv))
                        (eval_expr-*t env rbase))
                       ((evoo-*t (expr_result idx))
                        (eval_expr-*t rbv.env lx.index))
                       ((evob-*t idxv) (v_to_label idx.val))
                       ((evo-*t newarray)
                        (val-case rbv.val :v_record
                          (if (val-imap-has-key idxv rbv.val.rec)
                              (ev_normal (v_record (val-imap-put idxv (val-fix v)
                                                                 rbv.val.rec)))
                            (ev_error "DE_BI: le_setenumarray unrecognized index"
                                      lx (list pos)))
                          :otherwise (ev_error "le_setenumarray non record base"
                                               lx (list pos)))))
                    (evtailcall-*t (eval_lexpr-*t idx.env lx.base newarray)))))
  :hints (("goal" :expand ((eval_lexpr-*t env lx v))
           :in-theory (enable val-imap-has-key val-imap-put v_to_label))))

(fgl::def-fgl-rewrite eval_lexpr-*t-when-error-free-setenumarray-redef
  (implies (and (eval_lexpr-*t-no-error env lx v)
                (lexpr_desc-case (lexpr->desc lx) :le_setenumarray))
           (equal (eval_lexpr-*t env (fgl::concrete lx) v)
                  (b* ((trace nil)
                       (pos (lexpr->pos_start lx))
                       ((le_setenumarray lx) (lexpr->desc lx))
                       (rbase (expr_of_lexpr lx.base))
                       (env (env-replace-static static-env env))
                       ((evoo-*tef _condvar-0 (expr_result rbv))
                        (eval_expr-*t env rbase))
                       ((evoo-*tef _condvar-1 (expr_result idx))
                        (eval_expr-*t rbv.env lx.index))
                       ((evob-*tef _condvar-2 idxv) (v_to_label idx.val))
                       ((evo-*tef _condvar-3 newarray)
                        (val-case rbv.val :v_record
                          (if (val-imap-has-key idxv rbv.val.rec)
                              (ev_normal (v_record (val-imap-put idxv (val-fix v)
                                                                 rbv.val.rec)))
                            (ev_error "DE_BI: le_setenumarray unrecognized index"
                                      lx (list pos)))
                          :otherwise (ev_error "le_setenumarray non record base"
                                               lx (list pos)))))
                    (evtailcall-*tef _condvar-4 (eval_lexpr-*t idx.env lx.base newarray)))))
  :hints (("goal" :expand ((eval_lexpr-*t env lx v)
                           (eval_lexpr-*t-no-error env lx v))
           :in-theory (enable val-imap-has-key val-imap-put v_to_label))))

(fgl::def-fgl-rewrite eval_lexpr-setfield-redef
  (implies (lexpr_desc-case (lexpr->desc lx) :le_setfield)
           (equal (eval_lexpr env (fgl::concrete lx) v)
                  (b* ((pos (lexpr->pos_start lx))
                       ((le_setfield lx) (lexpr->desc lx))
                       (rbase (expr_of_lexpr lx.base))
                       ((mv (evo (expr_result rbv)) orac) (eval_expr env rbase))
                       ((evo newrec)
                        (val-case rbv.val
                          :v_record (if (val-imap-has-key lx.field rbv.val.rec)
                                        (ev_normal (v_record (val-imap-put lx.field v rbv.val.rec)))
                                      (ev_error "le_setfield unrecognized field" lx (list pos)))
                          :otherwise (ev_error "le_setfield non record base" lx (list pos)))))
                    (eval_lexpr rbv.env lx.base newrec))))
  :hints (("goal" :expand ((eval_lexpr env lx v))
           :in-theory (enable val-imap-has-key val-imap-put
                              acl2::assoc-is-hons-assoc-equal-when-key-nonnil))))

(fgl::def-fgl-rewrite eval_lexpr-*t-setfield-redef
  (implies (lexpr_desc-case (lexpr->desc lx) :le_setfield)
           (equal (eval_lexpr-*t env (fgl::concrete lx) v)
                  (b* ((trace nil)
                       (pos (lexpr->pos_start lx))
                       ((le_setfield lx) (lexpr->desc lx))
                       (rbase (expr_of_lexpr lx.base))
                       (env (env-replace-static static-env env))
                       ((evoo-*t (expr_result rbv))
                        (eval_expr-*t env rbase))
                       ((evo-*t newrec)
                        (val-case rbv.val :v_record
                          (if (val-imap-has-key lx.field rbv.val.rec)
                              (ev_normal (v_record (val-imap-put lx.field v rbv.val.rec)))
                            (ev_error "le_setfield unrecognized field"
                                      lx (list pos)))
                          :otherwise (ev_error "le_setfield non record base"
                                               lx (list pos)))))
                    (evtailcall-*t (eval_lexpr-*t rbv.env lx.base newrec)))))
  :hints (("goal" :expand ((eval_lexpr-*t env lx v))
           :in-theory (enable val-imap-has-key val-imap-put
                              acl2::assoc-is-hons-assoc-equal-when-key-nonnil))))

(fgl::def-fgl-rewrite eval_lexpr-*t-when-error-free-setfield-redef
  (implies (and (eval_lexpr-*t-no-error env lx v)
                (lexpr_desc-case (lexpr->desc lx) :le_setfield))
           (equal (eval_lexpr-*t env (fgl::concrete lx) v)
                  (b* ((trace nil)
                       (pos (lexpr->pos_start lx))
                       ((le_setfield lx) (lexpr->desc lx))
                       (rbase (expr_of_lexpr lx.base))
                       (env (env-replace-static static-env env))
                       ((evoo-*tef _condvar-0 (expr_result rbv))
                        (eval_expr-*t env rbase))
                       ((evo-*tef _condvar-1 newrec)
                        (val-case rbv.val :v_record
                          (fgl::conditionalize
                           _condvar-3
                           (and (val-case rbv.val :v_record)
                                (val-imap-has-key lx.field (v_record->rec rbv.val)))
                           (ev_normal (v_record (val-imap-put lx.field v rbv.val.rec))))
                          :otherwise (ev_error "le_setfield non record base"
                                               lx (list pos)))))
                    (evtailcall-*tef _condvar-2 (eval_lexpr-*t rbv.env lx.base newrec)))))
  :hints (("goal" :expand ((eval_lexpr-*t env lx v)
                           (eval_lexpr-*t-no-error env lx v))
           :in-theory (enable val-imap-has-key val-imap-put
                              acl2::assoc-is-hons-assoc-equal-when-key-nonnil))))

(fgl::def-fgl-rewrite bitvec_fields_to_record!-redef
  (equal (bitvec_fields_to_record! fields slices rec bv width)
         (b* (((when (atom fields)) (ev_normal (v_record rec)))
              (field (identifier-fix (car fields)))
              ((unless (val-imap-has-key field rec))
               (ev_error "bitvec_fields_to_record!: field not in record"
                         (identifier-fix field) nil))
              ((intpair s) (car slices))
              (start s.first)
              (length s.second)
              ((unless (and (<= 0 start)
                            (<= 0 length)
                            (or (not width)
                                (<= (+ start length) (ifix width)))))
               (ev_error "bitvec_fields_to_record!: out of bounds slice"
                         (intpair-fix (car slices)) nil))
              (fieldval (loghead length (logtail start bv)))
              (new-rec (val-imap-put field (v_bitvector length fieldval) rec)))
           (bitvec_fields_to_record! (cdr fields) (cdr slices) new-rec bv width)))
  :hints(("Goal" :expand ((bitvec_fields_to_record! fields slices rec bv width))
          :in-theory (enable val-imap-has-key val-imap-put
                             acl2::assoc-is-hons-assoc-equal-when-key-nonnil))))


;; (defthm remove-assoc-equal-of-put-assoc-equal
;;   (equal (remove-assoc-equal k1 (put-assoc-equal k2 val x))
;;          (if (equal k1 k2)
;;              (remove-assoc-equal k1 x)
;;            (put-assoc-equal k2 val (remove-assoc-equal k1 x))))
;;   :hints(("Goal" :in-theory (enable put-assoc-equal
;;                                     remove-assoc-equal))))

;; (defthm val-imap-fix-of-remove-assoc-equal
;;   (equal (val-imap-fix (omap::delete key x))
;;          (omap::delete key (val-imap-fix x)))
;;   :hints(("Goal" :in-theory (enable remove-assoc-equal val-imap-fix))))

;; (defthm hons-assoc-equal-of-remove-assoc-equal
;;   (equal (hons-assoc-equal k1 (remove-assoc-equal k2 x))
;;          (if (equal k1 k2)
;;              nil
;;            (hons-assoc-equal k1 x)))
;;   :hints(("Goal" :in-theory (enable hons-assoc-equal remove-assoc-equal))))

(defthm val-imap-has-key-of-delete
  (implies (val-imap-p x)
           (equal (val-imap-has-key k1 (omap::delete k2 x))
                  (and (not (equal (identifier-fix k1) k2))
                       (val-imap-has-key k1 x))))
  :hints(("Goal" :in-theory (enable val-imap-has-key
                                    val-imap-fix))))

(fgl::def-fgl-rewrite delete-of-val-imap-put-pairs
  (implies (identifier-p key)
           (equal (omap::delete key (val-imap-put-pairs pairs x))
                  (val-imap-put-pairs (remove-assoc-equal key pairs) (omap::delete key (val-imap-fix x)))))
  :hints(("Goal" :in-theory (enable val-imap-put-pairs
                                    val-imap-put
                                    remove-assoc-equal
                                    hons-assoc-equal))))

;; (fgl::remove-fgl-rewrite remove-assoc-equal)
;; (fgl::def-fgl-rewrite remove-assoc-equal-of-cons
;;   (equal (remove-assoc-equal key (cons a b))
;;          (let ((rest (remove-assoc-equal key b)))
;;            (if (equal key (car a)) rest (cons a rest))))
;;   :hints(("Goal" :in-theory (enable remove-assoc-equal))))

;; (fgl::def-fgl-rewrite remove-assoc-equal-of-nil
;;   (equal (remove-assoc-equal key nil) nil)
;;   :hints(("Goal" :in-theory (enable remove-assoc-equal))))





;; (defthm alist-keys-of-put-assoc-equal
;;   (implies (and (hons-assoc-equal k x)
;;                 k)
;;            (equal (acl2::alist-keys (put-assoc-equal k v x))
;;                   (acl2::alist-keys x)))
;;   :hints(("Goal" :in-theory (enable acl2::alist-keys
;;                                     put-assoc-equal
;;                                     hons-assoc-equal))))


(defthm identifier-p-compound-recognizer
  (iff (identifier-p X)
       (stringp x))
  :hints(("Goal" :in-theory (enable identifier-p)))
  :rule-classes :compound-recognizer)




(define val-imap-remove ((key identifier-p) (x val-imap-p))
  :returns (new-x val-imap-p)
  :prepwork ((local (defthm val-imap-p-of-delete
                      (implies (val-imap-p x)
                               (val-imap-p (omap::delete k x))))))
  (omap::delete (identifier-fix key) (val-imap-fix x))
  ///
  (local (defthm hons-assoc-equal-of-remove1-assoc-equal
           (implies (not (equal k1 k2))
                    (equal (hons-assoc-equal k1 (remove1-assoc-equal k2 x))
                           (hons-assoc-equal k1 x)))))
  (defret val-imap-has-key-of-<fn>
    (implies (not (identifier-equiv k1 key))
             (equal (val-imap-has-key k1 new-x)
                    (val-imap-has-key k1 x)))
    :hints(("Goal" :in-theory (enable val-imap-has-key))))

  (defret val-imap-lookup-of-<fn>
    (implies (not (identifier-equiv k1 key))
             (equal (val-imap-lookup k1 new-x)
                    (val-imap-lookup k1 x)))
    :hints(("Goal" :in-theory (enable val-imap-lookup
                                      omap::lookup))))

  (fty::deffixequiv val-imap-remove)

  (fgl::remove-fgl-rewrite val-imap-remove))

(define val-imap-removekeys ((keys identifierlist-p) (x val-imap-p))
  :returns (new-x val-imap-p)
  (if (Atom keys)
      (val-imap-fix x)
    (val-imap-remove (Car keys)
                     (val-imap-removekeys (cdr keys) x)))
  ///
  (defret val-imap-has-key-of-<fn>
    (implies (not (member-equal (identifier-fix k1)
                                (identifierlist-fix keys)))
             (equal (val-imap-has-key k1 new-x)
                    (val-imap-has-key k1 x))))

  (fgl::add-fgl-rewrite val-imap-has-key-of-val-imap-removekeys)

  (defret val-imap-lookup-of-<fn>
    (implies (not (member-equal (identifier-fix k1)
                                (identifierlist-fix keys)))
             (equal (val-imap-lookup k1 new-x)
                    (val-imap-lookup k1 x))))

  (fgl::add-fgl-rewrite val-imap-lookup-of-val-imap-removekeys)

  (fgl::def-fgl-rewrite val-imap-removekeys-nest
    (equal (val-imap-removekeys keys1 (val-imap-removekeys keys2 x))
           (val-imap-removekeys (append keys1 keys2) x)))

  (fgl::def-fgl-rewrite val-imap-remove-to-removekeys
    (equal (val-imap-remove k x)
           (val-imap-removekeys (list k) x)))

  (fgl::remove-fgl-rewrite val-imap-removekeys)
  (fty::deffixequiv val-imap-removekeys))


(define record-type-fix-val1 (x fields)
  :verify-guards nil
  (b* (((when (atom fields)) nil)
       ((typed_identifier f1) (car fields))
       (val (ty-fix-val (val-imap-lookup f1.name x) f1.type)))
    (cons (cons f1.name val)
          (record-type-fix-val1 x (cdr fields)))))

;; (fgl::def-fgl-rewrite val-imap-lookup-of-remove1-assoc-equal
;;   (implies (not (equal (identifier-fix k1) (identifier-fix k2)))
;;            (equal (val-imap-lookup k1 (remove1-assoc-equal k2 x))
;;                   (val-imap-lookup k1 x)))
;;   :hints(("Goal" :in-theory (enable val-imap-lookup))))


;; (defthmd val-imap-add-pairs-is-append2
;;   (implies (val-imap-p pairs)
;;            (equal (val-imap-add-pairs pairs x)
;;                   (append pairs (val-imap-fix x))))
;;   :hints(("Goal" :in-theory (enable val-imap-add-pairs
;;                                     val-imap-add))))

(fgl::def-fgl-rewrite record-type-fix-val-redef
  (equal (record-type-fix-val x fields)
         (val-imap-add-pairs (record-type-fix-val1 x fields) nil))
  :hints(("Goal" :in-theory (enable val-imap-add-pairs
                                    val-imap-add
                                    val-imap-lookup
                                    record-type-fix-val1)
          :induct t
          :expand ((record-type-fix-val1 x fields)
                   (record-type-fix-val x fields)))))



(fgl::remove-fgl-rewrite val-imap-p)
(fgl::add-fgl-rewrite val-imap-fix-when-val-imap-p)
(fgl::add-fgl-rewrite val-imap-p-of-v_record->rec)

(fgl::remove-fgl-rewrite val-imap-fix)


(local (in-theory (disable typed_identifierlist-lookup-under-iff)))

(local
 (encapsulate nil
   (defthmd record-type-satisfied-implies-satisfied
     (implies (and (record-type-satisfied rec fields)
                   (identifier-p fld)
                   (val-imap-p rec)
                   (typed_identifierlist-lookup fld fields))
              (ty-satisfied (val-imap-lookup fld rec) ;;(cdr (hons-assoc-equal fld rec))
                            (typed_identifierlist-lookup fld fields)))
     :hints(("Goal" :in-theory (enable typed_identifierlist-lookup
                                       val-imap-lookup
                                       omap::lookup)
             :expand ((record-type-satisfied rec fields))
             :induct (typed_identifierlist-lookup fld fields))))

   (defthmd record-type-satisfied-implies-val-imap-has-key
     (implies (and (record-type-satisfied rec fields)
                   (identifier-p fld)
                   (val-imap-p rec)
                   (typed_identifierlist-lookup fld fields))
              (val-imap-has-key fld rec))
     :hints(("Goal" :in-theory (enable typed_identifierlist-lookup
                                       val-imap-has-key)
             :expand ((record-type-satisfied rec fields))
             :induct (typed_identifierlist-lookup fld fields))))))


(fgl::def-fgl-rewrite record-lookup-by-bind-ty-satisfied
  (b* (((t_record r) (ty->desc ty)))
    (implies (and (bind-ty-satisfied ty x)
                  (syntaxp (fgl::fgl-object-case ty :g-concrete))
                  (type_desc-case (ty->desc ty) :t_record)
                  (identifier-p fld)
                  (typed_identifierlist-lookup fld r.fields))
             (val-imap-has-key fld (v_record->rec x))))
  :hints(("Goal" :in-theory (enable bind-ty-satisfied
                                    record-type-satisfied-implies-val-imap-has-key)
          :expand ((ty-satisfied x ty)))))

(fgl::def-fgl-rewrite enumarray-lookup-by-bind-ty-satisfied
  (b* (((t_array r) (ty->desc ty))
       ((arraylength_enum r.index)))
    (implies (and (bind-ty-satisfied ty x)
                  (syntaxp (fgl::fgl-object-case ty :g-concrete))
                  (type_desc-case (ty->desc ty) :t_array)
                  (array_index-case r.index :arraylength_enum)
                  (identifier-p fld))
             (iff (val-imap-has-key fld (v_record->rec x))
                  (member-equal fld r.index.elts))))
  :hints(("Goal" :in-theory (enable bind-ty-satisfied
                                    omap::assoc-iff-in-keys
                                    val-imap-has-key)
          :expand ((ty-satisfied x ty)))))

(define rec-extract-keys ((keys identifierlist-p)
                          (rec val-imap-p))
  :returns (alist)
  :verify-guards nil
  (if (atom keys)
      nil
    (cons (cons (identifier-fix (car keys)) (val-imap-lookup (car keys) rec))
          (rec-extract-keys (cdr keys) rec)))
  ///
  (local (defret hons-assoc-equal-of-<fn>
           (equal (hons-assoc-equal key alist)
                  (and (member-equal key (identifierlist-fix keys))
                       (cons key (val-imap-lookup key rec))))))

  (local (defthmd val-imap-has-key-is-in-of-keys
           (iff (val-imap-has-key key rec)
                (set::in (identifier-fix key) (omap::keys (val-imap-fix rec))))
           :hints(("Goal" :in-theory (enable val-imap-has-key
                                             omap::assoc-to-in-of-keys)))))

  (local (defthm val-imap-lookup-of-nil
           (equal (val-imap-lookup key nil) (val-fix nil))
           :hints(("Goal" :in-theory (enable val-imap-lookup)))))

  (local (defthm val-imap-lookup-when-has-no-key
           (implies (not (val-imap-has-key key x))
                    (equal (val-imap-lookup key x)
                           (val-fix nil)))
           :hints(("Goal" :in-theory (enable val-imap-has-key
                                             val-imap-lookup
                                             omap::lookup)))))

  (local (defthm in-of-equal-to-mergesort
           (implies (equal x (mergesort y))
                    (iff (in k x)
                         (member-equal k y)))
           :hints(("Goal" :in-theory (e/d (set::in-to-member)
                                          (member-equal))))))

  (defret val-imap-add-pairs-of-rec-extract-keys
    (implies (equal (omap::keys (val-imap-fix rec)) (set::mergesort (identifierlist-fix keys)))
             (equal (val-imap-add-pairs alist nil)
                    (val-imap-fix rec)))
    :hints(("Goal" :use ((:instance val-imap-lookup-diff-key-when-unequal
                          (x (val-imap-add-pairs (rec-extract-keys keys rec) nil))
                          (y (val-imap-fix rec))))
            :in-theory (disable rec-extract-keys)
            :do-not-induct t)
           (and stable-under-simplificationp
                '(:in-theory (enable val-imap-has-key-is-in-of-keys))))))


(fgl::def-fgl-rewrite v_record->rec-bind-ty-satisfied-enumarray
  (b* (((t_array r) (ty->desc ty))
       ((arraylength_enum r.index)))
    (implies (and (bind-ty-satisfied ty x)
                  (syntaxp (fgl::fgl-object-case ty :g-concrete))
                  (type_desc-case (ty->desc ty) :t_array)
                  (array_index-case r.index :arraylength_enum)
                  (identifier-p fld))
             (equal (v_record->rec x)
                    (val-imap-add-pairs (rec-extract-keys r.index.elts
                                                          (fgl::fgl-hide (v_record->rec x)))
                                        nil))))
  :hints(("Goal" :in-theory (e/d (bind-ty-satisfied))
          :expand ((ty-satisfied x ty)))))

(fgl::def-fgl-rewrite v_record->rec-bind-ty-satisfied-record
  (b* (((t_record r) (ty->desc ty)))
    (implies (and (bind-ty-satisfied ty x)
                  (syntaxp (fgl::fgl-object-case ty :g-concrete))
                  (type_desc-case (ty->desc ty) :t_record))
             (equal (v_record->rec x)
                    (val-imap-add-pairs (rec-extract-keys (typed_identifierlist->names r.fields)
                                                          (fgl::fgl-hide (v_record->rec x)))
                                        nil))))
  :hints(("Goal" :in-theory (enable bind-ty-satisfied)
          :expand ((ty-satisfied x ty)))))

(fgl::remove-fgl-rewrites v_record->rec-bind-ty-satisfied-record
                          v_record->rec-bind-ty-satisfied-enumarray)


(fgl::def-fgl-branch-merge merge-v_record->rec-bind-ty-satisfied-enumarray
  (b* (((t_array r) (ty->desc ty))
       ((arraylength_enum r.index)))
    (implies (and (bind-ty-satisfied ty x)
                  (syntaxp (fgl::fgl-object-case ty :g-concrete))
                  (type_desc-case (ty->desc ty) :t_array)
                  (array_index-case r.index :arraylength_enum)
                  (identifier-p fld))
             (equal (if test (v_record->rec x) y)
                    (let ((pairs
                           (val-imap-add-pairs (rec-extract-keys r.index.elts
                                                                 (fgl::fgl-hide (v_record->rec x)))
                                               nil)))
                      (if test pairs y)))))
  :hints(("Goal" :in-theory (e/d (bind-ty-satisfied))
          :expand ((ty-satisfied x ty)))))
(fgl::remove-fgl-branch-merge merge-v_record->rec-bind-ty-satisfied-enumarray)

(fgl::def-fgl-branch-merge merge-v_record->rec-bind-ty-satisfied-record
  (b* (((t_record r) (ty->desc ty)))
    (implies (and (bind-ty-satisfied ty x)
                  (syntaxp (fgl::fgl-object-case ty :g-concrete))
                  (type_desc-case (ty->desc ty) :t_record))
             (equal (if test (v_record->rec x) y)
                    (if test (val-imap-add-pairs (rec-extract-keys (typed_identifierlist->names r.fields)
                                                                   (fgl::fgl-hide (v_record->rec x)))
                                                 nil)
                      y))))
  :hints(("Goal" :in-theory (enable bind-ty-satisfied)
          :expand ((ty-satisfied x ty)))))
(fgl::remove-fgl-branch-merge merge-v_record->rec-bind-ty-satisfied-record)

;; (fgl::def-fgl-branch-merge merge-v_record->rec-bind-ty-satisfied-record-force
;;   (b* (((t_record r) (ty->desc ty)))
;;     (implies (and (bind-ty-satisfied ty x)
;;                   (syntaxp (fgl::fgl-object-case ty :g-concrete))
;;                   (type_desc-case (ty->desc ty) :t_record))
;;              (equal (if test (v_record->rec x) y)
;;                     (fgl::if! test (v_record->rec x) y))))
;;   :hints(("Goal" :in-theory (enable fgl::if!))))


;; (fgl::def-fgl-rewrite equal-v_record->rec-bind-ty-satisfied-enumarray
;;   (b* (((t_array r) (ty->desc ty))
;;        ((arraylength_enum r.index)))
;;     (implies (and (bind-ty-satisfied ty x)
;;                   (syntaxp (fgl::fgl-object-case ty :g-concrete))
;;                   (type_desc-case (ty->desc ty) :t_array)
;;                   (array_index-case r.index :arraylength_enum)
;;                   (identifier-p fld))
;;              (equal (equal (v_record->rec x) y)
;;                     (equal (val-imap-add-pairs (rec-extract-keys r.index.elts
;;                                                                  (fgl::fgl-hide (v_record->rec x)))
;;                                                nil)
;;                            y))))
;;   :hints(("Goal" :in-theory (e/d (bind-ty-satisfied))
;;           :expand ((ty-satisfied x ty)))))

;; (fgl::def-fgl-rewrite equal-v_record->rec-bind-ty-satisfied-record
;;   (b* (((t_record r) (ty->desc ty)))
;;     (implies (and (bind-ty-satisfied ty x)
;;                   (syntaxp (fgl::fgl-object-case ty :g-concrete))
;;                   (type_desc-case (ty->desc ty) :t_record))
;;              (equal (equal (v_record->rec x) y)
;;                     (equal (val-imap-add-pairs (rec-extract-keys (typed_identifierlist->names r.fields)
;;                                                                  (fgl::fgl-hide (v_record->rec x)))
;;                                                nil)
;;                            y))))
;;   :hints(("Goal" :in-theory (enable bind-ty-satisfied)
;;           :expand ((ty-satisfied x ty)))))


;; (fgl::def-fgl-rewrite equal-val-imap-put-bind-ty-satisfied-record
;;   (b* (((t_record r) (ty->desc ty)))
;;     (implies (and (bind-ty-satisfied ty x)
;;                   (syntaxp (fgl::fgl-object-case ty :g-concrete))
;;                   (type_desc-case (ty->desc ty) :t_record))
;;              (equal (equal (val-imap-put-pairs pairs (v_record->rec x)) y)
;;                     (equal (val-imap-add-pairs (rec-extract-keys (typed_identifierlist->names r.fields)
;;                                                                  (fgl::fgl-hide
;;                                                                   (val-imap-put-pairs pairs (v_record->rec x))))
;;                                                nil)
;;                            y))))
;;   :hints(("Goal" :in-theory (enable bind-ty-satisfied)
;;           :expand ((ty-satisfied x ty)))))


(fgl::add-fgl-rewrites ;; val-p-of-cdr-of-hons-assoc-equal-when-val-imap-p
                       val-imap-p-of-v_record->rec)


;; (fgl::def-fgl-brewrite bind-ty-satisfied-of-record-lookup
;;   (b* (((t_record r) (ty->desc ty)))
;;     (implies (and (bind-ty-satisfied ty x)
;;                   (syntaxp (fgl::fgl-object-case ty :g-concrete))
;;                   (type_desc-case (ty->desc ty) :t_record)
;;                   (identifier-p fld)
;;                   (equal type (typed_identifierlist-lookup fld r.fields)))
;;              (equal (bind-ty-satisfied type (val-imap-lookup fld (v_record->rec x)))
;;                     type)))
;;   :hints(("Goal" :in-theory (enable bind-ty-satisfied
;;                                     v_array-nth
;;                                     record-type-satisfied-implies-satisfied)
;;           :expand ((ty-satisfied x ty)))))






;; ----------------------------------------------------------------------------------
;; Val-imap-lookups-equal --
;; Often we want to show that an instruction only can modify certain global variables,
;; certain fields of a record, etc.  So we want to add something of the form
;; (implies (not (member-equal key '("PSTATE" "_R"))) ;; the globals that can be modified
;;          (equal (val-imap-lookup key new-storage)
;;                 (val-imap-lookup key storage))))

;; If there are counterexamples, we get into an unhappy situation with the
;; lookup of the key in new-storage. We either need to merge multiple types of
;; val object or one type with another object of indeterminate type -- we don't
;; know the shape of the value looked up from the original storage, since we're
;; not being specific about what the key is.

;; So instead we want to check for each variable that has been written in
;; new-storage that either it is one of the excluded keys that can be written,
;; or else its value is the same in new-storage and storage. To do this we
;; rephrase the above form as:
;; (implies (not (member-equal key '("PSTATE" "_R"))) ;; the globals that can be modified
;;          (val-imap-lookups-equal key new-storage storage))
;; where val-imap-lookups-equal is just the equality of the lookups of key in
;; the two storages.


(define val-imap-lookups-equal (key new-st st)
  :verify-guards nil
  (equal (val-imap-lookup key new-st)
         (val-imap-lookup key st))
  ///
  (fgl::remove-fgl-rewrite val-imap-lookups-equal)

  (fgl::def-fgl-rewrite val-imap-lookups-equal-concrete
    (equal (val-imap-lookups-equal (fgl::concrete key) new-st st)
           (equal (val-imap-lookup key new-st)
                  (val-imap-lookup key st)))))


(define one-or-zero-constraint-aux (key lst)
  :returns (mv ok memb)
  (if (atom lst)
      (mv t nil)
    (mv-let (rest-ok rest-memb) (one-or-zero-constraint-aux key (cdr lst))
      (mv (and (or (not (equal key (car lst)))
                   (not rest-memb))
               rest-ok)
          (or (equal key (car lst)) rest-memb))))
  ///
  (defret <fn>-is-member
    (iff memb (member-equal key lst)))

  (defret no-duplicatesp-implies-<fn>
    (implies (no-duplicatesp-equal lst)
             ok)))

(define one-or-zero-constraint (key lst)
  :returns ok
  (mv-let (ok memb) (one-or-zero-constraint-aux key lst)
    (declare (ignore memb))
    ok)
  ///
  (defret no-duplicatesp-implies-<fn>
    (implies (no-duplicatesp-equal lst)
             ok)))

(define add-one-or-zero-constraint (key lst)
  (declare (ignorable key lst))
  t
  ///
  (fgl::remove-fgl-rewrite add-one-or-zero-constraint))

(fgl::def-fgl-boolean-constraint trigger-one-or-zero-constraint
  :bindings ((tr (add-one-or-zero-constraint key lst)))
  :body (and tr
             (if (no-duplicatesp-equal lst)
                 (one-or-zero-constraint key lst)
               t)))


(define val-imap-key-lookups-equal-rec (key keys full-keys new-st st)
  :verify-guards nil
  (if (atom keys)
      (and (not (member-equal key full-keys))
           (equal (val-imap-lookup key new-st)
                  (val-imap-lookup key st)))
    (acl2::or* (and (equal key (car keys))
                    (val-imap-lookups-equal (car keys) new-st st))
               (val-imap-key-lookups-equal-rec key (cdr keys) full-keys new-st st)))
  ///
  (defthm val-imap-key-lookups-equal-rec-when-unequal
    (implies (not (equal (val-imap-lookup key new-st)
                         (val-imap-lookup key st)))
             (not (val-imap-key-lookups-equal-rec key keys full-keys new-st st)))
    :hints(("Goal" :in-theory (enable val-imap-lookups-equal))))

  (defthm val-imap-key-lookups-equal-rec-correct
    (equal (val-imap-key-lookups-equal-rec key keys full-keys new-st st)
           (and (or (member-equal key keys)
                    (not (member-equal key full-keys)))
                (val-imap-lookups-equal key new-st st)))
    :hints(("Goal" :in-theory (enable val-imap-lookups-equal)
            :induct t
            :expand ((:free (key) (val-imap-key-lookups-equal-rec key keys full-keys new-st st))))))

  (fgl::def-fgl-rewrite val-imap-lookups-equal-resolve
    (implies (syntaxp (not (fgl::fgl-object-case key :g-concrete)))
             (equal (val-imap-lookups-equal key (val-imap-put-pairs pairs st) st)
                    (let* ((written-keys (acl2::alist-keys pairs))
                           (ignore (fgl::trigger-constraints (add-one-or-zero-constraint key written-keys))))
                      (declare (ignore ignore))
                      (val-imap-key-lookups-equal-rec key written-keys written-keys (val-imap-put-pairs pairs st) st))))
    :hints(("Goal" :in-theory (enable val-imap-lookups-equal)))))



(fgl::def-ctrex-rule val-imap-lookup-ctrex-rule
  :match ((val (val-imap-lookup k x)))
  :assign (omap::update k val x)
  :assigned-var x
  :ruletype :property)

(fgl::add-fgl-rewrite val-imap-p-of-update)

(fgl::def-fgl-rewrite env-find-vars-fgl
  (equal (env-find-vars vars env)
         (if (atom vars)
      nil
    (b* ((first (env-find (car vars) env))
         (rest (env-find-vars (cdr vars) env)))
      (env_result-case first
        :lk_local (val-imap-add (car vars) first.val rest)
        :lk_global (val-imap-add (car vars) first.val rest)
        :otherwise rest))))
  :hints (("Goal" :in-theory (enable env-find-vars val-imap-add))))

(fgl::remove-fgl-rewrite env-find-vars)

(define identifierlist-filter (x)
  :returns (new-x identifierlist-p)
  (if (atom x)
      nil
    (if (identifier-p (car x))
        (cons (car x) (identifierlist-filter (cdr x)))
      (identifierlist-filter (cdr x))))
  ///
  (defret member-of-<fn>
    (iff (member-equal k new-x)
         (and (identifier-p k)
              (member-equal k x)))))

(fgl::def-fgl-rewrite val-imap-keys-of-val-imap-add-pairs
  (equal (val-imap-keys (val-imap-add-pairs pairs nil))
         (mergesort (identifierlist-filter (acl2::alist-keys pairs))))
  :hints(("Goal" :in-theory (enable pick-a-point-subset-strategy
                                    set::double-containment-no-backchain-limit))
         (set::pick-a-point-subset-hint
          id clause world stable-under-simplificationp)
         (and stable-under-simplificationp
              '(:in-theory (enable val-imap-has-key))))
  :otf-flg t)

(fgl::def-fgl-rewrite val-imap-keys-of-val-imap-put-pairs
  (equal (val-imap-keys (val-imap-put-pairs pairs base))
         (val-imap-keys base))
  :hints(("Goal" :in-theory (enable pick-a-point-subset-strategy
                                    set::double-containment-no-backchain-limit))
         (set::pick-a-point-subset-hint
          id clause world stable-under-simplificationp)
         (and stable-under-simplificationp
              '(:in-theory (enable val-imap-has-key))))
  :otf-flg t)

(fgl::enable-split-ifs val-imap-has-key)
(fgl::disable-if-merge-args val-imap-has-key)

(fgl::enable-split-ifs val-imap-lookup)
(fgl::disable-if-merge-args val-imap-lookup)

;; (fgl::disable-if-merge-args val-imap-add-pairs)
;; (fgl::disable-if-merge-args val-imap-put-pairs)

