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

(include-book "ast")
(include-book "../asl/utils/omaps")
(local (include-book "varset"))
(local (table fty::deftagsum-defaults :short-names t))
(local (std::add-default-post-define-hook :fix))

(deftagsum evttype
  (:evt-r ((addr natp :rule-classes :type-prescription)
           (val natp :rule-classes :type-prescription)
           ;; UID of corresponding write event
           (from natp :rule-classes :type-prescription)))
  (:evt-w ((addr natp :rule-classes :type-prescription)
           (val natp :rule-classes :type-prescription)
           (initp booleanp)))
  (:evt-b ())
  (:evt-f ()))

(defenum evtkind-p (:evt-r :evt-w :evt-b :evt-f))

(defthm evtkind-p-of-evttype-kind
  (evtkind-p (evttype-kind x))
  :hints(("Goal" :in-theory (enable evttype-kind))))

;; TODO flesh this out
(defprod evt
  ((uid natp :rule-classes :type-prescription)
   (procid natp :rule-classes :type-prescription)
   (po-index natp :rule-classes :type-prescription)
   (data evttype)))

(deflist evtlist :elt-type evt :true-listp t :elementp-of-nil nil
  ///
  (defthm evtlist-p-of-append
    (implies (and (evtlist-p x)
                  (evtlist-p y))
             (evtlist-p (append x y)))))


(defprod execgraph
  ((evts evtlist)))


(defprod evtpair
  ((from evt) (to evt))
  :layout :list)


(fty::defset relation :elt-type evtpair :elementp-of-nil nil)

(defprod fndef
  ((name var)
   (formals pat)
   (body exp)))
  
(deflist fndeflist :elt-type fndef :true-listp t :elementp-of-nil nil)


(deftypes val
  (deftagsum val
    ;; Empty; may be a set, valset, or relation, we don't need to distinguish.
    ;; This is useful for computing least fixpoints of relations, so that we
    ;; don't need to know what kind the starting variables should be.
    (:v_empty  ())
    (:v_tag    ((tag tag)))
    (:v_rel    ((rel relation)))
    (:v_set    ((evts evtlist))) ;; events are not values so this is a separate category than valset
    (:v_valset ((elts vallist))) ;; needs to satisfy well-formedness condition
    (:v_tuple  ((elts vallist))) ;; needs to have 0 or 2 or more elements
    (:v_enum   ((tags taglist)))
    (:v_fun    ((formals pat)
                (body exp)
                ;; Extension to represent the non-well-founded recursive function
                ;; binding form: if fndeflist is nonempty then this function
                ;; represents the infinite closure where env includes the bindings
                ;; of each name in recdefs to the corresponding closure that
                ;; includes that env itself.
                (recdefs fndeflist)
                (env env)))
    (:v_proc   ((formals pat)
                (body inslist)
                (env env))))

  (deflist vallist :elt-type val :true-listp t :elementp-of-nil nil)

  (fty::defomap env :key-type var :val-type val))


(defenum val-kind-p (:v_empty :v_tag :v_rel :v_set :v_valset :v_tuple :v_enum :v_fun :v_proc))
(defthm val-kind-p-of-val-kind
  (val-kind-p (val-kind x))
  :hints(("Goal" :in-theory (enable val-kind))))

(defenum judgement-p (:allowed :forbidden))

(defprod result
  ((judgement judgement-p)
   (flags varlist-p)
   (env env)
   (comrels varlist-p)))

(deflist resultlist :elt-type result :true-listp t :elementp-of-nil nil
  ///
  (defthm resultlist-p-of-append
    (implies (and (resultlist-p x)
                  (resultlist-p y))
             (resultlist-p (append x y)))))

(define vallist-same-kind-aux ((kind val-kind-p) (x vallist-p))
  (if (atom x)
      t
    (and (eq (val-kind (car x)) (val-kind-fix kind))
         (vallist-same-kind-aux kind (cdr x)))))

(define vallist-same-kind ((x vallist-p))
  (or (atom x)
      (vallist-same-kind-aux (val-kind (car x)) (cdr x))))
                               

(defines well-formed
  (define val-well-formed ((x val-p))
    :measure (val-count x)
    :hints ((and stable-under-simplificationp
                 '(:expand ((val-count x)))))
    (val-case x
      :v_valset (and (vallist-same-kind x.elts)
                     (vallist-well-formed x.elts))
      :v_tuple (and (or (atom x.elts)
                        (consp (cdr x.elts)))
                    (vallist-well-formed x.elts))
      :v_fun (env-well-formed x.env)
      :v_proc (env-well-formed x.env)
      :otherwise t))

  (define vallist-well-formed ((x vallist-p))
    :measure (vallist-count x)
    (if (atom x)
        t
      (and (val-well-formed (car x))
           (vallist-well-formed (cdr x)))))

  (define env-well-formed ((x env-p))
    :measure (env-count x)
    (b* ((x (env-fix x)))
      (if (omap::emptyp x)
          t
        (and (val-well-formed (omap::head-val x))
             (env-well-formed (omap::tail x))))))

  ///
  (fty::deffixequiv-mutual well-formed))
      
   

(define env-lookup ((v var-p) (env env-p))
  :returns (val (iff (val-p val) val))
  (cdr (omap::assoc (var-fix v) (env-fix env))))

(define env-update ((var var-p) (val val-p) (env env-p))
  :returns (new-env env-p)
  (omap::update (var-fix var) (val-fix val) (env-fix env))
  ///
  (defret env-lookup-of-<fn>
    (equal (env-lookup v new-env)
           (if (equal (var-fix v) (var-fix var))
               (val-fix val)
             (env-lookup v env)))
    :hints(("Goal" :in-theory (enable env-lookup)))))


(define env-extract ((vars varlist-p) (env env-p))
  :Returns (new-env env-p)
  :prepwork ((local (defthm env-p-of-omap-restrict
                      (implies (env-p env)
                               (env-p (omap::restrict keys env)))
                      :hints(("Goal" :in-theory (enable omap::restrict))))))
  (omap::restrict (mergesort (varlist-fix vars)) (env-fix env))
  ///
  (defret env-lookup-of-<fn>
    (equal (env-lookup v new-env)
           (and (member-equal (var-fix v) (varlist-fix vars))
                (env-lookup v env)))
    :hints(("Goal" :in-theory (enable env-lookup
                                      omap::assoc-of-restrict)))))


  
(local (defthm cdr-of-omap-assoc-env
         (implies (env-p env)
                  (iff (cdr (omap::assoc k env))
                       (omap::assoc k env)))
         :hints(("Goal" :in-theory (enable omap::assoc)))))

(define env-combine ((env1 env-p) (env2 env-p))
  :returns (new-env env-p)
  (omap::update* (env-fix env1) (env-fix env2))
  ///
  (defret env-lookup-of-<fn>
    (equal (env-lookup v new-env)
           (or (env-lookup v env1)
               (env-lookup v env2)))
    :hints(("Goal" :in-theory (enable env-lookup))))

  (defret env-combine-of-empty
    (equal (env-combine nil x) (env-fix x))
    :hints(("Goal" :in-theory (enable omap::update*)))))


(define env-diff-key ((x env-p) (y env-p))
  :returns (key (iff (var-p key) key))
  :verify-guards nil
  (let ((key (omap::diff-key (env-fix x) (env-fix y))))
    (and key (var-fix key)))
  ///
  (local (defthm diff-key-x-x
           (not (omap::diff-key x x))
           :hints(("Goal" :in-theory (enable omap::diff-key)))))
  (local (defthm key-var-p-when-assoc-env
           (implies (and (env-p env)
                         (not (var-p key)))
                    (not (omap::assoc key env)))))
  
  (local (defthm car-of-omap-assoc
           (equal (car (omap::assoc k y))
                  (and (omap::assoc k y) k))
           :hints(("Goal" :in-theory (enable omap::assoc)))))
                  
  (verify-guards env-diff-key
    :hints (("goal" :use ((:instance omap::diff-key-when-unequal
                           (x (env-fix x)) (y (env-fix y))))))
    :otf-flg t)
  
  (defretd env-diff-key-when-unequal
    (implies (not (equal (env-fix x) (env-fix y)))
             (not (equal (env-lookup key x)
                         (env-lookup key y))))
    :hints (("goal" :use ((:instance omap::diff-key-when-unequal
                           (x (env-fix x)) (y (env-fix y))))
             :in-theory (enable env-lookup)
             :cases ((var-p (omap::diff-key (env-fix x) (env-fix y)))))
            '(:cases ((omap::assoc (env-diff-key x y) (env-fix x))))
            '(:cases ((omap::assoc (env-diff-key x y) (env-fix y)))))))


(define env-keys ((x env-p))
  :returns (keys (and (setp keys)
                      (varlist-p keys)))
  :prepwork ((local (defthm varlist-p-keys-of-env
                      (implies (env-p env)
                               (varlist-p (omap::keys env)))
                      :hints(("Goal" :in-theory (enable omap::keys))))))
  (omap::keys (env-fix x))
  ///
  (defthmd in-env-keys-iff-env-lookup
    (iff (in v (env-keys x))
         (and (var-p v)
              (env-lookup v x)))
    :hints(("Goal" :in-theory (enable env-lookup
                                      omap::in-of-keys-to-assoc))))

  (defthm env-keys-of-env-update
    (equal (env-keys (env-update v val x))
           (insert (var-fix v) (env-keys x)))
    :hints(("Goal" :in-theory (enable env-update))))

  (defthm env-keys-of-env-combine
    (equal (env-keys (env-combine x y))
           (union (env-keys x) (env-keys y)))
    :hints(("Goal" :in-theory (enable env-combine)))))


