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

(include-book "ast")
(include-book "centaur/fty/visitor" :dir :system)
(include-book "centaur/fty/multicase" :dir :system)
(include-book "trace-interp")
(include-book "centaur/misc/dfs-seen-property" :dir :System)
(local (include-book "interp-theory"))
(local (include-book "std/util/defretgen" :dir :system))
(local (include-book "clause-processors/find-subterms" :dir :system))
(local (include-book "std/alists/alist-keys" :dir :system))
(local (include-book "std/lists/sets" :dir :system))
(local (std::add-default-post-define-hook :fix))

(fty::defprod call-sig
  ((fn identifier-p)
   (pos posn-p)))

;; Call-siglist: a list of call signatures, each sufficient to decide whether a
;; tracespec matches or not.
(fty::deflist call-siglist :elt-type call-sig :true-listp t :elementp-of-nil nil)


(fty::defvisitor-template all-callsigs ((x :object))
  :returns (calls (:join (append calls1 calls)
                   :tmp-var calls1
                   :initial nil)
                  call-siglist-p)
  :renames ((expr all-callsigs-expr-aux)
            (stmt all-callsigs-stmt-aux))
  :prod-fns ((t_bits (fields :skip)))
  :type-fns ((expr all-callsigs-expr)
             (stmt all-callsigs-stmt))
  :fnname-template all-callsigs-<type>)


;; Collect all call signatures present in an expression/type/etc.
(fty::defvisitor-multi all-callsigs-expr
  (define all-callsigs-expr ((x expr-p))
    :measure (acl2::nat-list-measure (list (expr-count x) 1))
    :returns (calls call-siglist-p)
    (b* (((expr x)))
      (expr_desc-case x.desc
        :e_call (cons (call-sig (call->name x.desc.call) x.pos_start)
                      (all-callsigs-expr-aux x))
        :otherwise (all-callsigs-expr-aux x))))

  (fty::defvisitors :template all-callsigs
    :types (expr)
    :measure (acl2::nat-list-measure (list :count 0))))


(set-bogus-measure-ok t)
(local (in-theory (enable maybe-stmt-some->val)))
(local (defthm maybe-stmt-count-when-stmt
         (implies x
                  (< (stmt-count x) (maybe-stmt-count x)))
         :hints(("Goal" :in-theory (enable maybe-stmt-count)))
         :rule-classes :linear))
;; (local
;;  (set-default-hints
;;   '((and stable-under-simplificationp '(:in-theory (enable maybe-stmt-some->val))))))


(fty::defvisitors all-callsigs-stmt-deps
  :template all-callsigs
  :dep-types (stmt)
  :types (ty-timeframe-imap)
  :measure (acl2::nat-list-measure (list :count 0)))

;; Collect all call signatures present in a statement.
(fty::defvisitor-multi all-callsigs-stmt
  (define all-callsigs-stmt ((x stmt-p))
    :measure (acl2::nat-list-measure (list (stmt-count x) 1))
    :returns (calls call-siglist-p)
    (b* (((stmt x)))
      (stmt_desc-case x.desc
        :s_call (cons (call-sig (call->name x.desc.call) x.pos_start)
                      (all-callsigs-stmt-aux x))
        :otherwise (all-callsigs-stmt-aux x))))

  (fty::defvisitors :template all-callsigs
    :types (stmt)
    :measure (acl2::nat-list-measure (list :count 0))))





(define call-siglist-filter-traced ((x call-siglist-p)
                                    (tracespec tracespec-p))
  :returns (new-x call-siglist-p)
  (b* (((when (atom x))
        nil)
       ((call-sig x1) (car x))
       ((when (find-call-tracespec x1.fn x1.pos tracespec))
        (cons (call-sig-fix x1)
              (call-siglist-filter-traced (cdr x) tracespec))))
    (call-siglist-filter-traced (cdr x) tracespec))
  ///
  (defthm call-siglist-filter-traced-of-append
    (equal (call-siglist-filter-traced (append x y) tracespec)
           (append (call-siglist-filter-traced x tracespec)
                   (call-siglist-filter-traced y tracespec))))

  (defthm call-siglist-filter-traced-of-cons
    (equal (call-siglist-filter-traced (cons (call-sig fn pos) rest) tracespec)
           (let ((rest (call-siglist-filter-traced rest tracespec)))
             (if (find-call-tracespec fn pos tracespec)
                 (cons (call-sig fn pos) rest)
               rest))))

  (defthm call-siglist-filter-traced-of-nil
    (equal (call-siglist-filter-traced nil tracespec) nil)))





(fty::defvisitor-template traced-callsigs ((x :object) (tracespec tracespec-p))
  :returns (calls (:join (append calls1 calls)
                   :tmp-var calls1
                   :initial nil)
                  call-siglist-p)
  :renames ((expr traced-callsigs-expr-aux)
            (stmt traced-callsigs-stmt-aux))
  :type-fns ((expr traced-callsigs-expr)
             (stmt traced-callsigs-stmt))
  :prod-fns ((t_bits (fields :skip)))
  :fnname-template traced-callsigs-<type>
  :defines-args (:flag-local nil))

;; Collects call signatures in an expression that match a tracespec.
(fty::defvisitor-multi traced-callsigs-expr
  (define traced-callsigs-expr ((x expr-p) (tracespec tracespec-p))
    :measure (acl2::nat-list-measure (list (expr-count x) 1))
    :returns (calls call-siglist-p)
    (b* (((expr x)))
      (expr_desc-case x.desc
        :e_call (b* ((fn (call->name x.desc.call))
                     (pos x.pos_start))
                  (if (find-call-tracespec fn pos tracespec)
                      (cons (call-sig fn pos)
                            (traced-callsigs-expr-aux x tracespec))
                    (traced-callsigs-expr-aux x tracespec)))
        :otherwise (traced-callsigs-expr-aux x tracespec))))

  (fty::defvisitors :template traced-callsigs
    :types (expr)
    :measure (acl2::nat-list-measure (list :count 0)))
  
  :defines-args (:flag-local nil))


(set-bogus-measure-ok t)
;; (local
;;  (set-default-hints
;;   '((and stable-under-simplificationp '(:in-theory (enable maybe-stmt-some->val))))))


(fty::defvisitors traced-callsigs-stmt-deps
  :template traced-callsigs
  :dep-types (stmt)
  :types (ty-timeframe-imap)
  :measure (acl2::nat-list-measure (list :count 0)))

;; Collects call signatures in a statement that match a tracespec.
(fty::defvisitor-multi traced-callsigs-stmt
  (define traced-callsigs-stmt ((x stmt-p) (tracespec tracespec-p))
    :measure (acl2::nat-list-measure (list (stmt-count x) 1))
    :returns (calls call-siglist-p)
    (b* (((stmt x)))
      (stmt_desc-case x.desc
        :s_call (b* ((fn (call->name x.desc.call))
                     (pos x.pos_start))
                  (if (find-call-tracespec fn pos tracespec)
                      (cons (call-sig fn pos)
                            (traced-callsigs-stmt-aux x tracespec))
                    (traced-callsigs-stmt-aux x tracespec)))
        :otherwise (traced-callsigs-stmt-aux x tracespec))))

  (fty::defvisitors :template traced-callsigs
    :types (stmt)
    :measure (acl2::nat-list-measure (list :count 0)))
  
  :defines-args (:flag-local nil))





(local (defthm traced-callsigs-maybe-stmt-of-nil
         (equal (traced-callsigs-maybe-stmt nil tracespec) nil)
         :hints (("goal" :expand ((traced-callsigs-maybe-stmt nil tracespec))))))


;; Generates proofs that traced-callsigs-<type> equals call-siglist-filter-traced of all-callsigs-<type>.
(std::defretgen <fn>-redef
  :rules (((not (or (:fnname traced-callsigs-expr-aux)
                    (:fnname traced-callsigs-stmt-aux)))
           (:each-formal :name x :action (:add-concl (equal calls
                                                            (call-siglist-filter-traced
                                                             (all-callsigs-<type> x)
                                                             tracespec)))))
          ((:fnname traced-callsigs-expr-aux)
           (:add-concl (equal calls
                              (call-siglist-filter-traced
                               (all-callsigs-expr-aux x)
                               tracespec))))
          ((:fnname traced-callsigs-stmt-aux)
           (:add-concl (equal calls
                              (call-siglist-filter-traced
                               (all-callsigs-stmt-aux x)
                               tracespec))))
          ((:fnname traced-callsigs-ty-timeframe-imap)
           (:add-keyword :hints (("goal" :induct t
                                   :in-theory (enable traced-callsigs-ty-timeframe-imap
                                                      all-callsigs-ty-timeframe-imap)))))
          ((not (:fnname traced-callsigs-ty-timeframe-imap))
           (:add-keyword :hints ('(:expand (<call>))
                                   (and stable-under-simplificationp
                                        (let* ((lit (car (last clause)))
                                               (calls (acl2::find-calls-of-fns
                                                       (append (getpropc 'all-callsigs-expr
                                                                         'acl2::recursivep
                                                                         nil world)
                                                               (getpropc 'all-callsigs-lexpr
                                                                         'acl2::recursivep
                                                                         nil world)
                                                               (getpropc 'all-callsigs-stmt
                                                                         'acl2::recursivep
                                                                         nil world)
                                                               '(all-callsigs-maybe-ty
                                                                 ;; all-callsigs-expr*maybe-ty
                                                                 ;; all-CALLSIGS-MAYBE-[EXPR*MAYBE-TY]
                                                                 all-CALLSIGS-MAYBE-EXPR
                                                                 all-callsigs-ty-timeframe
                                                                 all-callsigs-ty-timeframe-imap))
                                                       lit)))
                                          `(:expand (,@calls)
                                            ;; :do-not-induct t
                                            )))))))
  :functions (traced-callsigs-expr
              traced-callsigs-stmt
              traced-callsigs-maybe-ty
              ;; traced-callsigs-expr*maybe-ty
              ;; TRACED-CALLSIGS-MAYBE-[EXPR*MAYBE-TY]
              TRACED-CALLSIGS-MAYBE-EXPR
              traced-callsigs-lexpr
              traced-callsigs-ty-timeframe
              traced-callsigs-ty-timeframe-imap))






(fty::deflist stmtlist :elt-type stmt :true-listp t :elementp-of-nil nil)


(fty::defvisitor-template all-stmts ((x :object))
  :returns (stmts (:join (append stmts1 stmts)
                   :tmp-var stmts1
                   :initial nil)
                  stmtlist-p)
  :renames ((stmt all-stmts-stmt-aux))
  :type-fns ((stmt all-stmts-stmt))
  :fnname-template all-stmts-<type>)


;; Collects all substatements within a statement.
(fty::defvisitor-multi all-stmts-stmt
  (define all-stmts-stmt ((x stmt-p))
    :measure (acl2::nat-list-measure (list (stmt-count x) 1))
    :returns (new-stmts stmtlist-p)
    (cons (stmt-fix x)
          (all-stmts-stmt-aux x)))

  (fty::defvisitors :template all-stmts
    :types (stmt)
    :measure (acl2::nat-list-measure (list :count 0))))


(define stmtlist-filter-traced ((x stmtlist-p)
                                (tracespec tracespec-p))
  :returns (new-x stmtlist-p)
  (b* (((when (atom x))
        nil)
       ((when (find-stmt-tracespec (car x) tracespec))
        (cons (stmt-fix (car x))
              (stmtlist-filter-traced (cdr x) tracespec))))
    (stmtlist-filter-traced (cdr x) tracespec))
  ///
  (defthm stmtlist-filter-traced-of-append
    (equal (stmtlist-filter-traced (append x y) tracespec)
           (append (stmtlist-filter-traced x tracespec)
                   (stmtlist-filter-traced y tracespec))))

  (defthm stmtlist-filter-traced-of-cons
    (equal (stmtlist-filter-traced (cons x1 rest) tracespec)
           (let ((rest (stmtlist-filter-traced rest tracespec)))
             (if (find-stmt-tracespec x1 tracespec)
                 (cons (stmt-fix x1) rest)
               rest))))

  (defthm stmtlist-filter-traced-of-nil
    (equal (stmtlist-filter-traced nil tracespec) nil)))



(fty::defvisitor-template traced-stmts ((x :object) (tracespec tracespec-p))
  :returns (stmts (:join (append stmts1 stmts)
                   :tmp-var stmts1
                   :initial nil)
                  stmtlist-p)
  :renames ((stmt traced-stmts-stmt-aux))
  :type-fns ((stmt traced-stmts-stmt))
  :fnname-template traced-stmts-<type>)

;; Collects substatements of a statement that match a particular tracespec.
(fty::defvisitor-multi traced-stmts-stmt
  (define traced-stmts-stmt ((x stmt-p) (tracespec tracespec-p))
    :measure (acl2::nat-list-measure (list (stmt-count x) 1))
    :returns (stmts stmtlist-p)
    (if (find-stmt-tracespec x tracespec)
        (cons (stmt-fix x)
              (traced-stmts-stmt-aux x tracespec))
      (traced-stmts-stmt-aux x tracespec)))

  (fty::defvisitors :template traced-stmts
    :types (stmt)
    :measure (acl2::nat-list-measure (list :count 0)))

  :defines-args (:flag-local nil))


(local (defthm traced-stmts-maybe-stmt-of-nil
         (equal (traced-stmts-maybe-stmt nil tracespec) nil)
         :hints (("goal" :expand ((traced-stmts-maybe-stmt nil tracespec))))))

;; Proves that traced-stmts-<type> equals stmtlist-filter-traced of all-stmts-<type>.
(std::defretgen <fn>-redef
  :rules (((not (:fnname traced-stmts-stmt-aux))
           (:each-formal :name x :action (:add-concl (equal stmts
                                                            (stmtlist-filter-traced
                                                             (all-stmts-<type> x)
                                                             tracespec)))))
          ((:fnname traced-stmts-stmt-aux)
           (:add-concl (equal stmts
                              (stmtlist-filter-traced
                               (all-stmts-stmt-aux x)
                               tracespec))))
          (t (:add-keyword :hints ('(:expand (<call>))
                                   (and stable-under-simplificationp
                                        (let* ((lit (car (last clause)))
                                               (calls (acl2::find-calls-of-fns
                                                       (getpropc 'all-stmts-stmt
                                                                 'acl2::recursivep
                                                                 nil world)
                                                       lit)))
                                          `(:expand (,@calls)
                                            :do-not-induct t
                                            )))))))
  :functions (traced-stmts-stmt))

;; Collects callsigs of all calls within a function's body.
(define all-callsigs-func ((x func-p))
  :returns (calls call-siglist-p)
  (b* (((func x)))
    (append (all-callsigs-maybe-expr x.recurse_limit)
            (subprogram_body-case x.body
              :sb_asl (all-callsigs-stmt x.body.stmt)
              :otherwise nil))))




(local (defthm len-set-difference-of-cons
         (<= (len (set-difference$ a (cons x b)))
             (len (set-difference$ a b)))
         :hints(("Goal" :in-theory (enable set-difference$)))
         :rule-classes :linear))

(local (defthm len-set-difference-of-cons-when-not-member
         (implies (and (member x a)
                       (not (member x b)))
                  (< (len (set-difference$ a (cons x b)))
                     (len (set-difference$ a b))))
         :hints(("Goal" :in-theory (enable set-difference$)))
         :rule-classes :linear))

(local (defthm len-set-difference-when-subsetp
         (implies (subsetp-equal b c)
                  (<= (len (set-difference$ a c))
                      (len (set-difference$ a b))))
         :hints(("Goal" :in-theory (enable set-difference$)))
         :rule-classes :linear))

(local (defthm alist-keys-of-cons
         (Equal (acl2::alist-keys (cons (cons key val) x))
                (cons key (acl2::alist-keys x)))
         :hints(("Goal" :in-theory (enable acl2::alist-keys)))))



;; Given a static environment, collects all call signatures in the body of a (named) function.
(define all-callsigs-fnname ((fn identifier-p)
                             (static static_env_global-p))
  :returns (callsigs call-siglist-p)
  (mbe :logic (all-callsigs-func
               (func-ses->fn (cdr (hons-assoc-equal (identifier-fix fn)
                                                    (static_env_global->subprograms static)))))
       :exec (b* ((look (hons-assoc-equal fn (static_env_global->subprograms static))))
               (and look
                    (all-callsigs-func (func-ses->fn (cdr look))))))
  ///
  (defret <fn>-when-not-bound
    (implies (not (hons-assoc-equal (identifier-fix fn)
                                    (static_env_global->subprograms static)))
             (not callsigs)))

  (fty::deffixequiv all-callsigs-fnname))

(define all-callsigs-fnnames ((fns identifierlist-p)
                              (static static_env_global-p))
  :returns (callsigs call-siglist-p)
  (if (atom fns)
      nil
    (append (all-callsigs-fnname (car fns) static)
            (all-callsigs-fnnames (cdr fns) static))))

;; Extract the function names from a list of call signatures.
(define call-siglist->fns ((x call-siglist-p))
  :returns (fns identifierlist-p)
  (if (atom x)
      nil
    (cons (call-sig->fn (car x))
          (call-siglist->fns (cdr x)))))


;; Depth-first-search to find all calls that can occur within a function and its subfunctions. 
(defines all-callsigs-rec
  (define all-callsigs-rec ((x call-sig-p)
                            (static static_env_global-p)
                            (seen)
                            (acc call-siglist-p))
    :measure (list (len (set-difference-equal (acl2::alist-keys (static_env_global->subprograms static))
                                              (acl2::alist-keys seen)))
                   0 1)
    :hints (("goal" :cases (hons-assoc-equal (call-sig->fn x)
                                             (static_env_global->subprograms static))))
    :well-founded-relation acl2::nat-list-<
    ;; (acl2::two-nats-measure
    ;; (len (set-difference-equal (acl2::alist-keys (static_env_global->subprograms static))
    ;;                            (acl2::alist-keys seen)))
    ;; 0)
    :returns (mv (new-seen)
                 (new-acc call-siglist-p))
    :verify-guards nil
    :prepwork ((local
                (defret <fn>-when-not-bound-inverse
                  (implies (and callsigs
                                (identifier-p fn))
                           (hons-assoc-equal fn
                                             (static_env_global->subprograms static)))
                  :fn all-callsigs-fnname)))
    (b* (((call-sig x))
         ((when (hons-get x.fn seen)) (mv seen (call-siglist-fix acc)))
         (seen (hons-acons x.fn t seen))
         (calls (all-callsigs-fnname x.fn static))
         (acc (append calls (call-siglist-fix acc))))
      (all-callsigs-list-rec calls static seen acc)))

  (define all-callsigs-list-rec ((x call-siglist-p)
                                 (static static_env_global-p)
                                 (seen)
                                 (acc call-siglist-p))
    :measure ;; (acl2::two-nats-measure
    ;;  (len (set-difference-equal (acl2::alist-keys (static_env_global->subprograms static))
    ;;                             (acl2::alist-keys seen)))
    ;;  (len x))
    (list (len (set-difference-equal (acl2::alist-keys (static_env_global->subprograms static))
                                     (acl2::alist-keys seen)))
          (len x) 0)
    ;; (acl2::dfs-measure x (static_env_global->subprograms static) seen)
    :returns (mv (new-seen)
                 (new-acc call-siglist-p))
    (b* (((when (atom x)) (mv seen (call-siglist-fix acc)))
         ((mv seen1 acc)
          (all-callsigs-rec (car x) static seen acc))
         ((unless (mbt (acl2::subsetp-equal (acl2::alist-keys seen)
                                            (acl2::alist-keys seen1))))
          (mv seen1 (call-siglist-fix acc))))
      (all-callsigs-list-rec (cdr x) static seen1 acc)))
  ///

  (fty::deffixequiv-mutual all-callsigs-rec)
  (local (include-book "std/lists/sets" :dir :system))

  (std::defret-mutual <fn>-seen-subsetp
    (defret <fn>-seen-subsetp
      (subsetp-equal (acl2::alist-keys seen) (acl2::alist-keys new-seen))
      :hints ('(:expand (<call>)))
      :fn all-callsigs-rec)
    (defret <fn>-seen-subsetp
      (subsetp-equal (acl2::alist-keys seen) (acl2::alist-keys new-seen))
      :hints ('(:expand (<call>)))
      :fn all-callsigs-list-rec))

  (verify-guards all-callsigs-rec)
  
  (std::defret-mutual <fn>-subsetp
    (defret <fn>-subsetp
      (subsetp-equal (call-siglist-fix acc) new-acc)
      :hints ('(:expand (<call>)))
      :fn all-callsigs-rec)
    (defret <fn>-subsetp
      (subsetp-equal (call-siglist-fix acc) new-acc)
      :hints ('(:expand (<call>)))
      :fn all-callsigs-list-rec))

  (std::defret-mutual <fn>-subsetp-no-fix
    (defret <fn>-subsetp-no-fix
      (implies (call-siglist-p acc)
               (subsetp-equal acc new-acc))
      :hints ('(:expand (<call>)))
      :fn all-callsigs-rec)
    (defret <fn>-subsetp-no-fix
      (implies (call-siglist-p acc)
               (subsetp-equal acc new-acc))
      :hints ('(:expand (<call>)))
      :fn all-callsigs-list-rec))

  (std::defret-mutual <fn>-seen-keys-identifierlist-p
    (defret <fn>-seen-keys-identifierlist-p
      (implies (identifierlist-p (acl2::alist-keys seen))
               (identifierlist-p (acl2::alist-keys new-seen)))
      :hints ('(:expand (<call>)))
      :fn all-callsigs-rec)
    (defret <fn>-seen-keys-identifierlist-p
      (implies (identifierlist-p (acl2::alist-keys seen))
               (identifierlist-p (acl2::alist-keys new-seen)))
      :hints ('(:expand (<call>)))
      :fn all-callsigs-list-rec))

  (defret <fn>-subsetp-trans
    (implies (subsetp-equal y (call-siglist-fix acc))
             (subsetp-equal y new-acc))
    :hints (("goal" :use <fn>-subsetp
             :in-theory (disable <fn>-subsetp)))
    :fn all-callsigs-rec)

  (defret <fn>-subsetp-trans
    (implies (subsetp-equal y (call-siglist-fix acc))
             (subsetp-equal y new-acc))
    :hints (("goal" :use <fn>-subsetp
             :in-theory (disable <fn>-subsetp)))
    :fn all-callsigs-list-rec)

  (std::defret-mutual <fn>-accumulates-calls-of-newly-seen
    (defret <fn>-accumulates-calls-of-newly-seen
      (implies (and (hons-assoc-equal (identifier-fix fn) new-seen)
                    (not (hons-assoc-equal (identifier-fix fn) seen)))
               (subsetp-equal (all-callsigs-fnname fn static) new-acc))
      :hints ('(:expand (<call>)))
      :fn all-callsigs-rec)
    (defret <fn>-accumulates-calls-of-newly-seen
      (implies (and (hons-assoc-equal (identifier-fix fn) new-seen)
                    (not (hons-assoc-equal (identifier-fix fn) seen)))
               (subsetp-equal (all-callsigs-fnname fn static) new-acc))
      :hints ('(:expand (<call>)))
      :fn all-callsigs-list-rec))

  (local
   (progn
     (defun-sk all-callsigs-rec-seen-normalize-acc-cond (x static seen)
       (forall acc
               (implies (syntaxp (not (equal acc ''nil)))
                        (equal (mv-nth 0 (all-callsigs-rec x static seen acc))
                               (mv-nth 0 (all-callsigs-rec x static seen nil)))))
       :rewrite :direct)

     (defun-sk all-callsigs-list-rec-seen-normalize-acc-cond (x static seen)
       (forall acc
               (implies (syntaxp (not (equal acc ''nil)))
                        (equal (mv-nth 0 (all-callsigs-list-rec x static seen acc))
                               (mv-nth 0 (all-callsigs-list-rec x static seen nil)))))
       :rewrite :direct)

     (local (in-theory (disable all-callsigs-rec-seen-normalize-acc-cond
                                all-callsigs-list-rec-seen-normalize-acc-cond)))
  
     (std::defret-mutual <fn>-seen-normalize-acc-lemma
       (defret <fn>-seen-normalize-acc-lemma
         (all-callsigs-rec-seen-normalize-acc-cond x static seen)
         :hints ('(:expand ((all-callsigs-rec-seen-normalize-acc-cond x static seen)
                            (:free (acc) <call>))))
         :fn all-callsigs-rec)
       (defret <fn>-seen-normalize-acc-lemma
         (all-callsigs-list-rec-seen-normalize-acc-cond x static seen)
         :hints ('(:expand ((all-callsigs-list-rec-seen-normalize-acc-cond x static seen)
                            (:free (acc) <call>))))
         :fn all-callsigs-list-rec))))

  (defret <fn>-seen-normalize-acc
      (implies (syntaxp (not (equal acc ''nil)))
               (equal new-seen
                      (let ((acc nil)) (mv-nth 0 <call>))))
      :hints (("goal" :use <fn>-seen-normalize-acc-lemma
               :in-theory (disable <fn>-seen-normalize-acc-lemma)))
      :fn all-callsigs-rec)
  
  (defret <fn>-seen-normalize-acc
    (implies (syntaxp (not (equal acc ''nil)))
             (equal new-seen
                    (let ((acc nil)) (mv-nth 0 <call>))))
    :hints (("goal" :use <fn>-seen-normalize-acc-lemma
             :in-theory (disable <fn>-seen-normalize-acc-lemma)))
    :fn all-callsigs-list-rec)
  
  (local
   (defund graph-nodes-is-subprograms (static)
     (equal (acl2::graph-nodes)
            (acl2::alist-keys (static_env_global->subprograms static)))))

  (defun-sk graph-node-succs-is-subprogram-calls (static)
    (forall x
            (implies (identifier-p x)
                     (equal (acl2::graph-node-succs x)
                            (call-siglist->fns (all-callsigs-fnname x static)))))
    :rewrite :direct)
  (in-theory (disable graph-node-succs-is-subprogram-calls
                      graph-node-succs-is-subprogram-calls-necc))

  (local (in-theory (enable graph-node-succs-is-subprogram-calls-necc)))
  
  (local 
   (std::defret-mutual <fn>-seen-is-dfs-traverse
     (defret <fn>-seen-is-dfs-traverse
       (implies (graph-node-succs-is-subprogram-calls static)
                (equal (acl2::alist-keys new-seen)
                       (acl2::dfs-traverse-node (call-sig->fn x) (acl2::alist-keys seen))))
       :hints ('(:expand (<call>
                          (:free (seen) (acl2::dfs-traverse-node (call-sig->fn x) seen))
                          (:free (seen) (acl2::dfs-traverse-list nil seen)))))
       :fn all-callsigs-rec)
     (defret <fn>-seen-is-dfs-traverse
       (implies (graph-node-succs-is-subprogram-calls static)
                (equal (acl2::alist-keys new-seen)
                       (acl2::dfs-traverse-list (call-siglist->fns x) (acl2::alist-keys seen))))
       :hints ('(:expand (<call>
                          (call-siglist->fns x)
                          (:free (x y seen) (acl2::dfs-traverse-list (cons x y) seen))
                          (:free (seen) (acl2::dfs-traverse-list nil seen)))))
       :fn all-callsigs-list-rec)))

  (local
   (defret <fn>-top-seen-collects-successors-lemma
     :pre-bind ((seen nil))
     (implies (graph-node-succs-is-subprogram-calls static)
              (acl2::set-equiv (acl2::alist-keys new-seen)
                               (cons (call-sig->fn x)
                                     (acl2::alist-keys (mv-nth 0 (all-callsigs-list-rec
                                                                  (all-callsigs-fnname (call-sig->fn x) static)
                                                                  static nil nil))))))
     :fn all-callsigs-rec))

  (local
   (defret <fn>-top-seen-collects-successors-lemma
     :pre-bind ((seen nil))
     (implies (graph-node-succs-is-subprogram-calls static)
              (acl2::set-equiv (acl2::alist-keys new-seen)
                               (if (atom x)
                                   nil
                                 (append (acl2::alist-keys (mv-nth 0 (all-callsigs-rec (car x) static nil nil)))
                                         (acl2::alist-keys (mv-nth 0 (all-callsigs-list-rec (cdr x) static nil nil)))))))
     :hints(("Goal" :in-theory (e/d ()
                                    (acl2::set-equiv))
             :expand ((:with acl2::dfs-traverse-list-rec
                       (:free (a b) (acl2::dfs-traverse-list (cons a b) nil)))
                      (call-siglist->fns x))))
     :fn all-callsigs-list-rec))

  (local (defun-sk crock (static0 static)
           (forall x
                   (implies (identifier-p x)
                            (equal (call-siglist->fns (all-callsigs-fnname x
                                                                           static0))
                                   (call-siglist->fns (all-callsigs-fnname x static)))))
           :rewrite :direct))
  (local (in-theory (disable crock graph-node-succs-is-subprogram-calls)))
  
  
  (defretd <fn>-top-seen-collects-successors
    :pre-bind ((seen nil)
               (static static0))
    (acl2::set-equiv (acl2::alist-keys new-seen)
                     (cons (call-sig->fn x)
                           (acl2::alist-keys (mv-nth 0 (all-callsigs-list-rec
                                                        (all-callsigs-fnname (call-sig->fn x) static0)
                                                        static0 nil nil)))))
    :hints (("goal" :use ((:instance
                           (:functional-instance
                            all-callsigs-rec-top-seen-collects-successors-lemma
                            (acl2::graph-nodes (lambda () (acl2::alist-keys (static_env_global->subprograms static0))))
                            (acl2::graph-node-succs (lambda (x)
                                                      (and (identifier-p x)
                                                           (call-siglist->fns (all-callsigs-fnname x static0)))))
                            (graph-node-succs-is-subprogram-calls (lambda (static) (crock static0 static)))
                            (graph-node-succs-is-subprogram-calls-witness (lambda (static) (crock-witness static0 static))))
                           (static static0))))
            (and stable-under-simplificationp
                 (let ((lit (assoc 'crock clause)))
                   (and lit `(:in-theory (enable crock))))))
    :fn all-callsigs-rec
    :otf-flg t)

  (defretd <fn>-top-seen-collects-successors
    :pre-bind ((seen nil)
               (static static0))
    (acl2::set-equiv (acl2::alist-keys new-seen)
                     (if (atom x)
                         nil
                       (append (acl2::alist-keys (mv-nth 0 (all-callsigs-rec (car x) static nil nil)))
                               (acl2::alist-keys (mv-nth 0 (all-callsigs-list-rec (cdr x) static nil nil))))))
    :hints (("goal" :use ((:instance
                           (:functional-instance
                            all-callsigs-list-rec-top-seen-collects-successors-lemma
                            (acl2::graph-nodes (lambda () (acl2::alist-keys (static_env_global->subprograms static0))))
                            (acl2::graph-node-succs (lambda (x)
                                                      (and (identifier-p x)
                                                           (call-siglist->fns (all-callsigs-fnname x static0)))))
                            (graph-node-succs-is-subprogram-calls (lambda (static) (crock static0 static)))
                            (graph-node-succs-is-subprogram-calls-witness (lambda (static) (crock-witness static0 static))))
                           (static static0))))
            (and stable-under-simplificationp
                 (let ((lit (assoc 'crock clause)))
                   (and lit `(:in-theory (enable crock))))))
    :fn all-callsigs-list-rec
    :otf-flg t))





;; Depth-first-search to find all functions that can be called within a function and its subfunctions.
(defines all-subfunctions-rec
  (define all-subfunctions-rec ((x call-sig-p)
                                (static static_env_global-p)
                                (seen))
    :measure (list (len (set-difference-equal (acl2::alist-keys (static_env_global->subprograms static))
                                              (acl2::alist-keys seen)))
                   0 1)
    :hints (("goal" :cases (hons-assoc-equal (call-sig->fn x)
                                             (static_env_global->subprograms static))))
    :well-founded-relation acl2::nat-list-<
    ;; (acl2::two-nats-measure
    ;; (len (set-difference-equal (acl2::alist-keys (static_env_global->subprograms static))
    ;;                            (acl2::alist-keys seen)))
    ;; 0)
    :returns (new-seen)
    :verify-guards nil
    :prepwork ((local
                (defret <fn>-when-not-bound-inverse
                  (implies (and callsigs
                                (identifier-p fn))
                           (hons-assoc-equal fn
                                             (static_env_global->subprograms static)))
                  :fn all-callsigs-fnname)))
    (b* (((call-sig x))
         ((when (hons-get x.fn seen)) seen)
         (seen (hons-acons x.fn t seen))
         (calls (all-callsigs-fnname x.fn static)))
      (all-subfunctions-list-rec calls static seen)))

  (define all-subfunctions-list-rec ((x call-siglist-p)
                                     (static static_env_global-p)
                                     (seen))
    :measure ;; (acl2::two-nats-measure
    ;;  (len (set-difference-equal (acl2::alist-keys (static_env_global->subprograms static))
    ;;                             (acl2::alist-keys seen)))
    ;;  (len x))
    (list (len (set-difference-equal (acl2::alist-keys (static_env_global->subprograms static))
                                     (acl2::alist-keys seen)))
          (len x) 0)
    ;; (acl2::dfs-measure x (static_env_global->subprograms static) seen)
    :returns (new-seen)
    (b* (((when (atom x)) seen)
         (seen1
          (all-subfunctions-rec (car x) static seen))
         ((unless (mbt (acl2::subsetp-equal (acl2::alist-keys seen)
                                            (acl2::alist-keys seen1))))
          seen1))
      (all-subfunctions-list-rec (cdr x) static seen1)))
  ///

  (fty::deffixequiv-mutual all-subfunctions-rec)
  (local (include-book "std/lists/sets" :dir :system))

  (std::defret-mutual <fn>-seen-subsetp
    (defret <fn>-seen-subsetp
      (subsetp-equal (acl2::alist-keys seen) (acl2::alist-keys new-seen))
      :hints ('(:expand (<call>)))
      :fn all-subfunctions-rec)
    (defret <fn>-seen-subsetp
      (subsetp-equal (acl2::alist-keys seen) (acl2::alist-keys new-seen))
      :hints ('(:expand (<call>)))
      :fn all-subfunctions-list-rec))

  (verify-guards all-subfunctions-rec)
  

  (std::defret-mutual <fn>-seen-keys-identifierlist-p
    (defret <fn>-seen-keys-identifierlist-p
      (implies (identifierlist-p (acl2::alist-keys seen))
               (identifierlist-p (acl2::alist-keys new-seen)))
      :hints ('(:expand (<call>)))
      :fn all-subfunctions-rec)
    (defret <fn>-seen-keys-identifierlist-p
      (implies (identifierlist-p (acl2::alist-keys seen))
               (identifierlist-p (acl2::alist-keys new-seen)))
      :hints ('(:expand (<call>)))
      :fn all-subfunctions-list-rec))
  
  (local
   (defund graph-nodes-is-subprograms (static)
     (equal (acl2::graph-nodes)
            (acl2::alist-keys (static_env_global->subprograms static)))))

  (defun-sk graph-node-succs-is-subprogram-calls (static)
    (forall x
            (implies (identifier-p x)
                     (equal (acl2::graph-node-succs x)
                            (call-siglist->fns (all-callsigs-fnname x static)))))
    :rewrite :direct)
  (in-theory (disable graph-node-succs-is-subprogram-calls
                      graph-node-succs-is-subprogram-calls-necc))

  (local (in-theory (enable graph-node-succs-is-subprogram-calls-necc)))
  
  (local 
   (std::defret-mutual <fn>-seen-is-dfs-traverse
     (defret <fn>-seen-is-dfs-traverse
       (implies (graph-node-succs-is-subprogram-calls static)
                (equal (acl2::alist-keys new-seen)
                       (acl2::dfs-traverse-node (call-sig->fn x) (acl2::alist-keys seen))))
       :hints ('(:expand (<call>
                          (:free (seen) (acl2::dfs-traverse-node (call-sig->fn x) seen))
                          (:free (seen) (acl2::dfs-traverse-list nil seen)))))
       :fn all-subfunctions-rec)
     (defret <fn>-seen-is-dfs-traverse
       (implies (graph-node-succs-is-subprogram-calls static)
                (equal (acl2::alist-keys new-seen)
                       (acl2::dfs-traverse-list (call-siglist->fns x) (acl2::alist-keys seen))))
       :hints ('(:expand (<call>
                          (call-siglist->fns x)
                          (:free (x y seen) (acl2::dfs-traverse-list (cons x y) seen))
                          (:free (seen) (acl2::dfs-traverse-list nil seen)))))
       :fn all-subfunctions-list-rec)))

  (local
   (defret <fn>-top-seen-collects-successors-lemma
     :pre-bind ((seen nil))
     (implies (graph-node-succs-is-subprogram-calls static)
              (acl2::set-equiv (acl2::alist-keys new-seen)
                               (cons (call-sig->fn x)
                                     (acl2::alist-keys (all-subfunctions-list-rec
                                                        (all-callsigs-fnname (call-sig->fn x) static)
                                                        static nil)))))
     :fn all-subfunctions-rec))

  (local
   (defret <fn>-top-seen-collects-successors-lemma
     :pre-bind ((seen nil))
     (implies (graph-node-succs-is-subprogram-calls static)
              (acl2::set-equiv (acl2::alist-keys new-seen)
                               (if (atom x)
                                   nil
                                 (append (acl2::alist-keys (all-subfunctions-rec (car x) static nil))
                                         (acl2::alist-keys (all-subfunctions-list-rec (cdr x) static nil))))))
     :hints(("Goal" :in-theory (e/d ()
                                    (acl2::set-equiv))
             :expand ((:with acl2::dfs-traverse-list-rec
                       (:free (a b) (acl2::dfs-traverse-list (cons a b) nil)))
                      (call-siglist->fns x))))
     :fn all-subfunctions-list-rec))

  (local (defun-sk crock (static0 static)
           (forall x
                   (implies (identifier-p x)
                            (equal (call-siglist->fns (all-callsigs-fnname x
                                                                           static0))
                                   (call-siglist->fns (all-callsigs-fnname x static)))))
           :rewrite :direct))
  (local (in-theory (disable crock graph-node-succs-is-subprogram-calls)))
  
  
  (defretd <fn>-top-seen-collects-successors
    :pre-bind ((seen nil)
               (static static0))
    (acl2::set-equiv (acl2::alist-keys new-seen)
                     (cons (call-sig->fn x)
                           (acl2::alist-keys (all-subfunctions-list-rec
                                              (all-callsigs-fnname (call-sig->fn x) static0)
                                              static0 nil))))
    :hints (("goal" :use ((:instance
                           (:functional-instance
                            all-subfunctions-rec-top-seen-collects-successors-lemma
                            (acl2::graph-nodes (lambda () (acl2::alist-keys (static_env_global->subprograms static0))))
                            (acl2::graph-node-succs (lambda (x)
                                                      (and (identifier-p x)
                                                           (call-siglist->fns (all-callsigs-fnname x static0)))))
                            (graph-node-succs-is-subprogram-calls (lambda (static) (crock static0 static)))
                            (graph-node-succs-is-subprogram-calls-witness (lambda (static) (crock-witness static0 static))))
                           (static static0))))
            (and stable-under-simplificationp
                 (let ((lit (assoc 'crock clause)))
                   (and lit `(:in-theory (enable crock))))))
    :fn all-subfunctions-rec
    :otf-flg t)

  (defretd <fn>-top-seen-collects-successors
    :pre-bind ((seen nil)
               (static static0))
    (acl2::set-equiv (acl2::alist-keys new-seen)
                     (if (atom x)
                         nil
                       (append (acl2::alist-keys (all-subfunctions-rec (car x) static nil))
                               (acl2::alist-keys (all-subfunctions-list-rec (cdr x) static nil)))))
    :hints (("goal" :use ((:instance
                           (:functional-instance
                            all-subfunctions-list-rec-top-seen-collects-successors-lemma
                            (acl2::graph-nodes (lambda () (acl2::alist-keys (static_env_global->subprograms static0))))
                            (acl2::graph-node-succs (lambda (x)
                                                      (and (identifier-p x)
                                                           (call-siglist->fns (all-callsigs-fnname x static0)))))
                            (graph-node-succs-is-subprogram-calls (lambda (static) (crock static0 static)))
                            (graph-node-succs-is-subprogram-calls-witness (lambda (static) (crock-witness static0 static))))
                           (static static0))))
            (and stable-under-simplificationp
                 (let ((lit (assoc 'crock clause)))
                   (and lit `(:in-theory (enable crock))))))
    :fn all-subfunctions-list-rec
    :otf-flg t)

  (defthm all-subfunctions-list-rec-of-cons
    (acl2::set-equiv (acl2::alist-keys (all-subfunctions-list-rec (cons a b) static nil))
                     (append (acl2::alist-keys (all-subfunctions-rec a static nil))
                             (acl2::alist-keys (all-subfunctions-list-rec b static nil))))
    :hints (("goal" :use ((:instance all-subfunctions-list-rec-top-seen-collects-successors
                           (x (cons a b)) (static0 static))))))

  (defthm all-subfunctions-list-rec-when-not-consp
    (implies (not (consp x))
             (equal (all-subfunctions-list-rec x static seen)
                    seen))
    :hints (("goal" :expand ((all-subfunctions-list-rec x static seen)))))

  (local (in-theory (disable all-subfunctions-list-rec)))
  (defthm all-subfunctions-list-rec-of-append
    (acl2::set-equiv (acl2::alist-keys (all-subfunctions-list-rec (append a b) static nil))
                     (append (acl2::alist-keys (all-subfunctions-list-rec a static nil))
                             (acl2::alist-keys (all-subfunctions-list-rec b static nil))))))


(define all-subfunctions ((x call-sig-p)
                          (static static_env_global-p))
  :returns (fns identifierlist-p)
  (acl2::alist-keys (all-subfunctions-rec x static nil)))

(define all-subfunctions-list ((x call-siglist-p)
                               (static static_env_global-p))
  :returns (fns identifierlist-p)
  (acl2::alist-keys (all-subfunctions-list-rec x static nil))
  ///
  (fty::deffixequiv all-subfunctions-list)
  (defret all-subfunctions-list-of-atom
    (implies (not (consp x))
             (equal fns nil)))
  (defthm all-subfunctions-list-of-cons
    (acl2::set-equiv (all-subfunctions-list (cons x y) static)
                     (append (all-subfunctions x static)
                             (all-subfunctions-list y static)))
    :hints(("Goal" :in-theory (enable all-subfunctions))))

  (defthm all-subfunctions-list-of-append
    (acl2::set-equiv (all-subfunctions-list (append x y) static)
                     (append (all-subfunctions-list x static)
                             (all-subfunctions-list y static))))

  (local (in-theory (e/d (acl2::hons-assoc-equal-iff-member-alist-keys)
                         (acl2::alist-keys-member-hons-assoc-equal
                          all-subfunctions-list))))
  
  (local (defthm member-when-member
           (implies (and (member f fns)
                         (member s (all-subfunctions f static)))
                    (member s (all-subfunctions-list fns static)))
           :hints(("Goal" :in-theory (enable member)))))

  (local (defthm member-when-subset
           (implies (and (subsetp fs fns)
                         (member s (all-subfunctions-list fs static)))
                    (member s (all-subfunctions-list fns static)))
           :hints(("Goal" :in-theory (enable subsetp-equal)
                   :induct (subsetp fs fns)))))

  (local (defthm subset-when-subset
           (implies (subsetp fs fns)
                    (subsetp (all-subfunctions-list fs static)
                             (all-subfunctions-list fns static)))
           :hints(("Goal" :in-theory (enable acl2::subsetp-witness-rw)))))
  
  (defcong acl2::set-equiv acl2::set-equiv (all-subfunctions-list fns static) 1
    :hints(("Goal" :in-theory (enable acl2::set-equiv))))

  
  (defthm all-subfunctions-of-call-sig
    (acl2::set-equiv (all-subfunctions (call-sig name pos) static)
                     (cons (identifier-fix name)
                           (all-subfunctions-list (all-callsigs-fnname name static) static)))
    :hints(("Goal" :in-theory (enable all-subfunctions
                                      all-subfunctions-list
                                      all-subfunctions-rec-top-seen-collects-successors)))))



                                      


(define all-stmts-func ((x func-p))
  :returns (stmts stmtlist-p)
  (b* (((func x)))
    (subprogram_body-case x.body
      :sb_asl (all-stmts-stmt x.body.stmt)
      :otherwise nil)))

(define all-stmts-fnname ((fn identifier-p)
                          (static static_env_global-p))
  :returns (stmts stmtlist-p)
  (b* ((look (hons-assoc-equal (identifier-fix fn)
                               (static_env_global->subprograms static))))
    (and look
         (all-stmts-func (func-ses->fn (cdr look))))))


(define traced-stmts-func ((x func-p) (tracespec tracespec-p))
  :returns (stmts stmtlist-p)
  (b* (((func x)))
    (subprogram_body-case x.body
      :sb_asl (traced-stmts-stmt x.body.stmt tracespec)
      :otherwise nil))
  ///
  (defretd <fn>-in-terms-of-all-stmts
    (equal stmts
           (stmtlist-filter-traced (all-stmts-func x) tracespec))
    :hints(("Goal" :in-theory (enable all-stmts-func)))))

(define traced-stmts-fnname ((fn identifier-p)
                             (static static_env_global-p)
                             (tracespec tracespec-p))
  :returns (stmts stmtlist-p)
  (b* ((look (hons-assoc-equal (identifier-fix fn)
                               (static_env_global->subprograms static))))
    (and look
         (traced-stmts-func (func-ses->fn (cdr look)) tracespec)))
  ///
  (defretd <fn>-in-terms-of-all-stmts
    (equal stmts
           (stmtlist-filter-traced (all-stmts-fnname fn static) tracespec))
    :hints(("Goal" :in-theory (enable all-stmts-fnname
                                      traced-stmts-func-in-terms-of-all-stmts)))))

(define traced-callsigs-func ((x func-p) (tracespec tracespec-p))
  :returns (callsigs call-siglist-p)
  (b* (((func x)))
    (append (traced-callsigs-maybe-expr x.recurse_limit tracespec)
            (subprogram_body-case x.body
              :sb_asl (traced-callsigs-stmt x.body.stmt tracespec)
              :otherwise nil)))
  ///
  (defretd <fn>-in-terms-of-all-callsigs
    (equal callsigs
           (call-siglist-filter-traced (all-callsigs-func x) tracespec))
    :hints(("Goal" :in-theory (enable all-callsigs-func)))))


(define traced-callsigs-fnname ((fn identifier-p)
                             (static static_env_global-p)
                             (tracespec tracespec-p))
  :returns (callsigs call-siglist-p)
  (b* ((look (hons-assoc-equal (identifier-fix fn)
                               (static_env_global->subprograms static))))
    (and look
         (traced-callsigs-func (func-ses->fn (cdr look)) tracespec)))
  ///
  (defretd <fn>-in-terms-of-all-callsigs
    (equal callsigs
           (call-siglist-filter-traced (all-callsigs-fnname fn static) tracespec))
    :hints(("Goal" :in-theory (enable all-callsigs-fnname
                                      traced-callsigs-func-in-terms-of-all-callsigs)))))






(defthm iff-of-true-lists-when-set-equiv
  (implies (and (acl2::set-equiv x y)
                (true-listp x)
                (true-listp y))
           (iff x y))
  :hints(("Goal" :in-theory (enable acl2::set-equiv)))
  :rule-classes nil)

(define all-stmts-fnnames ((fns identifierlist-p)
                           (static static_env_global-p))
  :returns (stmts stmtlist-p)
  (if (atom fns)
      nil
    (append (all-stmts-fnname (car fns) static)
            (all-stmts-fnnames (cdr fns) static))))



(define traced-stmts-fnnames ((fns identifierlist-p)
                              (static static_env_global-p)
                              (tracespec tracespec-p))
  :returns (stmts stmtlist-p)
  (if (atom fns)
      nil
    (append (traced-stmts-fnname (car fns) static tracespec)
            (traced-stmts-fnnames (cdr fns) static tracespec)))
  ///
  (local (defthm member-when-member
           (implies (and (member f fns)
                         (member s (traced-stmts-fnname f static tracespec)))
                    (member s (traced-stmts-fnnames fns static tracespec)))))

  (local (defthm member-when-subset
           (implies (and (subsetp fs fns)
                         (member s (traced-stmts-fnnames fs static tracespec)))
                    (member s (traced-stmts-fnnames fns static tracespec)))))

  (local (defthm subset-when-subset
           (implies (subsetp fs fns)
                    (subsetp (traced-stmts-fnnames fs static tracespec)
                             (traced-stmts-fnnames fns static tracespec)))
           :hints(("Goal" :in-theory (enable acl2::subsetp-witness-rw)))))
  
  (defcong acl2::set-equiv acl2::set-equiv (traced-stmts-fnnames fns static tracespec) 1
    :hints(("Goal" :in-theory (enable acl2::set-equiv))))

  (defcong acl2::set-equiv iff (traced-stmts-fnnames fns static tracespec) 1
    :hints (("goal" :use ((:instance iff-of-true-lists-when-set-equiv
                           (x (traced-stmts-fnnames fns static tracespec))
                           (y (traced-stmts-fnnames fns-equiv static tracespec)))))))

  (defthm traced-stmts-fnnames-of-append
    (equal (traced-stmts-fnnames (append x y) static tracespec)
           (append (traced-stmts-fnnames x static tracespec)
                   (traced-stmts-fnnames y static tracespec))))

  (defthm traced-stmts-fnnames-of-nil
    (equal (traced-stmts-fnnames nil static tracespec)
           nil))

  (defretd <fn>-in-terms-of-all-stmts
    (equal stmts
           (stmtlist-filter-traced (all-stmts-fnnames fns static) tracespec))
    :hints(("Goal" :in-theory (enable all-stmts-fnnames
                                      traced-stmts-fnname-in-terms-of-all-stmts)))))

(define traced-callsigs-fnnames ((fns identifierlist-p)
                              (static static_env_global-p)
                              (tracespec tracespec-p))
  :returns (callsigs call-siglist-p)
  (if (atom fns)
      nil
    (append (traced-callsigs-fnname (car fns) static tracespec)
            (traced-callsigs-fnnames (cdr fns) static tracespec)))
  ///
  
  (local (defthm member-when-member
           (implies (and (member f fns)
                         (member s (traced-callsigs-fnname f static tracespec)))
                    (member s (traced-callsigs-fnnames fns static tracespec)))))

  (local (defthm member-when-subset
           (implies (and (subsetp fs fns)
                         (member s (traced-callsigs-fnnames fs static tracespec)))
                    (member s (traced-callsigs-fnnames fns static tracespec)))))

  (local (defthm subset-when-subset
           (implies (subsetp fs fns)
                    (subsetp (traced-callsigs-fnnames fs static tracespec)
                             (traced-callsigs-fnnames fns static tracespec)))
           :hints(("Goal" :in-theory (enable acl2::subsetp-witness-rw)))))
  
  (defcong acl2::set-equiv acl2::set-equiv (traced-callsigs-fnnames fns static tracespec) 1
    :hints(("Goal" :in-theory (enable acl2::set-equiv))))

  (defcong acl2::set-equiv iff (traced-callsigs-fnnames fns static tracespec) 1
    :hints (("goal" :use ((:instance iff-of-true-lists-when-set-equiv
                           (x (traced-callsigs-fnnames fns static tracespec))
                           (y (traced-callsigs-fnnames fns-equiv static tracespec)))))))

  (defthm traced-callsigs-fnnames-of-append
    (equal (traced-callsigs-fnnames (append x y) static tracespec)
           (append (traced-callsigs-fnnames x static tracespec)
                   (traced-callsigs-fnnames y static tracespec))))

  (defthm traced-callsigs-fnnames-of-nil
    (equal (traced-callsigs-fnnames nil static tracespec)
           nil))

  (defretd <fn>-in-terms-of-all-callsigs
    (equal callsigs
           (call-siglist-filter-traced (all-callsigs-fnnames fns static) tracespec))
    :hints(("Goal" :in-theory (enable all-callsigs-fnnames
                                      traced-callsigs-fnname-in-terms-of-all-callsigs)))))




(defmacro def-trace-free-x (type)
  (acl2::template-subst
   '(define trace-free-<type>-p ((x <type>-p)
                                 &key
                                 ((static-env static_env_global-p) 'static-env)
                                 ((tracespec tracespec-p) 'tracespec))
      (b* ((fns (all-subfunctions-list (all-callsigs-<type> x) static-env)))
        (and (not (traced-callsigs-fnnames fns static-env tracespec))
             (not (traced-stmts-fnnames fns static-env tracespec))
             ;; (not (traced-stmts-<type> x tracespec))
             (not (traced-callsigs-<type> x tracespec)))))
   :str-alist `(("<TYPE>" . ,(symbol-name type)))
   :pkg-sym 'asl-pkg))

(def-trace-free-x expr)
(def-trace-free-x expr_desc)
(def-trace-free-x exprlist)
(def-trace-free-x pattern_desc)
(def-trace-free-x pattern)
(def-trace-free-x patternlist)
(def-trace-free-x slice)
(def-trace-free-x slicelist)
(def-trace-free-x call)
(def-trace-free-x type_desc)
(def-trace-free-x ty)
(def-trace-free-x tylist)
(def-trace-free-x int_constraint)
(def-trace-free-x int_constraintlist)
(def-trace-free-x constraint_kind)
(def-trace-free-x array_index)
(def-trace-free-x named_expr)
(def-trace-free-x named_exprlist)
(def-trace-free-x maybe-ty)
(def-trace-free-x typed_identifier)
(def-trace-free-x typed_identifierlist)
(def-trace-free-x lexpr)
(def-trace-free-x lexpr_desc)
(def-trace-free-x lexprlist)
(def-trace-free-x maybe-expr)
(def-trace-free-x ty-timeframe)
(def-trace-free-x ty-timeframe-imap)

(local (defthm consp-under-iff
         (implies (true-listp x)
                  (iff (consp x) x))))


(defthmd trace-free-expr_desc-p-redef
  (iff (trace-free-expr_desc-p x)
       (expr_desc-case x
         :e_atc (and (trace-free-expr-p x.expr)
                     (trace-free-ty-p x.type))
         :e_binop (and (trace-free-expr-p x.arg1)
                       (trace-free-expr-p x.arg2))
         :e_unop (trace-free-expr-p x.arg)
         :e_call (trace-free-call-p x.call)
         :e_slice (and (trace-free-expr-p x.expr)
                       (trace-free-slicelist-p x.slices))
         :e_cond (and (trace-free-expr-p x.test)
                      (trace-free-expr-p x.then)
                      (trace-free-expr-p x.else))
         :e_getarray (and (trace-free-expr-p x.base)
                          (trace-free-expr-p x.index))
         :e_getenumarray (and (trace-free-expr-p x.base)
                              (trace-free-expr-p x.index))
         :e_getfield (trace-free-expr-p x.base)
         :e_getfields (trace-free-expr-p x.base)
         :e_getitem (trace-free-expr-p x.base)
         :e_record (and (trace-free-ty-p x.type)
                        (trace-free-named_exprlist-p x.fields))
         :e_tuple (trace-free-exprlist-p x.exprs)
         :e_array (and (trace-free-expr-p x.length)
                       (trace-free-expr-p x.value))
         :e_enumarray (trace-free-expr-p x.value)
         :e_arbitrary (trace-free-ty-p x.type)
         :e_pattern (and (trace-free-expr-p x.expr)
                         (trace-free-pattern-p x.pattern))
         :otherwise t))
  :hints(("Goal" :in-theory (enable trace-free-expr_desc-p
                                    trace-free-expr-p
                                    trace-free-ty-p
                                    trace-free-call-p
                                    trace-free-slicelist-p
                                    trace-free-named_exprlist-p
                                    trace-free-exprlist-p
                                    trace-free-pattern-p)
          :expand ((all-callsigs-expr_desc x))))
  :rule-classes :definition)

(defthmd trace-free-expr-p-redef
  (iff (trace-free-expr-p x)
       (and (b* (((expr x)))
              (expr_desc-case x.desc
                :e_call (and (not (find-call-tracespec (call->name x.desc.call) x.pos_start tracespec))
                             (not (traced-callsigs-fnnames (all-subfunctions (call-sig (call->name x.desc.call)
                                                                                       x.pos_start)
                                                                             static-env)
                                                           static-env
                                                           tracespec))
                             (not (traced-stmts-fnnames (all-subfunctions (call-sig (call->name x.desc.call)
                                                                                    x.pos_start)
                                                                          static-env)
                                                        static-env
                                                        tracespec)))
                :otherwise t))
            (trace-free-expr_desc-p (expr->desc x))))
  :hints(("Goal" :in-theory (enable trace-free-expr-p
                                    trace-free-expr_desc-p)
          :expand ((all-callsigs-expr x)
                   (all-callsigs-expr-aux x)
                   (:free (a b) (traced-stmts-fnnames (cons a b) static-env tracespec))
                   (:free (a b) (traced-callsigs-fnnames (cons a b) static-env tracespec)))))
  :rule-classes :definition)

(defthmd trace-free-exprlist-p-redef
  (iff (trace-free-exprlist-p x)
       (if (atom x)
           t
         (and (trace-free-expr-p (car x))
              (trace-free-exprlist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-free-exprlist-p
                                    trace-free-expr-p)
          :expand ((all-callsigs-exprlist x))))
  :rule-classes :definition)

(defthmd trace-free-pattern_desc-p-redef
  (iff (trace-free-pattern_desc-p x)
       (pattern_desc-case x
         :pattern_any (trace-free-patternlist-p x.patterns)
         :pattern_geq (trace-free-expr-p x.expr)
         :pattern_leq (trace-free-expr-p x.expr)
         :pattern_not (trace-free-pattern-p x.pattern)
         :pattern_range (and (trace-free-expr-p x.lower)
                             (trace-free-expr-p x.upper))
         :pattern_single (trace-free-expr-p x.expr)
         :pattern_tuple (trace-free-patternlist-p x.patterns)
         :otherwise t))
  :hints(("Goal" :in-theory (enable trace-free-patternlist-p
                                    trace-free-pattern-p
                                    trace-free-expr-p
                                    trace-free-pattern_desc-p)
          :expand ((all-callsigs-pattern_desc x))))
  :rule-classes :definition)

(defthmd trace-free-pattern-p-redef
  (iff (trace-free-pattern-p x)
       (trace-free-pattern_desc-p (pattern->desc x)))
  :hints(("Goal" :in-theory (enable trace-free-pattern_desc-p
                                    trace-free-pattern-p)
          :expand ((all-callsigs-pattern x))))
  :rule-classes :definition)

(defthmd trace-free-patternlist-p-redef
  (iff (trace-free-patternlist-p x)
       (if (atom x)
           t
         (and (trace-free-pattern-p (car x))
              (trace-free-patternlist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-free-patternlist-p
                                    trace-free-pattern-p)
          :expand ((all-callsigs-patternlist x))))
  :rule-classes :definition)

(defthmd trace-free-slice-p-redef
  (iff (trace-free-slice-p x)
       (slice-case x
         :slice_single (trace-free-expr-p x.index)
         :slice_range (and (trace-free-expr-p x.end)
                           (trace-free-expr-p x.start))
         :slice_length (and (trace-free-expr-p x.start)
                            (trace-free-expr-p x.length))
         :slice_star (and (trace-free-expr-p x.factor)
                          (trace-free-expr-p x.length))))
  :hints(("Goal" :in-theory (enable trace-free-expr-p
                                    trace-free-slice-p)
          :expand ((all-callsigs-slice x))))
  :rule-classes :definition)

(defthmd trace-free-slicelist-p-redef
  (iff (trace-free-slicelist-p x)
       (if (atom x)
           t
         (and (trace-free-slice-p (car x))
              (trace-free-slicelist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-free-slicelist-p
                                    trace-free-slice-p)
          :expand ((all-callsigs-slicelist x))))
  :rule-classes :definition)


(defthmd trace-free-call-p-redef
  (iff (trace-free-call-p x)
       (b* (((call x)))
         (and (trace-free-exprlist-p x.params)
              (trace-free-exprlist-p x.args))))
  :hints(("Goal" :in-theory (enable trace-free-call-p
                                    trace-free-exprlist-p)
          :expand ((all-callsigs-call x))))
  :rule-classes :definition)


(define trace-free-fnname-p ((x identifier-p)
                             &key
                             ((static-env static_env_global-p) 'static-env)
                             ((tracespec tracespec-p) 'tracespec))
  (b* ((fns (all-subfunctions-list (all-callsigs-fnname x static-env) static-env)))
    (and (not (traced-callsigs-fnnames fns static-env tracespec))
         (not (traced-stmts-fnnames fns static-env tracespec))
         (not (traced-callsigs-fnname x static-env tracespec))
         (not (traced-stmts-fnname x static-env tracespec)))))

(defthm trace-free-fnname-p-when-trace-free-callexpr-p
  (implies (and (trace-free-expr-p x)
                (expr_desc-case (expr->desc x) :e_call))
           (trace-free-fnname-p (call->name (e_call->call (expr->desc x)))))
  :hints(("Goal" :in-theory (enable trace-free-fnname-p
                                    trace-free-expr-p)
          :expand ((all-callsigs-expr x)
                   (all-callsigs-expr-aux x)
                   (ALL-CALLSIGS-EXPR_DESC (EXPR->DESC X))
                   (ALL-CALLSIGS-CALL (E_CALL->CALL (EXPR->DESC X)))
                   (:free (a b) (traced-callsigs-fnnames (cons a b) static-env tracespec))
                   (:free (a b) (traced-stmts-fnnames (cons a b) static-env tracespec))))))

(defthm trace-free-fnname-p-implies-recurse-limit
  (b* ((look (hons-assoc-equal (identifier-fix name)
                               (static_env_global->subprograms static-env))))
    (implies (and (trace-free-fnname-p name)
                  look)
             (trace-free-maybe-expr-p
              (func->recurse_limit
               (func-ses->fn
                (cdr look))))))
  :hints (("goal" :in-theory (enable trace-free-fnname-p
                                     trace-free-expr-p
                                     trace-free-maybe-expr-p
                                     all-callsigs-fnname
                                     all-callsigs-func
                                     all-callsigs-maybe-expr
                                     traced-callsigs-fnname
                                     traced-callsigs-func
                                     maybe-expr-some->val))))

           



(defthmd trace-free-type_desc-p-redef
  (iff (trace-free-type_desc-p x)
       (type_desc-case x
         :t_int (trace-free-constraint_kind-p x.constraint)
         :t_bits (trace-free-expr-p x.expr)
         :t_tuple (trace-free-tylist-p x.types)
         :t_array (and (trace-free-array_index-p x.index)
                       (trace-free-ty-p x.type))
         :t_record (trace-free-typed_identifierlist-p x.fields)
         :t_exception (trace-free-typed_identifierlist-p x.fields)
         :t_collection (trace-free-typed_identifierlist-p x.fields)
         :otherwise t))
  :hints(("Goal" :in-theory (enable trace-free-type_desc-p
                                    trace-free-constraint_kind-p
                                    trace-free-expr-p
                                    trace-free-tylist-p
                                    trace-free-ty-p
                                    trace-free-array_index-p
                                    trace-free-typed_identifierlist-p)
          :expand ((all-callsigs-type_desc x))))
  :rule-classes :definition)

(defthmd trace-free-ty-p-redef
  (iff (trace-free-ty-p x)
       (trace-free-type_desc-p (ty->desc x)))
  :hints(("Goal" :in-theory (enable trace-free-ty-p
                                    trace-free-type_desc-p)
          :expand ((all-callsigs-ty x))))
  :rule-classes :definition)

(defthmd trace-free-tylist-p-redef
  (iff (trace-free-tylist-p x)
       (if (atom x)
           t
         (and (trace-free-ty-p (car x))
              (trace-free-tylist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-free-tylist-p
                                    trace-free-ty-p)
          :expand ((all-callsigs-tylist x))))
  :rule-classes :definition)

(defthmd trace-free-int_constraint-p-redef
  (iff (trace-free-int_constraint-p x)
       (int_constraint-case x
         :constraint_exact (trace-free-expr-p x.val)
         :constraint_range (and (trace-free-expr-p x.from)
                                (trace-free-expr-p x.to))))
  :hints(("Goal" :in-theory (enable trace-free-int_constraint-p
                                    trace-free-expr-p)
          :expand ((all-callsigs-int_constraint x))))
  :rule-classes :definition)


(defthmd trace-free-int_constraintlist-p-redef
  (iff (trace-free-int_constraintlist-p x)
       (if (atom x)
           t
         (and (trace-free-int_constraint-p (car x))
              (trace-free-int_constraintlist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-free-int_constraintlist-p
                                    trace-free-int_constraint-p)
          :expand ((all-callsigs-int_constraintlist x))))
  :rule-classes :definition)


(defthmd trace-free-constraint_kind-p-redef
  (iff (trace-free-constraint_kind-p x)
       (constraint_kind-case x
         :wellconstrained (trace-free-int_constraintlist-p x.constraints)
         :otherwise t))
  :hints(("Goal" :in-theory (enable trace-free-constraint_kind-p
                                    trace-free-int_constraintlist-p)
          :expand ((all-callsigs-constraint_kind x))))
  :rule-classes :definition)


(defthmd trace-free-array_index-p-redef
  (iff (trace-free-array_index-p x)
       (array_index-case x
         :arraylength_expr (trace-free-expr-p x.length)
         :otherwise t))
  :hints(("Goal" :in-theory (enable trace-free-array_index-p
                                    trace-free-expr-p)
          :expand ((all-callsigs-array_index x))))
  :rule-classes :definition)


(defthmd trace-free-named_expr-p-redef
  (iff (trace-free-named_expr-p x)
       (b* (((named_expr x)))
         (trace-free-expr-p x.expr)))
  :hints(("Goal" :in-theory (enable trace-free-named_expr-p
                                    trace-free-expr-p)
          :expand ((all-callsigs-named_expr x))))
  :rule-classes :definition)

(defthmd trace-free-named_exprlist-p-redef
  (iff (trace-free-named_exprlist-p x)
       (if (atom x)
           t
         (and (trace-free-named_expr-p (car x))
              (trace-free-named_exprlist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-free-named_exprlist-p
                                    trace-free-named_expr-p)
          :expand ((all-callsigs-named_exprlist x))))
  :rule-classes :definition)

(defthmd trace-free-maybe-ty-redef
  (iff (Trace-free-maybe-ty-p x)
       (or (not x)
           (trace-free-ty-p x)))
  :hints(("Goal" :in-theory (enable trace-free-ty-p
                                    trace-free-maybe-ty-p
                                    maybe-ty-some->val)
          :expand ((all-callsigs-maybe-ty x)))))


(defthmd trace-free-typed_identifier-p-redef
  (iff (trace-free-typed_identifier-p x)
       (b* (((typed_identifier x)))
         (trace-free-ty-p x.type)))
  :hints(("Goal" :in-theory (enable trace-free-typed_identifier-p
                                    trace-free-ty-p)
          :expand ((all-callsigs-typed_identifier x))))
  :rule-classes :definition)

(defthmd trace-free-typed_identifierlist-p-redef
  (iff (trace-free-typed_identifierlist-p x)
       (if (atom x)
           t
         (and (trace-free-typed_identifier-p (car x))
              (trace-free-typed_identifierlist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-free-typed_identifierlist-p
                                    trace-free-typed_identifier-p)
          :expand ((all-callsigs-typed_identifierlist x))))
  :rule-classes :definition)

(defthmd trace-free-lexpr_desc-p-redef
  (iff (trace-free-lexpr_desc-p x)
       (lexpr_desc-case x
         :le_slice (and (trace-free-lexpr-p x.base)
                        (trace-free-slicelist-p x.slices))
         :le_setarray (and (trace-free-lexpr-p x.base)
                           (trace-free-expr-p x.index))
         :le_setenumarray (and (trace-free-lexpr-p x.base)
                               (trace-free-expr-p x.index))
         :le_setfield (trace-free-lexpr-p x.base)
         :le_setfields (trace-free-lexpr-p x.base)
         :le_destructuring (trace-free-lexprlist-p x.elts)
         :otherwise t))
  :hints(("Goal" :in-theory (enable trace-free-lexpr_desc-p
                                    trace-free-lexpr-p
                                    trace-free-expr-p
                                    trace-free-slicelist-p
                                    trace-free-lexprlist-p)
          :expand ((all-callsigs-lexpr_desc x))))
  :rule-classes :definition)


(defthmd trace-free-lexpr-p-redef
  (iff (trace-free-lexpr-p x)
       (trace-free-lexpr_desc-p (lexpr->desc x)))
  :hints(("Goal" :in-theory (enable trace-free-lexpr_desc-p
                                    trace-free-lexpr-p)
          :expand ((all-callsigs-lexpr x))))
  :rule-classes :definition)

(defthmd trace-free-lexprlist-p-redef
  (iff (trace-free-lexprlist-p x)
       (if (atom x)
           t
         (and (trace-free-lexpr-p (car x))
              (trace-free-lexprlist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-free-lexprlist-p
                                    trace-free-lexpr-p)
          :expand ((all-callsigs-lexprlist x))))
  :rule-classes :definition)

(defthmd trace-free-maybe-expr-p-redef
  (iff (trace-free-maybe-expr-p x)
       (or (not x)
           (trace-free-expr-p x)))
  :hints(("Goal" :in-theory (enable trace-free-expr-p
                                    trace-free-maybe-expr-p
                                    maybe-expr-some->val)
          :expand ((all-callsigs-maybe-expr x))))
  :rule-classes :definition)

(defthm trace-free-ty-timeframe-imap-implies-lookup
  (implies (and (trace-free-ty-timeframe-imap-p x)
                (ty-timeframe-imap-p x)
                (hons-assoc-equal name x)
                (identifier-p name))
           (trace-free-ty-p (ty-timeframe->ty
                             (cdr (hons-assoc-equal name x)))))
  :hints(("Goal" :induct (hons-assoc-equal name x)
          :in-theory (enable trace-free-ty-p
                             trace-free-ty-timeframe-imap-p)
          :expand ((all-callsigs-ty-timeframe-imap x)
                   (all-callsigs-ty-timeframe (cdar x))
                   (traced-callsigs-ty-timeframe-imap x tracespec)
                   (traced-callsigs-ty-timeframe (cdar x) tracespec)))))
                             


(defmacro def-trace-free-s (type)
  (acl2::template-subst
   '(define trace-free-<type>-p ((x <type>-p)
                                 &key
                                 ((static-env static_env_global-p) 'static-env)
                                 ((tracespec tracespec-p) 'tracespec))
      (b* ((fns (all-subfunctions-list (all-callsigs-<type> x) static-env)))
        (and (not (traced-callsigs-fnnames fns static-env tracespec))
             (not (traced-stmts-fnnames fns static-env tracespec))
             (not (traced-stmts-<type> x tracespec))
             (not (traced-callsigs-<type> x tracespec)))))
   :str-alist `(("<TYPE>" . ,(symbol-name type)))
   :pkg-sym 'asl-pkg))

(def-trace-free-s stmt_desc)
(def-trace-free-s stmt)
(def-trace-free-s maybe-stmt)
(def-trace-free-s catcher)
(def-trace-free-s catcherlist)

(defthmd trace-free-stmt_desc-p-redef
  (iff (trace-free-stmt_desc-p x)
       (stmt_desc-case x
         :s_seq (and (trace-free-stmt-p x.first)
                     (trace-free-stmt-p x.second))
         :s_decl (and (trace-free-maybe-ty-p x.ty)
                      (trace-free-maybe-expr-p x.expr))
         :s_assign (and (trace-free-lexpr-p x.lexpr)
                        (trace-free-expr-p x.expr))
         :s_call (trace-free-call-p x.call)
         :s_return (trace-free-maybe-expr-p x.expr)
         :s_cond (and (trace-free-expr-p x.test)
                      (trace-free-stmt-p x.then)
                      (trace-free-stmt-p x.else))
         :s_assert (trace-free-expr-p x.expr)
         :s_for (and (trace-free-expr-p x.start_e)
                     (trace-free-expr-p x.end_e)
                     (trace-free-stmt-p x.body)
                     (trace-free-maybe-expr-p x.limit))
         :s_while (and (trace-free-expr-p x.test)
                       (trace-free-maybe-expr-p x.limit)
                       (trace-free-stmt-p x.body))
         :s_repeat (and (trace-free-stmt-p x.body)
                        (trace-free-expr-p x.test)
                        (trace-free-maybe-expr-p x.limit))
         :s_throw (and (trace-free-expr-p x.val)
                       (trace-free-maybe-ty-p x.ty))
         :s_try (and (trace-free-stmt-p x.body)
                     (trace-free-catcherlist-p x.catchers)
                     (trace-free-maybe-stmt-p x.otherwise))
         :s_print (trace-free-exprlist-p x.args)
         :s_pragma (trace-free-exprlist-p x.exprs)
         :otherwise t))
  :hints(("Goal" :in-theory (enable trace-free-exprlist-p
                                    trace-free-maybe-stmt-p
                                    trace-free-catcherlist-p
                                    trace-free-stmt-p
                                    trace-free-maybe-ty-p
                                    trace-free-expr-p
                                    trace-free-maybe-expr-p
                                    trace-free-call-p
                                    trace-free-lexpr-p
                                    trace-free-stmt_desc-p)
          :expand ((all-callsigs-stmt_desc x)
                   (all-stmts-stmt_desc x))))
  :rule-classes :definition)

(defthmd trace-free-stmt-p-redef
  (iff (trace-free-stmt-p x)
       (and (not (find-stmt-tracespec x tracespec))
            (b* (((stmt x)))
              (stmt_desc-case x.desc
                :s_call (and (not (find-call-tracespec (call->name x.desc.call) x.pos_start tracespec))
                             (not (traced-callsigs-fnnames (all-subfunctions (call-sig (call->name x.desc.call)
                                                                                       x.pos_start)
                                                                             static-env)
                                                           static-env
                                                           tracespec))
                             (not (traced-stmts-fnnames (all-subfunctions (call-sig (call->name x.desc.call)
                                                                                    x.pos_start)
                                                                          static-env)
                                                        static-env
                                                        tracespec)))
                :otherwise t))
            (trace-free-stmt_desc-p (stmt->desc x))))
  :hints(("Goal" :in-theory (enable trace-free-stmt-p
                                    trace-free-stmt_desc-p)
          :expand ((all-callsigs-stmt x)
                   (all-stmts-stmt x)
                   (all-callsigs-stmt-aux x)
                   (all-stmts-stmt-aux x)
                   (:free (a b) (traced-callsigs-fnnames (cons a b) static-env tracespec))
                   (:free (a b) (traced-stmts-fnnames (cons a b) static-env tracespec)))))
  :rule-classes :definition)


(defthm trace-free-fnname-p-implies-body
  (b* ((look (hons-assoc-equal (identifier-fix name)
                               (static_env_global->subprograms static-env))))
    (implies (and (trace-free-fnname-p name)
                  look
                  (subprogram_body-case (func->body (func-ses->fn (cdr look))) :sb_asl))
             (trace-free-stmt-p
              (sb_asl->stmt
               (func->body (func-ses->fn (cdr look)))))))
  :hints (("goal" :in-theory (enable trace-free-fnname-p
                                     trace-free-stmt-p
                                     all-callsigs-fnname
                                     all-callsigs-func
                                     traced-callsigs-fnname
                                     traced-callsigs-func
                                     traced-stmts-fnname
                                     traced-stmts-func))))

(defthm trace-free-fnname-p-when-trace-free-callstmt-p
  (implies (and (trace-free-stmt-p x)
                (stmt_desc-case (stmt->desc x) :s_call))
           (trace-free-fnname-p (call->name (s_call->call (stmt->desc x)))))
  :hints(("Goal" :in-theory (enable trace-free-fnname-p
                                    trace-free-stmt-p)
          :expand ((all-callsigs-stmt x)
                   (all-callsigs-stmt-aux x)
                   (ALL-CALLSIGS-STMT_DESC (STMT->DESC X))
                   (ALL-CALLSIGS-CALL (S_CALL->CALL (STMT->DESC X)))
                   (:free (a b) (traced-callsigs-fnnames (cons a b) static-env tracespec))
                   (:free (a b) (traced-stmts-fnnames (cons a b) static-env tracespec))))))



(defthmd trace-free-maybe-stmt-p-redef
  (iff (trace-free-maybe-stmt-p x)
       (or (not x)
           (trace-free-stmt-p x)))
  :hints(("Goal" :in-theory (enable trace-free-stmt-p
                                    trace-free-maybe-stmt-p
                                    maybe-stmt-some->val)
          :expand ((all-callsigs-maybe-stmt x)
                   (all-stmts-maybe-stmt x))))
  :rule-classes :definition)

(defthmd trace-free-catcher-p-redef
  (iff (trace-free-catcher-p x)
       (b* (((catcher x)))
         (and (trace-free-ty-p x.ty)
              (trace-free-stmt-p x.stmt))))
  :hints(("Goal" :in-theory (enable trace-free-catcher-p
                                    trace-free-ty-p
                                    trace-free-stmt-p)
          :expand ((all-callsigs-catcher x)
                   (all-stmts-catcher x))))
  :rule-classes :definition)


(defthmd trace-free-catcherlist-p-redef
  (iff (trace-free-catcherlist-p x)
       (if (atom x)
           t
         (and (trace-free-catcher-p (car x))
              (trace-free-catcherlist-p (cdr x)))))
  :hints(("Goal" :in-theory (enable trace-free-catcherlist-p
                                    trace-free-catcher-p)
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
(local (defthm ev_error->desc-of-init-backtrace
         (equal (ev_error->desc (init-backtrace err pos))
                (ev_error->desc err))
         :hints(("Goal" :in-theory (enable init-backtrace)))))

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
 (defthm trace-free-stmt-p-of-find-catcher
   (b* ((c (find_catcher static-env throw catchers)))
     (implies (and (trace-free-catcherlist-p catchers)
                   c)
              (trace-free-stmt-p (catcher->stmt c))))
   :hints(("Goal" :in-theory (enable find_catcher)
           :induct t
           :expand ((trace-free-catcherlist-p catchers)
                    (trace-free-catcher-p (car catchers)))))))





(local
 (defthm env->global-of-push_scope
   (equal (env->global (push_scope env))
          (env->global env))
   :hints(("Goal" :in-theory (enable push_scope)))))

(set-ignore-ok t)

(local (in-theory (enable trace-free-maybe-expr-p-redef)))

(defthm-expr_of_lexpr-flag
  (defthm trace-free-expr-p-of-expr_of_lexpr
    (implies (trace-free-lexpr-p x)
             (trace-free-expr-p (expr_of_lexpr x)))
    :hints ('(:expand ((expr_of_lexpr x)
                       (trace-free-lexpr-p x)
                       (trace-free-lexpr_desc-p (lexpr->desc x))
                       (:free (x pos) (trace-free-expr-p (expr x pos))))
              :in-theory (enable trace-free-expr_desc-p-redef)))
    :flag expr_of_lexpr)
  (defthm trace-free-expr-p-of-expr_of_lexprlist
    (implies (trace-free-lexprlist-p x)
             (trace-free-exprlist-p (exprlist_of_lexprlist x)))
    :hints ('(:expand ((exprlist_of_lexprlist x)
                       (trace-free-lexprlist-p x)
                       (trace-free-exprlist-p nil)
                       (:free (a b) (trace-free-exprlist-p (cons a b))))))
    :flag exprlist_of_lexprlist))


(defthm trace-free-constraint-kind-p-of-deparameterized
  (TRACE-FREE-CONSTRAINT_KIND-P
   (WELLCONSTRAINED
    (LIST (CONSTRAINT_EXACT (EXPR (E_VAR (PARAMETRIZED->NAME X))
                                  '((FNAME . "<none>")
                                    (LNUM . 0)
                                    (BOL . 0)
                                    (CNUM . 0)))))
    '(:PRECISION_FULL)))
  :hints(("Goal" :in-theory (enable trace-free-constraint_kind-p
                                    all-callsigs-constraint_kind
                                    all-callsigs-int_constraintlist
                                    all-callsigs-int_constraint
                                    all-callsigs-expr
                                    all-callsigs-expr-aux
                                    all-callsigs-expr_desc))))
          

(defthm trace-free-exprlist-p-of-named_exprlist->exprs
  (implies (trace-free-named_exprlist-p x)
           (trace-free-exprlist-p (named_exprlist->exprs x)))
  :hints(("Goal" :in-theory (enable named_exprlist->exprs
                                    trace-free-named_exprlist-p-redef
                                    trace-free-exprlist-p-redef
                                    trace-free-named_expr-p-redef))))


(defthm call-tracespeclist-find-of-combine-tracespecs
  (not (call-tracespeclist-find fn pos nil))
  :hints(("Goal" :in-theory (enable call-tracespeclist-find))))

(defthm find-call-tracespec-of-combine-tracespecs
  (implies (not (find-call-tracespec fn pos tracespec))
           (not (find-call-tracespec fn pos (combine-tracespecs t nil tracespec))))
  :hints(("Goal" :in-theory (enable find-call-tracespec
                                    combine-tracespecs))))

(defthm call-siglist-filter-traced-of-combine-tracespecs
  (implies (not (call-siglist-filter-traced x tracespec))
           (not (call-siglist-filter-traced x (combine-tracespecs t nil tracespec))))
  :hints(("Goal" :in-theory (enable call-siglist-filter-traced))))

(defthm stmt-tracespeclist-find-of-combine-tracespecs
  (not (stmt-tracespeclist-find x nil))
  :hints(("Goal" :in-theory (enable stmt-tracespeclist-find))))


(defthm find-stmt-tracespec-of-combine-tracespecs
  (implies (not (find-stmt-tracespec x tracespec))
           (not (find-stmt-tracespec x (combine-tracespecs t nil tracespec))))
  :hints(("Goal" :in-theory (enable find-stmt-tracespec
                                    combine-tracespecs))))


(defthm stmtlist-filter-traced-of-combine-tracespecs
  (implies (not (stmtlist-filter-traced x tracespec))
           (not (stmtlist-filter-traced x (combine-tracespecs t nil tracespec))))
  :hints(("Goal" :in-theory (enable stmtlist-filter-traced))))



(defthm trace-free-fnname-p-of-combine-tracespecs-call
  (implies (trace-free-fnname-p name)
           (trace-free-fnname-p name :tracespec (combine-tracespecs t nil tracespec)))
  :hints(("Goal" :in-theory (enable trace-free-fnname-p
                                    traced-callsigs-fnnames-in-terms-of-all-callsigs
                                    traced-callsigs-fnname-in-terms-of-all-callsigs
                                    traced-stmts-fnname-in-terms-of-all-stmts
                                    traced-stmts-fnnames-in-terms-of-all-stmts))))

(defthm trace-free-ty-timeframe-imap-p-of-combine-tracespecs-call
  (implies (trace-free-ty-timeframe-imap-p name)
           (trace-free-ty-timeframe-imap-p name :tracespec (combine-tracespecs t nil tracespec)))
  :hints(("Goal" :in-theory (enable trace-free-ty-timeframe-imap-p
                                    traced-callsigs-fnnames-in-terms-of-all-callsigs
                                    traced-callsigs-fnname-in-terms-of-all-callsigs
                                    traced-stmts-fnname-in-terms-of-all-stmts
                                    traced-stmts-fnnames-in-terms-of-all-stmts))))

(local
 (defthm combine-tracespecs-nil-nil
   (equal (combine-tracespecs nil nil tracespec)
          (tracespec-fix tracespec))
   :hints(("Goal" :in-theory (enable combine-tracespecs)))))

(local (in-theory (disable floor
                           acl2::true-listp-append
                           (:t binary-append)
                           true-listp
                           consp-under-iff
                           acl2::append-when-not-consp
                           acl2::append-of-nil
                           trace-free-maybe-expr-p-redef
                           env-replace-static-with-self
                           append
                           hons-assoc-equal)))

(with-output
  ;; makes it so it won't take forever to print the induction scheme
  :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
  :off (event)

  (std::defret-mutual-generate <fn>-no-trace-when-trace-free
    :mutual-recursion asl-interpreter-mutual-recursion-*t
    :formal-hyps (((expr-p x) (trace-free-expr-p x))
                  ((int_constraintlist-p x) (trace-free-int_constraintlist-p x))
                  ((constraint_kind-p x) (trace-free-constraint_kind-p x))
                  ((tylist-p x) (trace-free-tylist-p x))
                  ((typed_identifierlist-p x) (trace-free-typed_identifierlist-p x))
                  ((ty-p x) (trace-free-ty-p x))
                  ((pattern-p x) (trace-free-pattern-p x))
                  ((patternlist-p x) (trace-free-patternlist-p x))
                  ((exprlist-p x) (trace-free-exprlist-p x))
                  ((lexpr-p x) (trace-free-lexpr-p x))
                  ((lexprlist-p x) (trace-free-lexprlist-p x))
                  ((maybe-expr-p x) (trace-free-maybe-expr-p x))
                  ((stmt-p x) (trace-free-stmt-p x))
                  ((catcherlist-p x) (trace-free-catcherlist-p x))
                  ((maybe-stmt-p x) (trace-free-maybe-stmt-p x))
                  ((slice-p x) (trace-free-slice-p x))
                  ((slicelist-p x) (trace-free-slicelist-p x)))
                
    :rules ((t ;; (:add-bindings ((static-env (global-env->static (env->global env)))))
             (:add-concl (not (equal (ev_error->desc res) "Trace abort")))
             (:add-concl (equal trace nil))
             (:add-hyp (trace-free-ty-timeframe-imap-p
                        (static_env_global->declared_types static-env))))
            ((:fnname is_val_of_type-*t)
             (:add-keyword :hints ('(:expand ((trace-free-ty-p ty)
                                              (trace-free-type_desc-p (ty->desc ty))
                                              (TRACE-FREE-CONSTRAINT_KIND-P (T_INT->CONSTRAINT (TY->DESC TY))))))))
            ((:fnname check_int_constraints-*t)
             (:add-keyword :hints ('(:expand ((TRACE-FREE-INT_CONSTRAINTLIST-P CONSTRS)
                                              (TRACE-FREE-INT_CONSTRAINT-P (CAR CONSTRS)))))))
            ((:fnname is_val_of_type_tuple-*t)
             (:add-keyword :hints ('(:expand ((trace-free-tylist-p types))))))
            ((:fnname eval_slice_list-*t)
             (:add-keyword :hints ('(:expand ((trace-free-slicelist-p sl))))))
            ((:fnname eval_slice-*t)
             (:add-keyword :hints ('(:expand ((trace-free-slice-p s))))))
            ((:fnname eval_catchers-*t)
             (:add-keyword :hints ('(:expand ((TRACE-FREE-MAYBE-STMT-P OTHERWISE))))))
            ((:fnname eval_stmt-*t1)
             (:add-keyword :hints ('(:expand ((TRACE-FREE-stmt-p s)
                                              (trace-free-stmt_desc-p (stmt->desc s))
                                              (TRACE-FREE-MAYBE-EXPR-P (S_RETURN->EXPR (STMT->DESC S)))
                                              (TRACE-FREE-MAYBE-EXPR-P (S_DECL->EXPR (STMT->DESC S)))
                                              (TRACE-FREE-EXPR-P (S_RETURN->EXPR (STMT->DESC S)))
                                              (TRACE-FREE-EXPR_DESC-P (EXPR->DESC (S_RETURN->EXPR (STMT->DESC S))))
                                              (TRACE-FREE-CALL-P (S_CALL->CALL (STMT->DESC S))))))))
            ((:fnname eval_limit-*t)
             (:add-keyword :hints ('(:expand ((TRACE-FREE-MAYBE-EXPR-P X))))))
            ((:fnname eval_lexpr_list-*t)
             (:add-keyword :hints ('(:expand ((TRACE-FREE-lexprlist-p lx))))))
            ((:fnname eval_lexpr-*t)
             (:add-keyword :hints ('(:expand ((TRACE-FREE-lexpr-p lx)
                                              (TRACE-FREE-lexpr_desc-p (lexpr->desc lx)))))))
            ((:fnname eval_call-*t)
             (:add-hyp (and (trace-free-fnname-p name)
                            (not (find-call-tracespec name pos tracespec)))))
            ((:fnname eval_subprogram-*t)
             (:add-hyp (and (trace-free-fnname-p name)
                            (not (find-call-tracespec name pos tracespec)))))
            ((:fnname eval_subprogram-*t1)
             (:add-hyp (trace-free-fnname-p name)))
            ((:fnname eval_expr_list-*t)
             (:add-keyword :hints ('(:expand ((trace-free-exprlist-p e))))))
            ((or (:fnname eval_pattern-any-*t)
                 (:fnname eval_pattern_tuple-*t))
             (:add-keyword :hints ('(:expand ((trace-free-patternlist-p p))))))
            ((:fnname eval_pattern-*t)
             (:add-keyword :hints ('(:expand ((trace-free-pattern-p p)
                                              (trace-free-pattern_desc-p (pattern->desc p)))))))
            ((:fnname resolve-ty-*t)
             (:add-keyword :hints ('(:expand ((trace-free-ty-p x)
                                              (trace-free-type_desc-p (ty->desc x))
                                              (TRACE-FREE-ARRAY_INDEX-P (T_ARRAY->INDEX (TY->DESC X))))))))
            ((:fnname resolve-typed_identifierlist-*t)
             (:add-keyword :hints ('(:expand ((trace-free-typed_identifierlist-p x)
                                              (TRACE-FREE-TYPED_IDENTIFIER-P (CAR X)))))))
            ((:fnname resolve-tylist-*t)
             (:add-keyword :hints ('(:expand ((trace-free-tylist-p x))))))
            ((:fnname resolve-constraint_kind-*t)
             (:add-keyword :hints ('(:expand ((trace-free-constraint_kind-p x))))))
            ((:fnname resolve-int_constraints-*t)
             (:add-keyword :hints ('(:expand ((trace-free-int_constraintlist-p x)
                                              (trace-free-int_constraint-p (car x)))))))
            ((:fnname eval_expr-*t)
             (:add-keyword :hints ('(:expand ((trace-free-expr-p e)
                                              (trace-free-expr_desc-p (expr->desc e))
                                              (TRACE-FREE-CALL-P (E_CALL->CALL (EXPR->DESC E))))))))
            ((:fnname eval_stmt-*t)
             (:add-keyword :hints ('(:expand ((trace-free-stmt-p s)
                                              (trace-free-stmt_desc-p (stmt->desc s))
                                              (TRACE-FREE-CALL-P (s_CALL->CALL (stmt->DESC s)))))))))
    :hints ((vl::big-mutrec-default-hint 'eval_expr-*t-fn id nil world))))


