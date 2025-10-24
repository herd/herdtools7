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

(include-book "../exp-equiv")
(include-book "std/util/defconsts" :dir :system)

; (depends-on "aarch64haz-1-fragment.cat.lsp")
; (depends-on "aarch64haz-2-fragment.cat.lsp")
; (depends-on "aarch64haz-1.cat.lsp")
; (depends-on "aarch64haz-2.cat.lsp")
(acl2::defconsts (& *haz1fragment* state)
  (read-ast-file "aarch64haz-1-fragment.cat.lsp"))

(acl2::defconsts (& *haz2fragment* state)
  (read-ast-file "aarch64haz-2-fragment.cat.lsp"))

(acl2::defconsts (& *haz1* state)
  (read-ast-file "aarch64haz-1.cat.lsp"))

(acl2::defconsts (& *haz2* state)
  (read-ast-file "aarch64haz-2.cat.lsp"))

(thm (inslist-p *haz1fragment*))
(thm (inslist-p *haz2fragment*))
(thm (inslist-p *haz1*))
(thm (inslist-p *haz2*))

(define haz1fragment ()
  :returns (cat inslist-p)
  *haz1fragment*)

(define haz2fragment ()
  :returns (cat inslist-p)
  *haz2fragment*)

(define haz1 ()
  :returns (cat inslist-p)
  *haz1*)

(define haz2 ()
  :returns (cat inslist-p)
  *haz2*)

(defconst *ex1*
  (execgraph
   (list
    ;; initial write for address 0
    (make-evt :uid 0
              :procid 0   ;; ?
              :po-index 0 ;; ?
              :data (make-evt-w :addr 0
                                :val 0
                                :initp t))
    ;; initial write for address 1
    (make-evt :uid 1
              :procid 0   ;; ?
              :po-index 1 ;; ?
              :data (make-evt-w :addr 1
                                :val 0
                                :initp t))
    ;; proc0 write 1 to address 0
    (make-evt :uid 2
              :procid 0
              :po-index 2
              :data (make-evt-w :addr 0 :val 1))
    ;; proc0 write 1 to address 1
    (make-evt :uid 3
              :procid 0
              :po-index 3
              :data (make-evt-w :addr 1 :val 1))
    ;; proc1 read 1 from address 1
    (make-evt :uid 4
              :procid 1
              :po-index 2
              :data (make-evt-r :addr 1 :val 1 :from 3))
    ;; proc1 read 0 from address 0
    (make-evt :uid 5
              :procid 1
              :po-index 3
              :data (make-evt-r :addr 0 :val 0 :from 0)))))

(defconst *co1*
  (b* ((evts (execgraph->evts *ex1*)))
    (mergesort
     (list (evtpair (nth 0 evts) (nth 2 evts))
           (evtpair (nth 1 evts) (nth 3 evts))))))
#||
(eval-inslist (haz1fragment)
              :ex *ex1*
              :result (make-result :judgement :allowed
                                   :env (list
                                         (cons "co" (v_rel *co1*))))
              :reclimit 1000)

(eval-inslist (haz2fragment)
              :ex *ex1*
              :result (make-result :judgement :allowed
                                   :env (list
                                         (cons "co" (v_rel *co1*))))
              :reclimit 1000)


(eval-inslist (haz1)
              :ex *ex1*
              :result (make-result :judgement :allowed
                                   :env (list
                                         (cons "co0" (v_rel nil))))
              :reclimit 1000)

(eval-inslist (haz2)
              :ex *ex1*
              :result (make-result :judgement :allowed
                                   :env (list
                                         (cons "co0" (v_rel nil))))
              :reclimit 1000)


(defstub foo () nil)

||#


(make-event
 `(defthm member-special-vars
    (implies (equal memberp (consp (member-equal v ',(special-vars))))
             (iff (member-equal v (special-vars))
                  memberp))
    :hints(("Goal" :in-theory (enable special-vars)))))




(defmacro defopener (name fn &key (hyp 't) (hint 'nil))
  `(encapsulate nil
     (set-ignore-ok t)
     (make-event
      (b* ((fn (acl2::deref-macro-name ',fn (acl2::macro-aliases (w state))))
           (formals (acl2::formals fn (w state)))
           (body (acl2::body fn nil (w state)))
           (hyp ',hyp)
           ;; ((er simp-body)
           ;;  (if ,simp
           ;;      (b* (((er new-hyp-term)
           ;;            (acl2::easy-simplify-term-fn hyp t ',hint 'iff t t 1000 1000 t state)))
           ;;        (acl2::easy-simplify-term-fn
           ;;         body new-hyp-term ',hint 'equal t t 1000 1000 t state))
           ;;    (value body)))
           (name ',name)
           (hint ',hint)
           (call `(,fn . ,formals))
           (concl `(equal ,call ,body))
           (thmbody (if (eq hyp t)
                        concl
                      `(implies ,hyp ,concl))))
        (value `(defthm ,name ,thmbody
                  :hints (("goal" :expand (,call)
                           . ,hint))))))
     (acl2::add-to-ruleset openers ,name)))

(acl2::def-ruleset openers nil)
(defopener open-eval-inslist eval-inslist :hyp (syntaxp (quotep x)))
(defopener open-eval-ins eval-ins :hyp (syntaxp (quotep x)))
(defopener open-eval-inslist* eval-inslist* :hyp (syntaxp (and (quotep x)
                                                               (or (quotep results)
                                                                   (and (consp results)
                                                                        (eq (car results) 'cons))))))

(defopener open-eval-bindings eval-bindings :hyp (syntaxp (quotep x)))
(defopener open-eval-binding eval-binding :hyp (syntaxp (quotep x)))
(defopener open-eval-exp eval-exp :hyp (syntaxp (quotep x)))
(defopener open-eval-explist eval-explist :hyp (syntaxp (quotep x)))
(defopener open-match-pat match-pat :hyp (syntaxp (quotep p)))
(defopener open-eval-op1 eval-op1 :hyp (syntaxp (quotep op)))
(defopener open-eval-op2 eval-op2 :hyp (syntaxp (quotep op)))
(defopener open-eval-var eval-var :hyp (syntaxp (quotep v)))
(defopener open-add-empty-bindings-to-env
  add-empty-bindings-to-env :hyp (syntaxp (quotep x)))


(define all-bindings-empty ((x bindinglist-p)
                            (env env-p))
  (if (atom x)
      t
    (and (b* (((binding x1) (car x))
              ((unless (pat-case x1.pat :pvar)) t)
              ((pvar x1.pat))
              ((unless x1.pat.var) t))
           (equal (env-lookup x1.pat.var env) (v_empty)))
         (all-bindings-empty (cdr x) env))))

(defopener open-all-bindings-empty
  all-bindings-empty :hyp (syntaxp (quotep x)))
              

;; (defopener open-eval-fixpoint-once eval-fixpoint
;;   :hyp (and (syntaxp (and (quotep x)
;;                           (equal reclimit 'reclimit)))
;;             (all-bindings-empty x env)))

(Defopener open-eval-fixpoint1 eval-fixpoint1 :hyp (syntaxp (quotep x)))
(Defopener open-eval-fixpoint-binding eval-fixpoint-binding :hyp (syntaxp (quotep x)))



;; (local
;;  (defthm exp-equiv-of-union-assoc
;;   (exp-eval-equiv (e_op loc1 :union (list (e_op loc2 :union (list x y)) z))
;;                   (e_op nil :union (list x (e_op nil :union (list y z)))))
;;   :hints(("Goal" :in-theory (enable exp-eval-equiv
;;                                     eval-op2)
;;           :expand ((:free (loc x y ex env reclimit) (eval-exp (e_op loc :union (list x y))))
;;                    (:free (x y ex env reclimit) (eval-explist (cons x y)))
;;                    (:free (ex env reclimit) (eval-explist nil))))))

(defthm fixpoint-is-empty
  (b* (((mv err res) (eval-fixpoint
                      '(((LOC)
                         (PAT :PVAR "ob")
                         (EXP :E_OP NIL
                              :UNION ((:E_VAR NIL "haz-ob")
                                      (:E_VAR NIL "obs")
                                      (:E_OP NIL
                                       :SEQ ((:E_VAR NIL "ob")
                                             (:E_VAR NIL "ob"))))))))))
    (implies (and (not err)
                  (equal (env-lookup "obs" env) (v_empty))
                  (equal (env-lookup "haz-ob" env) (v_empty))
                  (equal (env-lookup "ob" env) (v_empty)))
             (equal res (env-update "ob" (v_empty) env))))
  :hints (("goal" :use ((:instance eval-fixpoint-empty
                         (var "ob")
                         (loc2 nil)
                         (loc3 nil)
                         (loc4 nil)
                         (loc5 nil)
                         (loc6 nil)
                         (base (e_op nil :union (list (e_Var nil "haz-ob")
                                                      (e_var nil "obs")))))
                        (:instance bindinglist-eval-equiv-implies-err/res-equiv-eval-fixpoint-1
                         (x '(((LOC)
                         (PAT :PVAR "ob")
                         (EXP :E_OP NIL
                              :UNION ((:E_VAR NIL "haz-ob")
                                      (:E_VAR NIL "obs")
                                      (:E_OP NIL
                                       :SEQ ((:E_VAR NIL "ob")
                                             (:E_VAR NIL "ob"))))))))
                         (x-equiv '(((LOC)
                                     (PAT :PVAR "ob")
                                     (EXP :E_OP NIL
                                          :UNION ((:E_OP NIL
                                                   :UNION ((:E_VAR NIL "haz-ob")
                                                           (:E_VAR NIL "obs")))
                                                  (:E_OP NIL
                                                   :SEQ ((:E_VAR NIL "ob")
                                                         (:E_VAR NIL "ob")))))))))
                        (:instance exp-equiv-of-union-assoc
                         (loc1 nil) (loc2 nil)
                         (x (e_var nil "haz-ob"))
                         (y (e_var nil "obs"))
                         (z (e_op nil :seq (list (e_var nil "ob") (e_var nil "ob"))))))
           :in-theory (e/d (bindinglist-eval-equiv
                            binding-eval-equiv)
                           (eval-fixpoint-empty
                            bindinglist-eval-equiv-implies-err/res-equiv-eval-fixpoint-1)))))

(defthm fixpoint-is-closure
  (b* (((mv err res) (eval-fixpoint
                      '(((LOC)
                         (PAT :PVAR "ob")
                         (EXP :E_OP NIL
                              :UNION ((:E_VAR NIL "haz-ob")
                                      (:E_VAR NIL "obs")
                                      (:E_OP NIL
                                       :SEQ ((:E_VAR NIL "ob")
                                             (:E_VAR NIL "ob")))))))))
       (obs-val (env-lookup "obs" env))
       (haz-ob-val (env-lookup "haz-ob" env))
       (union-rel (union (val-case haz-ob-val
                           :v_rel haz-ob-val.rel
                           :otherwise nil)
                         (val-case obs-val
                           :v_rel obs-val.rel
                           :otherwise nil))))
    (implies (and (not err)
                  (or (and (equal obs-val (v_empty))
                           (val-case haz-ob-val :v_rel))
                      (and (val-case obs-val :v_rel)
                           (equal haz-ob-val (v_empty)))
                      (and (val-case haz-ob-val :v_rel)
                           (val-case obs-val :v_rel)))
                  (equal (env-lookup "ob" env) (v_empty)))
             (equal res (env-update "ob" (v_rel (transitive-closure union-rel)) env))))
  :hints (("goal" :use ((:instance eval-fixpoint-is-transitive-closure
                         (var "ob")
                         (loc2 nil)
                         (loc3 nil)
                         (loc4 nil)
                         (loc5 nil)
                         (loc6 nil)
                         (base (e_op nil :union (list (e_Var nil "haz-ob")
                                                      (e_var nil "obs")))))
                        (:instance bindinglist-eval-equiv-implies-err/res-equiv-eval-fixpoint-1
                         (x '(((LOC)
                         (PAT :PVAR "ob")
                         (EXP :E_OP NIL
                              :UNION ((:E_VAR NIL "haz-ob")
                                      (:E_VAR NIL "obs")
                                      (:E_OP NIL
                                       :SEQ ((:E_VAR NIL "ob")
                                             (:E_VAR NIL "ob"))))))))
                         (x-equiv '(((LOC)
                                     (PAT :PVAR "ob")
                                     (EXP :E_OP NIL
                                          :UNION ((:E_OP NIL
                                                   :UNION ((:E_VAR NIL "haz-ob")
                                                           (:E_VAR NIL "obs")))
                                                  (:E_OP NIL
                                                   :SEQ ((:E_VAR NIL "ob")
                                                         (:E_VAR NIL "ob")))))))))
                        (:instance exp-equiv-of-union-assoc
                         (loc1 nil) (loc2 nil)
                         (x (e_var nil "haz-ob"))
                         (y (e_var nil "obs"))
                         (z (e_op nil :seq (list (e_var nil "ob") (e_var nil "ob"))))))
           :in-theory (e/d (bindinglist-eval-equiv
                            binding-eval-equiv)
                           (eval-fixpoint-is-transitive-closure
                            bindinglist-eval-equiv-implies-err/res-equiv-eval-fixpoint-1)))))

(local (in-theory (enable eval-var-special
                          eval-test
                          eval-do_test)))

(defun env-updates-fn (args)
  (if (atom (cdr args))
      (car args)
    `(env-update ,(car args)
                 ,(cadr args)
                 ,(env-updates-fn (cddr args)))))
        
    

(defmacro env-updates (&rest args)
  (env-updates-fn args))

(define haz1fragment-baserel ((co relation-p) (ex execgraph-p))
  (b* ((evts (execgraph->evts ex))
       (rf (rf-relation evts))
       (ext (ext-relation evts))
       (loc (loc-relation evts))
       (po (po-relation evts))
       (r (evtlist-filter-type :evt-r evts))
       ([r] (id-relation r))
       (w (evtlist-filter-type :evt-w evts))
       ([w] (id-relation w))
       (m (append w r))
       ([m] (id-relation m))
       (id (id-relation evts))
       (invrf (inverse rf))
       (fr (difference (compose invrf co) id))
       (ca (union fr (relation-fix co)))
       (haz-ob (compose* (list [r] (intersect po loc) [r] (intersect ca ext) [w])))
       (obs (union (compose* (list [w] (intersect rf ext) [r]))
                   (compose* (list [m] (intersect ca ext) [w])))))
    (union haz-ob obs)))

(define haz2fragment-baserel ((co relation-p) (ex execgraph-p))
  (b* ((evts (execgraph->evts ex))
       (rf (rf-relation evts))
       (ext (ext-relation evts))
       (loc (loc-relation evts))
       (po (po-relation evts))
       (r (evtlist-filter-type :evt-r evts))
       ([r] (id-relation r))
       (w (evtlist-filter-type :evt-w evts))
       ([w] (id-relation w))
       (m (append w r))
       ([m] (id-relation m))
       (id (id-relation evts))
       (invrf (inverse rf))
       (fr (difference (compose invrf co) id))
       (ca (union fr (relation-fix co)))
       (haz-ob (compose* (list [w] (intersect rf ext) [r] (intersect po loc) [r])))
       (obs (union (compose* (list [w] (intersect rf ext) [r]))
                   (compose* (list [m] (intersect ca ext) [w])))))
    (union haz-ob obs)))


(define haz1fragment-spec ((co relation-p) (ex execgraph-p))
  :returns (results resultlist-p)
  (b* ((evts (execgraph->evts ex))
       (rf (rf-relation evts))
       (ext (ext-relation evts))
       (loc (loc-relation evts))
       (po (po-relation evts))
       (r (evtlist-filter-type :evt-r evts))
       ([r] (id-relation r))
       (w (evtlist-filter-type :evt-w evts))
       ([w] (id-relation w))
       (m (append w r))
       ([m] (id-relation m))
       (id (id-relation evts))
       (invrf (inverse rf))
       (fr (difference (compose invrf co) id))
       (ca (union fr (relation-fix co)))
       (haz-ob (compose* (list [r] (intersect po loc) [r] (intersect ca ext) [w])))
       (obs (union (compose* (list [w] (intersect rf ext) [r]))
                   (compose* (list [m] (intersect ca ext) [w]))))
       (ob (transitive-closure (union haz-ob obs)))
       (external (not (test-irreflexive ob)))
       (res (make-result :judgement :allowed
                         :flags (and external '("external"))
                         :env (env-updates
                               "ob" (v_rel ob)
                               "obs" (v_rel obs)
                               "haz-ob" (v_rel haz-ob)
                               "ca" (v_rel ca)
                               "fr" (v_rel fr)
                               "invrf" (v_rel invrf)
                               "co" (v_rel co)))))
    (list res)))

(define haz2fragment-spec ((co relation-p) (ex execgraph-p))
  :returns (results resultlist-p)
  (b* ((evts (execgraph->evts ex))
       (rf (rf-relation evts))
       (ext (ext-relation evts))
       (loc (loc-relation evts))
       (po (po-relation evts))
       (r (evtlist-filter-type :evt-r evts))
       ([r] (id-relation r))
       (w (evtlist-filter-type :evt-w evts))
       ([w] (id-relation w))
       (m (append w r))
       ([m] (id-relation m))
       (id (id-relation evts))
       (invrf (inverse rf))
       (fr (difference (compose invrf co) id))
       (ca (union fr (relation-fix co)))
       (haz-ob (compose* (list [w] (intersect rf ext) [r] (intersect po loc) [r])))
       (obs (union (compose* (list [w] (intersect rf ext) [r]))
                   (compose* (list [m] (intersect ca ext) [w]))))
       (ob (transitive-closure (union haz-ob obs)))
       (external (not (test-irreflexive ob)))
       (res (make-result :judgement :allowed
                         :flags (and external '("external"))
                         :env (env-updates
                               "ob" (v_rel ob)
                               "obs" (v_rel obs)
                               "haz-ob" (v_rel haz-ob)
                               "ca" (v_rel ca)
                               "fr" (v_rel fr)
                               "invrf" (v_rel invrf)
                               "co" (v_rel co)))))
    (list res)))

(defthm haz1fragment-computes-spec
 (b* (((mv err results)
       (eval-inslist (haz1fragment)
                     :result (make-result :judgement :allowed
                                          :env (env-update "co" (v_rel co) nil)))))
   (implies (not err)
            (equal results
                   (haz1fragment-spec co ex))))
 :hints(("Goal" :in-theory (enable haz1fragment-spec)
         :expand ((:free (x y) (compose* (cons x y)))))))

(defthm haz2fragment-computes-spec
 (b* (((mv err results)
       (eval-inslist (haz2fragment)
                     :result (make-result :judgement :allowed
                                          :env (env-update "co" (v_rel co) nil)))))
   (implies (not err)
            (equal results
                   (haz2fragment-spec co ex))))
 :hints(("Goal" :in-theory (enable haz2fragment-spec compose*))))


(defun-sk has-loop (x)
  (exists path
          (and (relation-path-p path x)
               (equal (car path) (car (last path))))))

(local (in-theory (disable has-loop
                           has-loop-suff)))

(local (include-book "std/util/termhints" :dir :system))

(defthm test-irreflexive-of-closure-iff-loop
  (iff (test-irreflexive (transitive-closure x))
       (not (has-loop x)))
  :hints ((acl2::use-termhint
           (if (has-loop x)
               (b* ((loop (has-loop-witness x))
                    (evt (car loop)))
                 `(:use ((:instance test-irreflexive-necc
                          (x (transitive-closure x))
                          (evt ,(acl2::hq evt)))
                         (:instance in-transitive-closure-when-path
                          (path ,(acl2::hq loop))))
                   :expand ((has-loop x))
                   :in-theory (disable test-irreflexive-necc
                                       in-transitive-closure-when-path)))
             (b* ((evt (test-irreflexive-badguy (transitive-closure x)))
                  (path (transitive-path evt evt x)))
               `(:use ((:instance has-loop-suff
                        (path ,(acl2::hq path)))
                       (:instance self-pair-exists-when-not-test-irreflexive
                        (x (transitive-closure x)))
                       (:instance transitive-path-correct
                        (src ,(acl2::hq evt)) (dst ,(acl2::hq evt))))
                 :in-theory (disable self-pair-exists-when-not-test-irreflexive
                                     transitive-path-correct
                                     test-irreflexive-by-badguy)))))))

(defthm relation-path-p-of-append-single
  (implies (and (relation-path-p x rel)
                (in (evtpair (car (last x)) y) (relation-fix rel)))
           (relation-path-p (append x (list y)) rel))
  :hints(("Goal" :in-theory (enable relation-path-p))))


(local (defthm car-last-of-append-singleton
         (equal (car (last (append lst (list x))))
                x)))


(define permute-loop-back ((x evtlist-p))
  :guard (consp (cdr x))
  :returns (new-x evtlist-p)
  (append (evtlist-fix (cdr x)) (list (evt-fix (cadr x))))
  ///
  (defret permute-loop-back-preserves-relation-path-p
    (implies (and (relation-path-p x rel)
                  (equal (evt-fix (car x)) (evt-fix (car (last x)))))
             (relation-path-p new-x rel))
    :hints (("goal" :in-theory (enable relation-path-p))))

  
  
  (defret permute-loop-back-endpoints
    (and (equal (car new-x) (evt-fix (cadr x)))
         (equal (car (last new-x))
                (evt-fix (cadr x))))))

(defthm evtlist-p-of-take
  (implies (and (<= (nfix n) (len x))
                (evtlist-p x))
           (evtlist-p (take n x))))

(defthm evt-p-of-nth
  (implies (and (< (nfix n) (len x))
                (evtlist-p x))
           (evt-p (nth n x))))

(local
 (progn
   (defthm len-equal-0
     (equal (equal (len x) 0)
            (not (consp x))))

   (defthm car-of-take
     (implies (<= 1 (nfix n))
              (equal (car (take n x)) (car x))))

   (defthm consp-of-take
     (equal (consp (take n x))
            (<= 1 (nfix n))))

   (defthm nth-of-take
     (equal (nth n (take m x))
            (and (< (nfix n) (nfix m))
                 (nth n x))))))
         


(defthm relation-path-p-of-take
  (implies (and (relation-path-p x rel)
                (<= (nfix n) (len x)))
           (equal (relation-path-p (take n x) rel)
                  (<= 2 (nfix n))))
  :hints(("Goal" :induct (take n x)
          :expand ((relation-path-p x rel)
                   (relation-path-p nil rel)
                   (:free (x y) (relation-path-p (cons x y) rel))))))



(define permute-loop-forward ((x evtlist-p))
  :guard (consp (cdr x))
  :returns (new-x evtlist-p)
  (let ((len (len x)))
    (if (<= len 2)
        (list (evt-fix (car x))
              (evt-fix (car x)))
      (cons (evt-fix (nth (- len 2) x))
            (evtlist-fix (take (- len 1) x)))))
  ///
  (local (defthm last-pair-in-relation
           (implies (and (relation-path-p x rel)
                         (equal (evt-fix dst) (evt-fix (car (last x))))
                         (equal (evt-fix src) (evt-fix (nth (- (len x) 2) x))))
                    (in (evtpair src dst) (relation-fix rel)))
           :hints(("Goal" :in-theory (enable relation-path-p)))))
  
  (defret permute-loop-forward-preserves-relation-path-p
    (implies (and (relation-path-p x rel)
                  (equal (evt-fix (car x)) (evt-fix (car (last x)))))
             (relation-path-p new-x rel))
    :hints (("goal" :expand ((relation-path-p x rel)
                             (:free (x y) (relation-path-p (cons x y) rel))
                             (:free (n) (nth n x))))))

  
  
  (defret permute-loop-forward-last
    (equal (car (last new-x))
           (car new-x)))

  (defret permute-loop-forward-first
    (equal (car new-x)
           (if (<= 3 (len x))
               (evt-fix (nth (- (len x) 2) x))
             (evt-fix (car x)))))

  (defret len-of-<fn>
    (equal (len new-x)
           (max 2 (len x))))

  (defret nth-of-<fn>
    (implies (and (< (nfix n) (len x))
                  (<= 3 (len x)))
             (equal (nth n new-x)
                    (if (zp n)
                        (evt-fix (nth (- (len x) 2) x))
                      (evt-fix (nth (1- n) x)))))))



(define haz1baserel-abstraction ((r evtlist-p)
                                 (w evtlist-p)
                                 (po&loc relation-p)
                                 (ca&ext relation-p)
                                 (rf&ext relation-p))
  :returns (haz1base relation-p)
  (b* (([r] (id-relation r))
       ([w] (id-relation w))
       ([m] (id-relation (append w r)))
       (haz-ob (compose* (list [r] po&loc [r] ca&ext [w])))
       (obs (union (compose* (list [w] rf&ext [r]))
                   (compose* (list [m] ca&ext [w])))))
    (union haz-ob obs)))

(define haz2baserel-abstraction ((r evtlist-p)
                                 (w evtlist-p)
                                 (po&loc relation-p)
                                 (ca&ext relation-p)
                                 (rf&ext relation-p))
  :returns (haz2base relation-p)
  (b* (([r] (id-relation r))
       ([w] (id-relation w))
       ([m] (id-relation (append w r)))
       (haz-ob (compose* (list [w] rf&ext [r] po&loc [r])))
       (obs (union (compose* (list [w] rf&ext [r]))
                   (compose* (list [m] ca&ext [w])))))
    (union haz-ob obs)))


(defthm car-last-of-evtlist-fix
  (implies (consp x)
           (equal (car (last (evtlist-fix x)))
                  (evt-fix (car (last x))))))


(defthm in-compose*-id-relation-first
  (implies (in (evtpair src dst) (compose* (cons (id-relation d) rest)))
           (member-equal (Evt-fix src) (evtlist-fix d)))
  :hints (("goal" :use ((:instance compose-path-p-of-compose*-path
                         (x (cons (id-relation d) rest))))
           :in-theory (e/d (compose-path-p)
                           (compose-path-p-of-compose*-path)))))

(defthm in-compose*-id-relation-last-lemma
  (implies (and (compose-path-p path x)
                (equal (car (last x)) (id-relation r)))
           (member-equal (evt-fix (car (last path))) (evtlist-fix r)))
  :hints(("Goal" :in-theory (enable compose-path-p))))

(defthm in-compose*-id-relation-last
  (implies (and (in (evtpair src dst) (compose* x))
                (equal lastrel (car (last x)))
                (equal lastrel (id-relation r)))
           (member-equal (Evt-fix dst) (evtlist-fix r)))
  :hints (("goal" :use ((:instance compose-path-p-of-compose*-path)
                        (:instance in-compose*-id-relation-last-lemma
                         (path (compose*-path src dst x))))
           :in-theory (e/d (compose-path-p)
                           (compose-path-p-of-compose*-path
                            in-compose*-id-relation-last-lemma)))))


(defthm consp-cdr-when-relation-path-p
  (implies (relation-path-p x rel)
           (consp (cdr x)))
  :hints(("Goal" :in-theory (enable relation-path-p)))
  :rule-classes :forward-chaining)

(define haz-loop-normalize-to-w-first ((hazloop evtlist-p)
                                        (r evtlist-p)
                                        (w evtlist-p)
                                        (po&loc relation-p)
                                        (ca&ext relation-p)
                                        (rf&ext relation-p))
  (declare (ignorable w po&loc ca&ext rf&ext))
  :guard (consp (cdr hazloop))
  :returns (new-loop evtlist-p)
  :verify-guards nil
  (if (member-equal (evt-fix (car hazloop)) (evtlist-fix r))
      (permute-loop-forward hazloop)
    (evtlist-fix hazloop))
  ///
  
  (verify-guards haz-loop-normalize-to-w-first)
  
  (defret <fn>-endpoints
    (implies (equal (evt-fix (car hazloop)) (evt-fix (car (last hazloop))))
             (equal (car (last new-loop))
                    (car new-loop))))


  (defret <fn>-preserves-haz1-path
    (b* ((baserel (haz1baserel-abstraction r w po&loc ca&ext rf&ext)))
      (implies (and (relation-path-p hazloop baserel)
                    (equal (evt-fix (car hazloop)) (evt-fix (car (last hazloop)))))
               (relation-path-p new-loop baserel))))

  (defret <fn>-preserves-haz2-path
    (b* ((baserel (haz2baserel-abstraction r w po&loc ca&ext rf&ext)))
      (implies (and (relation-path-p hazloop baserel)
                    (equal (evt-fix (car hazloop)) (evt-fix (car (last hazloop)))))
               (relation-path-p new-loop baserel))))

  (local
   (defthm intersectp-equal-bind-free
     (implies (and (bind-free '((key . (evt-fix$inline (car hazloop)))))
                   (member-equal key x)
                   (member-equal key y))
              (intersectp-equal x y))))

  (local
   (defthm intersectp-equal-bind-free-2
     (implies (and (bind-free '((key . (EVT-FIX$INLINE (NTH (BINARY-+ '-1 (LEN (cdr  HAZLOOP)))
                                                            hazloop)))))
                   (member-equal key x)
                   (member-equal key y))
              (intersectp-equal x y))))

  (local (defthm relation-path-p-implies-len
           (implies (relation-path-p x rel)
                    (<= 2 (len x)))
           :hints(("Goal" :in-theory (enable relation-path-p)))
           :rule-classes :forward-chaining))

  (local (defthm relation-path-p-implies-last-pair
           (implies (and (relation-path-p x rel)
                         (<= 2 (len x)))
                    (in (evtpair (nth (+ -2 (len x)) x)
                                 (car (last x)))
                        (relation-fix rel)))
           :hints(("Goal" :in-theory (enable relation-path-p)))))
                    
  
  (defret <fn>-write-first-haz1
    (b* ((baserel (haz1baserel-abstraction r w po&loc ca&ext rf&ext)))
      (implies (and (relation-path-p hazloop baserel)
                    (equal (evt-fix (car hazloop)) (evt-fix (car (last hazloop))))
                    (not (intersectp-equal (evtlist-fix r) (evtlist-fix w))))
               (and (member-equal (evt-fix (car new-loop)) (evtlist-fix w))
                    (not (member-equal (evt-fix (car new-loop)) (evtlist-fix r))))))
    :hints(("Goal" :in-theory (e/d (haz1baserel-abstraction))
            :expand ((:free (rel) (relation-path-p hazloop rel)))
            :do-not-induct t)
           (and stable-under-simplificationp
                '(:use ((:instance relation-path-p-implies-last-pair
                         (rel (haz1baserel-abstraction r w po&loc ca&ext rf&ext))
                         (x ;; (haz-loop-normalize-to-w-first hazloop r w po&loc ca&ext rf&ext)
                          hazloop)))
                  :in-theory (e/d () (relation-path-p-implies-last-pair
                                      haz-loop-normalize-to-w-first))))
           (and stable-under-simplificationp
                '(:in-theory (e/d (haz1baserel-abstraction)
                                  (relation-path-p-implies-last-pair)))))
    :otf-flg t)

  (defret <fn>-write-first-haz1-nofix
    (b* ((baserel (haz1baserel-abstraction r w po&loc ca&ext rf&ext)))
      (implies (and (relation-path-p hazloop baserel)
                    (evtlist-p w)
                    (equal (evt-fix (car hazloop)) (evt-fix (car (last hazloop))))
                    (not (intersectp-equal (evtlist-fix r) (evtlist-fix w))))
               (member-equal (car new-loop) w)))
    :hints (("goal" :use <fn>-write-first-haz1)))

  (defret <fn>-write-first-haz2
    (b* ((baserel (haz2baserel-abstraction r w po&loc ca&ext rf&ext)))
      (implies (and (relation-path-p hazloop baserel)
                    (equal (evt-fix (car hazloop)) (evt-fix (car (last hazloop))))
                    (not (intersectp-equal (evtlist-fix r) (evtlist-fix w))))
               (and (member-equal (evt-fix (car new-loop)) (evtlist-fix w))
                    (not (member-equal (evt-fix (car new-loop)) (evtlist-fix r))))))
    :hints(("Goal" :in-theory (e/d (haz2baserel-abstraction))
            :expand ((:free (rel) (relation-path-p hazloop rel)))
            :do-not-induct t)
           (and stable-under-simplificationp
                '(:use ((:instance relation-path-p-implies-last-pair
                         (rel (haz2baserel-abstraction r w po&loc ca&ext rf&ext))
                         (x ;; (haz-loop-normalize-to-w-first hazloop r w po&loc ca&ext rf&ext)
                          hazloop)))
                  :in-theory (e/d () (relation-path-p-implies-last-pair
                                      haz-loop-normalize-to-w-first))))
           (and stable-under-simplificationp
                '(:in-theory (e/d (haz2baserel-abstraction)
                                  (relation-path-p-implies-last-pair)))))
    :otf-flg t)

  (defret <fn>-write-first-haz2-nofix
    (b* ((baserel (haz2baserel-abstraction r w po&loc ca&ext rf&ext)))
      (implies (and (relation-path-p hazloop baserel)
                    (evtlist-p w)
                    (equal (evt-fix (car hazloop)) (evt-fix (car (last hazloop))))
                    (not (intersectp-equal (evtlist-fix r) (evtlist-fix w))))
               (member-equal (car new-loop) w)))
    :hints (("goal" :use <fn>-write-first-haz2))))


;; (defines collect-compose*-path-terms
;;   (define collect-compose*-path-terms ((x pseudo-termp) acc)
;;     :measure (acl2-count x)
;;     :returns (mv is-path new-acc)
;;     (cond ((atom x) (mv nil acc))
;;           ((eq (car x) 'quote) (mv nil acc))
;;           ((consp (car x))
;;            (mv nil (collect-compose*-path-terms-lst (cdr x) acc)))
;;           ((eq (car x) 'compose*-path) (mv t acc))
;;           ((and (eq (car x) 'car)
;;                 (consp (cdr x)))
;;            (b* (((mv is-path acc) (collect-compose*-path-terms (cadr x) acc)))
;;              (mv nil (if is-path (cons x acc) acc))))
;;           ((and (eq (car x) 'cdr)
;;                 (consp (cdr x)))
;;            (collect-compose*-path-terms (cadr x) acc))
;;           (t (mv nil (collect-compose*-path-terms-lst (cdr x) acc)))))

;;   (define collect-compose*-path-terms-lst ((x pseudo-term-listp) acc)
;;     :measure (acl2-count x)
;;     (b* (((when (atom x)) acc)
;;          ((mv & acc) (collect-compose*-path-terms (car x) acc)))
;;       (collect-compose*-path-terms-lst (cdr x) acc))))


;; (local (include-book "clause-processors/generalize" :dir :system))

(defsection compose-lemmas
  (defthm in-compose-id
    (iff (in pair (compose (id-relation r) x))
         (and (evtpair-p pair)
              (member-equal (evtpair->from pair) (evtlist-fix r))
              (in pair (relation-fix x))))
    :hints(("Goal" :in-theory (enable member-of-compose-rw)
            :use ((:instance member-of-compose-suff
                   (x (id-relation r)) (y x)
                   (mid (evtpair->from pair)))))))

  (defthm in-compose-id-2
    (iff (in pair (compose x (id-relation r)))
         (and (evtpair-p pair)
              (member-equal (evtpair->to pair) (evtlist-fix r))
              (in pair (relation-fix x))))
    :hints(("Goal" :in-theory (enable member-of-compose-rw)
            :use ((:instance member-of-compose-suff
                   (x x) (y (id-relation r))
                   (mid (evtpair->to pair)))))))

  (defthm in-compose*-id
    (iff (in pair (compose* (cons (id-relation r) x)))
         (and (evtpair-p pair)
              (member-equal (evtpair->from pair) (evtlist-fix r))
              (if (atom x)
                  (equal (evtpair->to pair) (evtpair->from pair))
                (in pair (compose* x)))))
    :hints(("Goal" :in-theory (enable compose*))))

  (defthm in-compose*-id-2
    (iff (in pair (compose* (list x (id-relation r))))
         (and (evtpair-p pair)
              (member-equal (evtpair->to pair) (evtlist-fix r))
              (in pair (relation-fix x))))
    :hints(("Goal" :in-theory (enable compose*))))

  (defthm compose-midpoint-of-id-relation
    (implies (and (member-equal (evt-fix src) (evtlist-fix r))
                  (in (evtpair src dst) (relation-fix y)))
             (equal (compose-midpoint src dst (id-relation r) y)
                    (evt-fix src)))
    :hints (("goal" :use ((:instance compose-midpoint-witnesses
                           (mid1 (evt-fix src))
                           (x (id-relation r))))
             :in-theory (disable compose-midpoint-witnesses))))

  (defthm compose-midpoint-of-id-relation-2
    (implies (and (member-equal (evt-fix dst) (evtlist-fix r))
                  (in (evtpair src dst) (relation-fix x)))
             (equal (compose-midpoint src dst x (id-relation r))
                    (evt-fix dst)))
    :hints (("goal" :use ((:instance compose-midpoint-witnesses
                           (mid1 (evt-fix dst))
                           (y (id-relation r))))
             :in-theory (disable compose-midpoint-witnesses)))))





(local (defthm member-append
         (iff (member-equal k (append x y))
              (or (member-equal k x) (member-equal k y)))))

(define haz2-loop-from-haz1-loop-aux ((haz1loop evtlist-p)
                                      (r evtlist-p)
                                      (w evtlist-p)
                                      (po&loc relation-p)
                                      (ca&ext relation-p)
                                      (rf&ext relation-p))
  :guard (and (relation-path-p haz1loop (haz1baserel-abstraction r w po&loc ca&ext rf&ext))
              ;; (equal (car haz1loop) (car (last haz1loop)))
              (member-equal (car haz1loop) w)
              (member-equal (car (last haz1loop)) w)
              (not (intersectp-equal r w)))
  :verify-guards nil
  :ruler-extenders (:lambdas cons)
  :returns (path evtlist-p)
  (b* (;; (pair (evtpair (car haz1loop) (cadr haz1loop)))
       ([r] (id-relation r))
       ([w] (id-relation w))
       ;; (haz-ob (compose* (list [r] po&loc [r] ca&ext [w])))
       ;; (obs1  (compose* (list [w] rf&ext [r])))
       (a (evt-fix (car haz1loop))) ;; write
       (b (evt-fix (cadr haz1loop)))
       ((when (member-equal b (evtlist-fix w)))
        ;; (member-equal pair obs2))
        ;; The pair can only be in obs2, which is also in the haz1 relation.
        (cons a
              (if (consp (cddr haz1loop))
                  (haz2-loop-from-haz1-loop-aux (cdr haz1loop)
                                                r w po&loc ca&ext rf&ext)
                (list b))))
       ;; The first pair must be in obs1; the next pair must either be in haz-ob or obs2.
       ;; If obs2, just collect both and go.
       ([m] (id-relation (append w r)))
       (obs2  (compose* (list [m] ca&ext [w])))
       (c (evt-fix (caddr haz1loop))) ;; write, (b c) in haz-ob1
       ((when (in (evtpair b c) obs2))
        (list* a b
               (if (consp (cdddr haz1loop))
                   (haz2-loop-from-haz1-loop-aux (cddr haz1loop)
                                                 r w po&loc ca&ext rf&ext)
                 (list c))))
       ;; Rework this into a pair that is in haz2's haz-ob, then a pair that's in obs2.
       ;; I.e., b is a read, (a b) in rf&ext
       (haz-ob-path (compose*-path b c (list [r] po&loc [r] ca&ext [w])))
       (new-b (nth 2 haz-ob-path)))
    (list* a new-b
           (if (consp (cdddr haz1loop))
               (haz2-loop-from-haz1-loop-aux (cddr haz1loop)
                                             r w po&loc ca&ext rf&ext)
             (list c))))
  ///

  (local
   (defthm intersectp-equal-bind-free
     (implies (and (bind-free '((key . (evt-fix$inline (car haz1loop)))))
                   (member-equal key x)
                   (member-equal key y))
              (intersectp-equal x y))))

  (local
   (defthm intersectp-equal-bind-free2
     (implies (and (bind-free '((key . (evt-fix$inline (car (cdr haz1loop))))))
                   (member-equal key x)
                   (member-equal key y))
              (intersectp-equal x y))))
  
  (defret endpoints-of-<fn>
    (and (equal (car path) (evt-fix (car haz1loop)))
         (implies (and (relation-path-p haz1loop (haz1baserel-abstraction r w po&loc ca&ext rf&ext))
                       (member-equal (evt-fix (car haz1loop)) (evtlist-fix w))
                       (member-equal (evt-fix (car (last haz1loop))) (evtlist-fix w))
                       (not (intersectp-equal (evtlist-fix r) (evtlist-fix w))))
                  (equal (car (last path))
                         (evt-fix (car (last haz1loop))))))
    :hints(("Goal" :in-theory (enable haz1baserel-abstraction)
            :induct <call>
            :do-not-induct t)
           (and stable-under-simplificationp
                '(:expand ((:free (rel) (relation-path-p haz1loop rel)))))
           (and stable-under-simplificationp
                '(:expand ((:free (rel) (relation-path-p (cdr haz1loop) rel)))))))

  (local (defthm nth-when-consp
           (implies (and (posp n)
                         (consp x))
                    (equal (nth n x)
                           (nth (1- n) (cdr x))))))
  
  (defret haz2-path-of-<fn>
    (b* ((haz1 (haz1baserel-abstraction r w po&loc ca&ext rf&ext))
         (haz2 (haz2baserel-abstraction r w po&loc ca&ext rf&ext)))
      (implies (and (relation-path-p haz1loop haz1)
                    (member-equal (evt-fix (car haz1loop)) (evtlist-fix w))
                    (member-equal (evt-fix (car (last haz1loop))) (evtlist-fix w))
                    (not (intersectp-equal (evtlist-fix r) (evtlist-fix w))))
               (relation-path-p path haz2)))
    :hints(("Goal" :in-theory (e/d (haz2baserel-abstraction
                                    haz1baserel-abstraction)
                                   ;; ((:d <fn>))
                                   )
            :induct <call>
             :do-not-induct t
            )
           (and stable-under-simplificationp
                '(:expand ((:free (rel) (relation-path-p haz1loop rel))
                           (:free (rel x y) (relation-path-p (cons x y) rel)))))
           (and stable-under-simplificationp
                '(:expand (;; (:free (src dst x y) (compose*-path src dst (cons x y)))
                           (:free (x y) (compose* (cons x y)))
                           (:free (x a b) (compose-path-p x (cons a b))))))
           (and stable-under-simplificationp
                (member-equal '(MEMBER-EQUAL (EVT-FIX$INLINE (CAR (CDR HAZ1LOOP)))
                                             (EVTLIST-FIX$INLINE W))
                              clause)
                '(;; :computed-hint-replacement
                  ;; ((and stable-under-simplificationp
                  ;;       (let ((elems (mergesort (collect-compose*-path-terms-lst clause nil))))
                  ;;         (and elems
                  ;;              `(:clause-processor
                  ;;                (acl2::generalize-with-alist-cp
                  ;;                 clause ',(pairlis$ elems
                  ;;                                    (make-list (len elems)
                  ;;                                               :initial-element 'e))))))))
                  :use ((:instance compose-path-p-of-compose*-path
                         (src (cadr haz1loop)) (dst (caddr haz1loop))
                         (x (list (id-relation r) po&loc (id-relation r) ca&ext (id-relation w))))
                        (:instance compose*-path-endpoints
                         (src (cadr haz1loop)) (dst (caddr haz1loop))
                         (x (list (id-relation r) po&loc (id-relation r) ca&ext (id-relation w)))))
                  :in-theory (disable compose-path-p-of-compose*-path
                                      compose*-path-endpoints)))
           (and stable-under-simplificationp
                '(:expand ((:free (rel) (relation-path-p (cdr haz1loop) rel)))
                  :in-theory (e/d (member-of-compose-rw
                                   member-of-compose-suff-rw
                                   member-of-compose-suff-rw2)
                                  (compose-path-p-of-compose*-path
                                   compose*-path-endpoints))))))
  (local
   (defthm intersectp-equal-bind-free-nofix
     (implies (and (bind-free '((key . (car haz1loop))))
                   (member-equal key x)
                   (member-equal key y))
              (intersectp-equal x y))))

  (local
   (defthm intersectp-equal-bind-free2-nofix
     (implies (and (bind-free '((key . (car (cdr haz1loop)))))
                   (member-equal key x)
                   (member-equal key y))
              (intersectp-equal x y))))
  
  (verify-guards haz2-loop-from-haz1-loop-aux
    :hints (("goal" :do-not-induct t
             :expand ((:free (rel) (relation-path-p nil rel))
                      (:free (rel) (relation-path-p haz1loop rel))
                      (:free (rel) (relation-path-p (cdr haz1loop) rel))
                      (evtlist-p haz1loop)
                      (evtlist-p (cdr haz1loop))
                      (:free (path x y) (compose-path-p path (cons x y))))
             :in-theory (e/d (haz1baserel-abstraction
                              in-of-compose*-rw)
                             (compose*-path-endpoints))))))



       






;; (define haz1baserel-abstraction ((r evtlist-p)
;;                                  (w evtlist-p)
;;                                  (po&loc relation-p)
;;                                  (ca&ext relation-p)
;;                                  (rf&ext relation-p))
;;   :returns (haz1base relation-p)
;;   (b* (([r] (id-relation r))
;;        ([w] (id-relation w))
;;        ([m] (id-relation (append w r)))
;;        (haz-ob (compose* (list [r] po&loc [r] ca&ext [w])))
;;        (obs (union (compose* (list [w] rf&ext [r]))
;;                    (compose* (list [m] ca&ext [w])))))
;;     (union haz-ob obs)))  


;; (define haz2baserel-abstraction ((r evtlist-p)
;;                                  (w evtlist-p)
;;                                  (po&loc relation-p)
;;                                  (ca&ext relation-p)
;;                                  (rf&ext relation-p))
;;   :returns (haz2base relation-p)
;;   (b* (([r] (id-relation r))
;;        ([w] (id-relation w))
;;        ([m] (id-relation (append w r)))
;;        (haz-ob (compose* (list [w] rf&ext [r] po&loc [r])))
;;        (obs (union (compose* (list [w] rf&ext [r]))
;;                    (compose* (list [m] ca&ext [w])))))
;;     (union haz-ob obs)))


(define haz1-loop-from-haz2-loop-aux ((haz2loop evtlist-p)
                                      (r evtlist-p)
                                      (w evtlist-p)
                                      (po&loc relation-p)
                                      (ca&ext relation-p)
                                      (rf&ext relation-p))
  :guard (and (relation-path-p haz2loop (haz2baserel-abstraction r w po&loc ca&ext rf&ext))
              ;; (equal (car haz2loop) (car (last haz2loop)))
              (member-equal (car haz2loop) w)
              (member-equal (car (last haz2loop)) w)
              (not (intersectp-equal r w)))
  :verify-guards nil
  :ruler-extenders (:lambdas cons)
  :returns (path evtlist-p)
  (b* (;; (pair (evtpair (car haz2loop) (cadr haz2loop)))
       ([r] (id-relation r))
       ([w] (id-relation w))
       ;; (haz-ob (compose* (list [r] po&loc [r] ca&ext [w])))
       ;; (obs1  (compose* (list [w] rf&ext [r])))
       (a (evt-fix (car haz2loop))) ;; write
       (b (evt-fix (cadr haz2loop)))
       (pair1 (evtpair a b))
       ((when (and (in pair1 (relation-fix ca&ext))
                   (member-equal b (evtlist-fix w))))
        ;; The first pair is in the [m] ca&ext [w] relation, which is also in haz1,
        ;; and we can continue from there since the next element is also a write.
        (cons a
              (if (consp (cddr haz2loop))
                  (haz1-loop-from-haz2-loop-aux (cdr haz2loop)
                                                r w po&loc ca&ext rf&ext)
                (list b))))
       (c (evt-fix (caddr haz2loop)))
       ((when (and (in pair1 (relation-fix rf&ext))
                   (member-equal b (evtlist-fix r))))
        ;; (a b) is in [w] rf&ext [r], therefore (b c) can only be in [m] ca&ext [w].
        ;; Both relations are also in haz1 and the latter ends in a write so we can then continue from there.
        (list* a b
               (if (consp (cdddr haz2loop))
                   (haz1-loop-from-haz2-loop-aux (cddr haz2loop)
                                                 r w po&loc ca&ext rf&ext)
                 (list c))))
       ;; (a b) is in haz-ob, which ends in [r], therefore (b c) can only be in [m] ca&ext [w].
       ;; We need to extract the middle element b' between a and b such that (a b') in [w] rf&ext [r], (b' b) in [r] po&loc [r],
       ;; so then (a b' c) is a path in haz1.
       (haz-ob-path (compose*-path a b (list [w] rf&ext [r] po&loc [r])))
       (new-b (nth 2 haz-ob-path)))
    (list* a new-b
           (if (consp (cdddr haz2loop))
               (haz1-loop-from-haz2-loop-aux (cddr haz2loop)
                                             r w po&loc ca&ext rf&ext)
             (list c))))
  ///

  (local
   (defthm intersectp-equal-bind-free
     (implies (and (bind-free '((key . (evt-fix$inline (car haz2loop)))))
                   (member-equal key x)
                   (member-equal key y))
              (intersectp-equal x y))))

  (local
   (defthm intersectp-equal-bind-free2
     (implies (and (bind-free '((key . (evt-fix$inline (car (cdr haz2loop))))))
                   (member-equal key x)
                   (member-equal key y))
              (intersectp-equal x y))))
  
  (defret endpoints-of-<fn>
    (and (equal (car path) (evt-fix (car haz2loop)))
         (implies (and (relation-path-p haz2loop (haz2baserel-abstraction r w po&loc ca&ext rf&ext))
                       (member-equal (evt-fix (car haz2loop)) (evtlist-fix w))
                       (member-equal (evt-fix (car (last haz2loop))) (evtlist-fix w))
                       (not (intersectp-equal (evtlist-fix r) (evtlist-fix w))))
                  (equal (car (last path))
                         (evt-fix (car (last haz2loop))))))
    :hints(("Goal" :in-theory (enable haz2baserel-abstraction)
            :induct <call>
            :do-not-induct t)
           (and stable-under-simplificationp
                '(:expand ((:free (rel) (relation-path-p haz2loop rel)))))
           (and stable-under-simplificationp
                '(:expand ((:free (rel) (relation-path-p (cdr haz2loop) rel)))))))

  (local (defthm nth-when-consp
           (implies (and (posp n)
                         (consp x))
                    (equal (nth n x)
                           (nth (1- n) (cdr x))))))
  
  (defret haz1-path-of-<fn>
    (b* ((haz2 (haz2baserel-abstraction r w po&loc ca&ext rf&ext))
         (haz1 (haz1baserel-abstraction r w po&loc ca&ext rf&ext)))
      (implies (and (relation-path-p haz2loop haz2)
                    (member-equal (evt-fix (car haz2loop)) (evtlist-fix w))
                    (member-equal (evt-fix (car (last haz2loop))) (evtlist-fix w))
                    (not (intersectp-equal (evtlist-fix r) (evtlist-fix w))))
               (relation-path-p path haz1)))
    :hints(("Goal" :in-theory (e/d (haz1baserel-abstraction
                                    haz2baserel-abstraction)
                                   ;; ((:d <fn>))
                                   )
            :induct <call>
             :do-not-induct t
             )
           
            
           (and stable-under-simplificationp
                '(:expand ((:free (rel) (relation-path-p haz2loop rel))
                           (:free (rel x y) (relation-path-p (cons x y) rel)))))
           (and stable-under-simplificationp
                '(:expand (;; (:free (src dst x y) (compose*-path src dst (cons x y)))
                           (:free (x y) (compose* (cons x y)))
                           (:free (x a b) (compose-path-p x (cons a b))))))
           (;; acl2::use-termhint
            ;; (b* (;; (pair (evtpair (car haz2loop) (cadr haz2loop)))
            ;;      ([r] (id-relation r))
            ;;      ([w] (id-relation w))
            ;;      ;; (haz-ob (compose* (list [r] po&loc [r] ca&ext [w])))
            ;;      ;; (obs1  (compose* (list [w] rf&ext [r])))
            ;;      (a (evt-fix (car haz2loop))) ;; write
            ;;      (b (evt-fix (cadr haz2loop)))
            ;;      (pair1 (evtpair a b))
            ;;      ((when (and (in pair1 (relation-fix ca&ext))
            ;;                  (member-equal b (evtlist-fix w))))
            ;;       ;; The first pair is in the [m] ca&ext [w] relation, which is also in haz1,
            ;;       ;; and we can continue from there since the next element is also a write.
            ;;       nil)
            ;;      (?c (evt-fix (caddr haz2loop)))
            ;;      ((when (and (in pair1 (relation-fix rf&ext))
            ;;                  (member-equal b (evtlist-fix r))))
            ;;       ;; (a b) is in [w] rf&ext [r], therefore (b c) can only be in [m] ca&ext [w].
            ;;       ;; Both relations are also in haz1 and the latter ends in a write so we can then continue from there.
            ;;       nil)
            ;;      ;; (a b) is in haz-ob, which ends in [r], therefore (b c) can only be in [m] ca&ext [w].
            ;;      ;; We need to extract the middle element b' between a and b such that (a b') in [w] rf&ext [r], (b' b) in [r] po&loc [r],
            ;;      ;; so then (a b' c) is a path in haz1.
            ;;      (haz-ob-path (compose*-path a b (list [w] rf&ext [r] po&loc [r])))
            ;;      (?new-b (nth 2 haz-ob-path)))
            and stable-under-simplificationp
              '(;; :computed-hint-replacement
                ;; ((and stable-under-simplificationp
                ;;       (let ((elems (mergesort (collect-compose*-path-terms-lst clause nil))))
                ;;         (and elems
                ;;              `(:clause-processor
                ;;                (acl2::generalize-with-alist-cp
                ;;                 clause ',(pairlis$ elems
                ;;                                    (make-list (len elems)
                ;;                                               :initial-element 'e))))))))
                :use ((:instance compose-path-p-of-compose*-path
                       (src (car haz2loop)) (dst (cadr haz2loop))
                       (x (list (id-relation w) rf&ext (id-relation r) po&loc (id-relation r))))
                      (:instance compose*-path-endpoints
                       (src (car haz2loop)) (dst (cadr haz2loop))
                       (x (list (id-relation w) rf&ext (id-relation r) po&loc (id-relation r)))))
                :in-theory (disable compose-path-p-of-compose*-path
                                    compose*-path-endpoints)))
           (and stable-under-simplificationp
                '(:expand ((:free (rel) (relation-path-p (cdr haz2loop) rel)))
                  :in-theory (e/d (member-of-compose-rw
                                   member-of-compose-suff-rw
                                   member-of-compose-suff-rw2)
                                  (compose-path-p-of-compose*-path
                                   compose*-path-endpoints))))))
  
  (local
   (defthm intersectp-equal-bind-free-nofix
     (implies (and (bind-free '((key . (car haz2loop))))
                   (member-equal key x)
                   (member-equal key y))
              (intersectp-equal x y))))

  (local
   (defthm intersectp-equal-bind-free2-nofix
     (implies (and (bind-free '((key . (car (cdr haz2loop)))))
                   (member-equal key x)
                   (member-equal key y))
              (intersectp-equal x y))))
  
  (verify-guards haz1-loop-from-haz2-loop-aux
    :hints (("goal" :do-not-induct t
             :expand ((:free (rel) (relation-path-p nil rel))
                      (:free (rel) (relation-path-p haz2loop rel))
                      (:free (rel) (relation-path-p (cdr haz2loop) rel))
                      (evtlist-p haz2loop)
                      (evtlist-p (cdr haz2loop)))
             :in-theory (e/d (haz2baserel-abstraction)
                             (compose*-path-endpoints)))
            (and stable-under-simplificationp
                 '(:expand ((:free (path x y) (compose-path-p path (cons x y))))
                   :in-theory (e/d (haz2baserel-abstraction
                                    in-of-compose*-rw)
                                   (compose*-path-endpoints)))))))



(define haz2-loop-from-haz1-loop ((haz1loop evtlist-p)
                                  (r evtlist-p)
                                  (w evtlist-p)
                                  (po&loc relation-p)
                                  (ca&ext relation-p)
                                  (rf&ext relation-p))
  :guard (and (relation-path-p haz1loop (haz1baserel-abstraction r w po&loc ca&ext rf&ext))
              (equal (car haz1loop) (car (last haz1loop)))
              (not (intersectp-equal r w)))
  :returns (path evtlist-p)
  (haz2-loop-from-haz1-loop-aux
   (haz-loop-normalize-to-w-first
    haz1loop r w po&loc ca&ext rf&ext)
   r w po&loc ca&ext rf&ext)
  ///
  (defret haz2-path-p-of-<fn>
    (implies (and (relation-path-p haz1loop (haz1baserel-abstraction r w po&loc ca&ext rf&ext))
                  (equal (evt-fix (car haz1loop))
                         (evt-fix (car (last haz1loop))))
                  (not (intersectp-equal (evtlist-fix r) (evtlist-fix w))))
             (relation-path-p path (haz2baserel-abstraction r w po&loc ca&ext rf&ext))))

  (defret endpoints-of-<fn>
    (implies (and (relation-path-p haz1loop (haz1baserel-abstraction r w po&loc ca&ext rf&ext))
                  (equal (evt-fix (car haz1loop))
                         (evt-fix (car (last haz1loop))))
                  (not (intersectp-equal (evtlist-fix r) (evtlist-fix w))))
             (equal (car (last path))
                    (car path))))

  (defthm has-loop-of-haz2baserel-abstraction-when-haz1
    (implies (and (has-loop (haz1baserel-abstraction r w po&loc ca&ext rf&ext))
                  (not (intersectp-equal (evtlist-fix r) (evtlist-fix w))))
             (has-loop (haz2baserel-abstraction r w po&loc ca&ext rf&ext)))
    :hints (("goal" :expand ((has-loop (haz1baserel-abstraction r w po&loc ca&ext rf&ext)))
             :use ((:instance has-loop-suff
                    (path (haz2-loop-from-haz1-loop
                           (has-loop-witness (haz1baserel-abstraction r w po&loc ca&ext rf&ext))
                           r w po&loc ca&ext rf&ext))
                    (x (haz2baserel-abstraction r w po&loc ca&ext rf&ext))))))))


(define haz1-loop-from-haz2-loop ((haz2loop evtlist-p)
                                  (r evtlist-p)
                                  (w evtlist-p)
                                  (po&loc relation-p)
                                  (ca&ext relation-p)
                                  (rf&ext relation-p))
  :guard (and (relation-path-p haz2loop (haz2baserel-abstraction r w po&loc ca&ext rf&ext))
              (equal (car haz2loop) (car (last haz2loop)))
              (not (intersectp-equal r w)))
  :returns (path evtlist-p)
  (haz1-loop-from-haz2-loop-aux
   (haz-loop-normalize-to-w-first
    haz2loop r w po&loc ca&ext rf&ext)
   r w po&loc ca&ext rf&ext)
  ///
  (defret haz1-path-p-of-<fn>
    (implies (and (relation-path-p haz2loop (haz2baserel-abstraction r w po&loc ca&ext rf&ext))
                  (equal (evt-fix (car haz2loop))
                         (evt-fix (car (last haz2loop))))
                  (not (intersectp-equal (evtlist-fix r) (evtlist-fix w))))
             (relation-path-p path (haz1baserel-abstraction r w po&loc ca&ext rf&ext))))

  (defret endpoints-of-<fn>
    (implies (and (relation-path-p haz2loop (haz2baserel-abstraction r w po&loc ca&ext rf&ext))
                  (equal (evt-fix (car haz2loop))
                         (evt-fix (car (last haz2loop))))
                  (not (intersectp-equal (evtlist-fix r) (evtlist-fix w))))
             (equal (car (last path))
                    (car path))))

  (defthm has-loop-of-haz1baserel-abstraction-when-haz2
    (implies (and (has-loop (haz2baserel-abstraction r w po&loc ca&ext rf&ext))
                  (not (intersectp-equal (evtlist-fix r) (evtlist-fix w))))
             (has-loop (haz1baserel-abstraction r w po&loc ca&ext rf&ext)))
    :hints (("goal" :expand ((has-loop (haz2baserel-abstraction r w po&loc ca&ext rf&ext)))
             :use ((:instance has-loop-suff
                    (path (haz1-loop-from-haz2-loop
                           (has-loop-witness (haz2baserel-abstraction r w po&loc ca&ext rf&ext))
                           r w po&loc ca&ext rf&ext))
                    (x (haz1baserel-abstraction r w po&loc ca&ext rf&ext))))))))




(defthmd haz1fragment-spec-in-terms-of-abstraction
  (equal (haz1fragment-spec co ex)
         (b* ((evts (execgraph->evts ex))
              (rf (rf-relation evts))
              (ext (ext-relation evts))
              (loc (loc-relation evts))
              (po (po-relation evts))
              (r (evtlist-filter-type :evt-r evts))
              ([r] (id-relation r))
              (w (evtlist-filter-type :evt-w evts))
              ([w] (id-relation w))
              (m (append w r))
              ([m] (id-relation m))
              (id (id-relation evts))
              (invrf (inverse rf))
              (fr (difference (compose invrf co) id))
              (ca (union fr (relation-fix co)))
              (haz-ob (compose* (list [r] (intersect po loc) [r] (intersect ca ext) [w])))
              (obs (union (compose* (list [w] (intersect rf ext) [r]))
                          (compose* (list [m] (intersect ca ext) [w]))))
              (ob (transitive-closure (union haz-ob obs)))
              (external (has-loop (haz1baserel-abstraction
                                   r w (intersect po loc)
                                   (intersect ca ext)
                                   (intersect rf ext))))
              (res (make-result :judgement :allowed
                                :flags (and external '("external"))
                                :env (env-updates
                                      "ob" (v_rel ob)
                                      "obs" (v_rel obs)
                                      "haz-ob" (v_rel haz-ob)
                                      "ca" (v_rel ca)
                                      "fr" (v_rel fr)
                                      "invrf" (v_rel invrf)
                                      "co" (v_rel co)))))
           (list res)))
  :hints(("Goal" :in-theory (enable haz1baserel-abstraction
                                    haz1fragment-spec))))

(defthmd haz2fragment-spec-in-terms-of-abstraction
  (equal (haz2fragment-spec co ex)
         (b* ((evts (execgraph->evts ex))
              (rf (rf-relation evts))
              (ext (ext-relation evts))
              (loc (loc-relation evts))
              (po (po-relation evts))
              (r (evtlist-filter-type :evt-r evts))
              ([r] (id-relation r))
              (w (evtlist-filter-type :evt-w evts))
              ([w] (id-relation w))
              (m (append w r))
              ([m] (id-relation m))
              (id (id-relation evts))
              (invrf (inverse rf))
              (fr (difference (compose invrf co) id))
              (ca (union fr (relation-fix co)))
              (haz-ob (compose* (list [w] (intersect rf ext) [r] (intersect po loc) [r])))
              (obs (union (compose* (list [w] (intersect rf ext) [r]))
                          (compose* (list [m] (intersect ca ext) [w]))))
              (ob (transitive-closure (union haz-ob obs)))
              (external (has-loop (haz2baserel-abstraction
                                   r w (intersect po loc)
                                   (intersect ca ext)
                                   (intersect rf ext))))
              (res (make-result :judgement :allowed
                                :flags (and external '("external"))
                                :env (env-updates
                                      "ob" (v_rel ob)
                                      "obs" (v_rel obs)
                                      "haz-ob" (v_rel haz-ob)
                                      "ca" (v_rel ca)
                                      "fr" (v_rel fr)
                                      "invrf" (v_rel invrf)
                                      "co" (v_rel co)))))
           (list res)))
  :hints(("Goal" :in-theory (enable haz2baserel-abstraction
                                    haz2fragment-spec))))


(encapsulate
  nil
  (local (defthm evttype-kind-when-member-filter
           (implies (member-equal (evt-fix e) (evtlist-filter-type type x))
                    (equal (evttype-kind (evt->data e))
                           (evtkind-fix type)))
           :hints(("Goal" :in-theory (enable evtlist-filter-type)))))
            

  (defthm evt-r/w-not-intersectp
    (implies (not (equal (evtkind-fix typ1)
                         (evtkind-fix typ2)))
             (not (intersectp-equal (evtlist-filter-type typ1 x)
                                    (evtlist-filter-type typ2 y))))
    :hints(("Goal" :in-theory (enable evtlist-filter-type intersectp-equal)))))


(defthm haz2fragment-spec-external-when-haz1fragment-spec-external
  (implies (member-equal "external"
                         (result->flags
                          (car (haz1fragment-spec co ex))))
           (member-equal "external"
                         (result->flags
                          (car (haz2fragment-spec co ex)))))
  :hints(("Goal" :in-theory (enable haz1fragment-spec-in-terms-of-abstraction
                                    haz2fragment-spec-in-terms-of-abstraction))))

(defthm haz1fragment-spec-external-when-haz2fragment-spec-external
  (implies (member-equal "external"
                         (result->flags
                          (car (haz2fragment-spec co ex))))
           (member-equal "external"
                         (result->flags
                          (car (haz1fragment-spec co ex)))))
  :hints(("Goal" :in-theory (enable haz2fragment-spec-in-terms-of-abstraction
                                    haz1fragment-spec-in-terms-of-abstraction))))




(defthm haz2fragment-external-iff-haz2fragment-external
  (b* (((mv err1 results1)
        (eval-inslist (haz1fragment)
                      :result (make-result :judgement :allowed
                                           :env (env-update "co" (v_rel co) nil))))
       ((mv err2 results2)
        (eval-inslist (haz2fragment)
                      :result (make-result :judgement :allowed
                                           :env (env-update "co" (v_rel co) nil)))))
    (implies (and (not err1)
                  (not err2))
             (iff (member-equal "external" (result->flags (car results2)))
                  (member-equal "external" (result->flags (car results1))))))
  :hints(("Goal" :in-theory (disable (haz1fragment)
                                     (haz2fragment)))))


;; haz1: ob =
;; ( [R]; (po & loc); [R]; (ca & ext); [W]
;;   | [W]; rf & ext; [R]
;;   | [M]; ca & ext; [W] )+

;; haz2: ob =
;; ( [W]; (rf & ext); [R]; (po & loc); [R]
;;   | [W]; rf & ext; [R]
;;   | [M]; ca & ext; [W] ) +


;; In any loop in haz1 ob,
;; [R]; (po & loc); [R]; (ca & ext); [W]
;; must be preceded by [W]; rf & ext; [R] (since it's the only way a step can end in a read).
;; Thus we have
;; [W]; rf & ext; [R]; (po & loc); [R]; (ca & ext); [W]
;; which is also a loop in haz2 ob
;; namely [W]; (rf & ext); [R]; (po & loc); [R] followed by [M]; ca & ext; [W]).

;; In any loop in haz2 ob,
;; [W]; (rf & ext); [R]; (po & loc); [R]
;; must be followed by [R]; ca & ext; [W] (since it's the only way a step can begin with a read).
;; Thus we have
;; [W]; (rf & ext); [R]; (po & loc); [R]; ca & ext; [W]
;; which is also attainable in haz1 ob.

;; 

