;;****************************************************************************;;
;;                              ACL2 ASL Library                              ;;
;;****************************************************************************;;
;;
;; SPDX-FileCopyrightText: Copyright 2026 Arm Limited and/or its affiliates <open-source-office@arm.com>
;; SPDX-License-Identifier: BSD-3-Clause
;; 
;; ****************************************************************************;;



(in-package "ASL")

(include-book "interp-redefs")
(include-book "tools/match-tree" :dir :system)
(include-book "centaur/fgl/reference-ctrex" :dir :system)

;; ----------------------------------------------------------------------------
;; Prettier printing of terms and counterexmaples.

;; When we can't run counterexamples because we've abstracted too much, the
;; best we can do instead is (1) replay them with the reference counterexample
;; feature (see eval_subprogram-*t-print-reference-ctrex-below) or (2) debug
;; them using fgl::interp-st-bvar-db-debug-with-ctrex-values, which prints out
;; the objects from which the Boolean variables of the conjecture are
;; associated, along with their assignments in the counterexample. However,
;; these objects are messy and big and we don't want to see all of them. So we
;; hack together a set of rewrites that just let us print them in a more
;; readable fashion. The rewrites are in the make-event containing (assign
;; :ctrex-rewrites ...)  below.  They are just a list of two-element lists (
;; from to ) with pattern variables delimited by (:? var). These help with tracing
;; reference counterexamples as well.

;; To see the full rewritten counterexample:
#|
(match-tree-rewrite (fgl::interp-st-bvar-db-debug-with-ctrex-values fgl::interp-st)
                    (sort-rules (@ :ctrex-rewrites) nil))
|#
;; If this prints too much, then we should add more rules to the
;; ctrex-rewrites.

;; Careful not to add any rules that would mess up the basic format of the
;; list, which is ((var-idx val-bit obj) ...) where var-idx is a natural number
;; and val-bit is 0 or 1.

;; Additionally, FIND-CTREX-DUPLICATES looks through a rewritten list for
;; Boolean variables that are the same (after rewriting). This can help find
;; terms that have failed to converge to a normal form. Of course, looking at
;; the variables after rewriting won't help because they're equal, so you'll
;; need to debug by looking for a difference in the originals before rewriting.
;; ----------------------------------------------------------------------------


(define match-tree-apply-rules (obj rules)
  (b* (((when (atom rules)) (mv nil nil))
       ((unless (consp (car rules)))
        (match-tree-apply-rules obj (cdr rules)))
       ((cons pat subst) (car rules))
       ((mv match alist) (acl2::match-tree pat obj nil))
       ((unless match) (match-tree-apply-rules obj (cdr rules))))
    (mv t (acl2::subst-tree subst alist))))


(define match-tree-rewrite (obj rw-alist)
  :mode :program
  (cond ((atom obj) (mv nil obj))
        ((symbolp (car obj))
         (b* (((mv successp new)
               (match-tree-apply-rules
                obj (cdr (assoc (car obj) rw-alist))))
              ((unless successp)
               (B* (((mv successp cdr)
                     (match-tree-rewrite (cdr obj) rw-alist))
                    ((unless successp) (mv nil obj))
                    (new (cons (car obj) cdr))
                    ((mv & new) (match-tree-rewrite new rw-alist)))
                 (mv t new)))
              ((mv & new) (match-tree-rewrite new rw-alist)))
           (mv t new)))
        (t (b* (((mv car-succ car) (match-tree-rewrite (car obj) rw-alist))
                ((mv cdr-succ cdr) (match-tree-rewrite (cdr obj) rw-alist))
                ((unless (or car-succ cdr-succ))
                 (mv nil obj))
                (new (cons car cdr))
                ((mv & new) (match-tree-rewrite new rw-alist)))
             (mv t new))))
  ///
  (memoize 'match-tree-rewrite))

(define match-tree-rewrite$-memo ((clk natp) obj (rw-alist alistp) memo)
  :measure (acl2::two-nats-measure (nfix clk) (acl2-count obj))
  :hooks nil
  (b*  (((when (zp clk)) (mv nil obj memo))
        ((when (atom obj)) (mv nil obj memo))
        (look (hons-get obj memo))
        ((when look)
         (if (consp (cdr look))
             (mv t (cadr look) memo)
           (mv nil obj memo)))
        ((mv successp res memo)
         (b* (((when (symbolp (car obj)))
               (b* (((mv successp new)
                     (match-tree-apply-rules
                      obj (cdr (assoc (car obj) rw-alist))))
                    ((unless successp)
                     (B* (((mv successp cdr memo)
                           (match-tree-rewrite$-memo clk (cdr obj) rw-alist memo))
                          ((unless successp) (mv nil obj memo))
                          (new (cons (car obj) cdr))
                          ((mv & new memo)
                           (match-tree-rewrite$-memo (1- clk) new rw-alist memo)))
                       (mv t new memo)))
                    ((mv & new memo) (match-tree-rewrite$-memo (1- clk) new rw-alist memo)))
                 (mv t new memo)))
              ((mv car-succ car memo) (match-tree-rewrite$-memo clk (car obj) rw-alist memo))
              ((mv cdr-succ cdr memo) (match-tree-rewrite$-memo clk (cdr obj) rw-alist memo))
              ((unless (or car-succ cdr-succ))
               (mv nil obj memo))
              (new (cons car cdr))
              ((mv & new memo) (match-tree-rewrite$-memo (1- clk) new rw-alist memo)))
           (mv t new memo)))
        (memo (hons-acons obj (and successp (list res)) memo)))
    (mv successp res memo)))

(define match-tree-rewrite$ (obj (rw-alist alistp))
  :hooks nil
  (b* (((mv & res memo)
        (match-tree-rewrite$-memo 1000000 obj rw-alist nil)))
    (fast-alist-free memo)
    res))

(define sort-rules (pairs (acc alistp))
  :hooks nil
  (b* (((when (atom pairs))
        (fast-alist-free (fast-alist-clean acc)))
       ((unless (and (consp (car pairs))
                     (consp (cdar pairs))
                     (not (cddar pairs))))
        (er hard? 'sort-rules "bad pair: ~x0" (car pairs))
        (sort-rules (cdr pairs) acc))
       ((list pat subst) (car pairs))
       ((unless (and (consp pat)
                     (symbolp (car pat))))
        (er hard? 'sort-rules "bad pair (pattern): ~x0" (car pairs))
        (sort-rules (cdr pairs) acc))
       (acc (cons (cons (car pat)
                        (cons (cons pat subst)
                              (cdr (assoc (car pat) acc))))
                  acc)))
    (sort-rules (cdr pairs) acc)))

(make-event
 (b* (((er &)
       (assign :ctrex-rewrites
               '((  (:g-apply (:? fn) . (:? args))   ((:? fn) . (:? args)) )
                 (  (val-imap-satisfies-types (:? arg1) . (:? args))  (val-imap-satisfies-types) )
                 (  (:g-concrete . (:? val)) (quote (:? val)) )
                 (  (quote ((declared_types . (:? arg1)) . (:? args)))  (some-static-env) )
                 (  (env . (:? args)) (some-env) )
                 (  (static_env_global . (:? args)) (some-static-env) )
                 (  (car (func_result->vals$inline (ev_normal->res$inline (mv-nth 0 (eval_subprogram-*t-fn
                                                                                     (:? env) (:? name) (:? params) (:? args) . (:? rest))))))
                    (retval ((:? name) (:? params) (:? args))))
                 (  (func_result->env$inline (ev_normal->res$inline (mv-nth 0 (eval_subprogram-*t-fn
                                                                               (:? env) (:? name) (:? params) (:? args) . (:? rest)))))
                    (retenv ((:? name) (:? params) (:? args))))
                 (  (eval_result-kind$inline (mv-nth 0 (eval_subprogram-*t-fn
                                                        (:? env) (:? name) (:? params) (:? args) . (:? rest))))
                    (status ((:? name) (:? params) (:? args))))
                 (  (ev_throwing->throwdata$inline (mv-nth 0 (eval_subprogram-*t-fn
                                                              (:? env) (:? name) (:? params) (:? args) . (:? rest))))
                    (throwdata ((:? name) (:? params) (:? args))))
                 (  (env->global$inline (ev_throwing->env$inline (mv-nth 0 (eval_subprogram-*t-fn
                                                                            (:? env) (:? name) (:? params) (:? args) . (:? rest)))))
                    (throwenv ((:? name) (:? params) (:? args))))
                 (  (mv-nth 1 (eval_subprogram-*t-fn
                               (:? env) (:? name) (:? params) (:? args) . (:? rest)))
                    (new-orac ((:? name) (:? params) (:? args))))
                 (  (ty-fix-val (:? val) (:? type)) (:? val))
                 (  (VAL-IMAP-LOOKUP
                     (:? name)
                     (GLOBAL-ENV->STORAGE$INLINE (ENV->GLOBAL$INLINE (:G-VAR . ENV))))
                    (:? name))
                 (  (val-imap-lookup
                     (:? field)
                     (v_record->rec$inline (:? rec)))
                    ((:? rec) (:? field)))

                 (  (equal (:? x) (:? x))
                    (equal-self))
                 (  (:g-integer . (:? bits))
                    <symbolic-integer>)))))
   (value '(value-triple :ctrex-rewrites)))
 :check-expansion t)





;; ----------------------------------------------------------------------------
;; eval_subprogram-*t-print-reference-ctrex Trace calls of eval_subprogram-*t
;; when we are evaluating a reference counterexample.  This needs to be set up
;; with (reference-ctrex-mode t) and removed with (reference-ctrex-mode
;; nil). The latter assumes that we want to turn on regular eval_subprogram-*t
;; symbolic simulation tracing (enables the eval_subprogram-*t-print rule).
;; ----------------------------------------------------------------------------

(define force-val-fix ((x val-p))
  (val-fix x)
  ///
  (fgl::def-fgl-rewrite force-val-fix-fgl
    (equal (force-val-fix x)
           (b* ((kind (val-kind x))
                (conc (fgl::syntax-bind concretep (fgl::fgl-object-case kind :g-concrete))))
             (if conc
                 (CASE kind
                   (:V_INT (V_INT (V_INT->VAL X)))
                   (:V_BOOL (V_BOOL (V_BOOL->VAL X)))
                   (:V_REAL (V_REAL (V_REAL->VAL X)))
                   (:V_STRING (V_STRING (V_STRING->VAL X)))
                   (:V_BITVECTOR (V_BITVECTOR (V_BITVECTOR->LEN X)
                                              (V_BITVECTOR->VAL X)))
                   (:V_LABEL (V_LABEL (V_LABEL->VAL X)))
                   (:V_RECORD (V_RECORD (V_RECORD->REC X)))
                   (:V_ARRAY (V_ARRAY (V_ARRAY->ARR X))))
               (val-fix x)))))
  (fgl::remove-fgl-rewrite force-val-fix))



(fgl::def-fgl-rewrite eval_subprogram-*t-print-reference-ctrex
  (implies (and (fgl::bind-fn-annotation annot 'eval_subprogram-*t-fn)
                ;; make sure this call hasn't already been printed
                (not (printed-annotation-index annot))
                ;; debugging for cases where we're never printing anything
                ;; (fgl::bind-var dummy (fgl::syntax-interp (b* (((mv res &) (fgl::reference-ctrex-check-path-condition-satisfied fgl::interp-st)))
                ;;                                            (cw "pathcond satisfied: ~x0~%lits:~%"
                ;;                                                res (fgl::reference-ctrex-debug-path-condition fgl::interp-st))
                ;;                                            t)))
                (fgl::bind-var in-pathcond (fgl::reference-ctrex-pathcond-check))
                ;; make sure there is a previous call
                ;; (outermost call needs to be rewrittten by save-oracle-on-outermost-eval_subprogram-*t)
                (fgl::syntax-bind old-index (or (find-previous-annotation-index-*t 10 1 'interp-st) 0))
                (equal index (+ 1 old-index)))
           (equal (eval_subprogram-*t env fn params args)
                  (b* (((posn pos1) (fgl::bind-var pos1 (if (fgl::syntax-interp
                                                             (fgl::fgl-object-case pos
                                                               :g-concrete (posn-p pos.val)
                                                               :otherwise nil))
                                                            pos
                                                          (fgl::g-concrete (make-posn)))))
                       (pos-msg (msg "~s0:~x1:~x2" pos1.fname pos1.lnum (- pos1.cnum pos1.bol))))
                    (fgl::fgl-prog2
                     (fgl::handle-error
                      :intro-bvars-fail
                      (fgl::disallow-boolean-var-intro
                       (b* ((args* (list params args))
                            (args-msg (fgl::reference-ctrex-object-eval-msg args*))
                            (args-gen (fgl::syntax-interp
                                       (match-tree-rewrite$
                                        (list params args) (sort-rules (@ :ctrex-rewrites) nil)))))
                         (fgl::syntax-interp (cw! "~t0~x1> eval_subprogram-*t ~x2 ~@3 ~x4 (~@5)~%"
                                                  (1+ index) index fn args-msg args-gen
                                                  (fgl::g-concrete->val pos-msg))))))
                     (b* ((res (fgl::annotate `(:printed ,index)
                                              (eval_subprogram-*t env fn params args)))
                          ((mv evres & &) res))
                       (fgl::fgl-prog2
                        (fgl::handle-error
                         :intro-bvars-fail
                         (fgl::disallow-boolean-var-intro
                          (b* ((retval (eval_result-case evres
                                         :ev_normal (b* (((func_result fr) evres.res))
                                                      (and (consp fr.vals)
                                                           (list (force-val-fix (car fr.vals)))))
                                         :otherwise nil))
                               (retval-msg (fgl::reference-ctrex-object-eval-msg retval)))
                            (fgl::syntax-interp
                             (b* ((errmsg (fgl::interp-st->errmsg 'interp-st)))
                               (fmt-to-comment-window!
                                "~t0<~x1 ~s2eval_subprogram-*t ~x3 ~@4 ~x5 (~@6)~%"
                                (pairlis2
                                 acl2::*base-10-chars*
                                 (list (1+ index) index (if errmsg "**" "") fn
                                       (if errmsg
                                           (if (msgp errmsg)
                                               errmsg
                                             (msg "~x0" errmsg))
                                         retval-msg)
                                       (if errmsg evres retval)
                                       (fgl::g-concrete->val pos-msg)))
                                0 '(nil 5 8 nil) nil))))))
                        res)))))))

(fgl::remove-fgl-rewrite eval_subprogram-*t-print-reference-ctrex)

(defmacro reference-ctrex-mode (val)
  (if val
      '(progn (fgl::remove-fgl-rewrite eval_subprogram-*t-print)
              (fgl::add-fgl-rewrite eval_subprogram-*t-print-reference-ctrex))
    '(progn (fgl::add-fgl-rewrite eval_subprogram-*t-print)
            (fgl::remove-fgl-rewrite eval_subprogram-*t-print-reference-ctrex))))



(define find-previous-annotation-index* ((tries natp) (n natp) (fn acl2::pseudo-fnsym-p) fgl::interp-st)
  :hooks nil
  (if (zp tries)
      nil
    (b* ((annot (fgl::interp-st-nth-fn-annotation n fn nil fgl::interp-st))
         ((unless (fgl::fgl-object-case annot :g-concrete))
          (find-previous-annotation-index* (1- tries) (1+ n) fn fgl::interp-st))
         (idx (ec-call (printed-annotation-index (fgl::g-concrete->val annot))))
         ((when idx) idx))
      (find-previous-annotation-index* (1- tries) (1+ n) fn fgl::interp-st))))


(fgl::def-fgl-rewrite eval_stmt-*t-print-reference-ctrex
  (implies (and (fgl::bind-fn-annotation annot 'eval_stmt-*t-fn)
                ;; make sure this call hasn't already been printed
                (not (printed-annotation-index annot))
                (fgl::bind-var in-pathcond (fgl::reference-ctrex-pathcond-check))
                (not (stmt_desc-case (stmt->desc s) '(:s_seq :s_pass)))
                ;; make sure there is a previous call
                ;; (outermost call needs to be rewrittten by save-oracle-on-outermost-eval_subprogram-*t)
                (fgl::syntax-bind old-index (or (find-previous-annotation-index* 10 1 'eval_stmt-*t-fn 'interp-st) 0))
                (equal index (+ 1 old-index)))
           (equal (eval_stmt-*t env (fgl::concrete s))
                  (b* ((pos (stmt->pos_start s))
                       ((posn pos1) (fgl::bind-var pos1 (if (fgl::syntax-interp
                                                             (fgl::fgl-object-case pos
                                                               :g-concrete (posn-p pos.val)
                                                               :otherwise nil))
                                                            pos
                                                          (fgl::g-concrete (make-posn)))))
                       (pos-msg (msg "~s0:~x1:~x2" pos1.fname pos1.lnum (- pos1.cnum pos1.bol))))
                    (fgl::fgl-prog2
                     (fgl::handle-error
                      :intro-bvars-fail
                      (fgl::disallow-boolean-var-intro
                       (fgl::syntax-interp (cw! "~t0~x1> eval_stmt-*t ~@2~%"
                                                (1+ index) index (fgl::g-concrete->val pos-msg)))))
                     (b* ((res (fgl::annotate `(:printed ,index)
                                              (eval_stmt-*t env s))))
                       (fgl::fgl-prog2
                        (fgl::handle-error
                         :intro-bvars-fail
                         (fgl::disallow-boolean-var-intro
                          (fgl::syntax-interp
                           (b* ((errmsg (fgl::interp-st->errmsg 'interp-st)))
                             (cw!
                              "~t0<~x1 ~s2eval_stmt-*t ~@3 ~@4~%"
                              (1+ index) index (if errmsg "**" "")
                              (if errmsg
                                  (if (msgp errmsg)
                                      errmsg
                                    (msg "~x0" errmsg))
                                "")
                              (fgl::g-concrete->val pos-msg))))))
                        res)))))))


(defmacro @! (x)
  `(and (boundp-global ',x state)
        (f-get-global ',x state)))

(fgl::def-fgl-rewrite declare_local_identifier-print-reference-ctrex
  (implies (and (fgl::bind-fn-annotation annot 'declare_local_identifier)
                (not annot)
                (fgl::bind-var in-pathcond (fgl::reference-ctrex-pathcond-check))
                (member-equal name (fgl::syntax-bind traced-varnames (fgl::g-concrete (@! :traced-varnames)))))
           (equal (declare_local_identifier env name val)
                  (b* ((res (fgl::annotate '(:printed) (declare_local_identifier env name val))))
                    (fgl::fgl-progn
                     (fgl::handle-error
                      :intro-bvars-fail
                      (fgl::disallow-boolean-var-intro
                       (b* ((val-msg (fgl::reference-ctrex-object-eval-msg val)))
                         (fgl::syntax-interp
                          (bcw :evisc '(nil 7 12 nil)
                               "declare_local_identifier ~x0 = ~@1 (~x2)~%" name val-msg val)))))
                     res)))))

(define print-declared-identifiers (names vals traced-varnames)
  (declare (ignorable names vals traced-varnames))
  nil
  ///
  (fgl::def-fgl-rewrite print-declared-identifiers-exp
    (equal (print-declared-identifiers names vals traced-varnames)
           (cond ((atom names) nil)
                 ((member-equal (car names) traced-varnames)
                  (fgl::fgl-progn
                   (fgl::handle-error
                      :intro-bvars-fail
                      (fgl::disallow-boolean-var-intro
                       (b* ((name (car names))
                            (val (car vals))
                            (val-msg (fgl::reference-ctrex-object-eval-msg val)))
                         (fgl::syntax-interp
                          (bcw :evisc '(nil 7 12 nil)
                               "declare_local_identifiers ~x0 = ~@1 (~x2)~%" name val-msg val)))))
                   (print-declared-identifiers (cdr names) (cdr vals) traced-varnames)))
                 (t (print-declared-identifiers (cdr names) (cdr vals) traced-varnames))))))
                    


(fgl::def-fgl-rewrite declare_local_identifiers-print-reference-ctrex
  (implies (and (fgl::bind-fn-annotation annot 'declare_local_identifiers)
                (not annot)
                (fgl::bind-var in-pathcond (fgl::reference-ctrex-pathcond-check))
                (intersectp-equal names (fgl::syntax-bind traced-varnames (fgl::g-concrete (@! :traced-varnames)))))
           (equal (declare_local_identifiers env names vals)
                  (b* ((res (fgl::annotate '(:printed) (declare_local_identifiers env names vals))))
                    (fgl::fgl-progn
                     (print-declared-identifiers names vals traced-varnames)
                     res)))))



(fgl::def-fgl-rewrite env-assign-print-reference-ctrex
  (implies (and (fgl::bind-fn-annotation annot 'env-assign)
                (not annot)
                (fgl::bind-var in-pathcond (fgl::reference-ctrex-pathcond-check))
                (member-equal name (fgl::syntax-bind traced-varnames (fgl::g-concrete (@! :traced-varnames)))))
           (equal (env-assign name val env)
                  (b* ((res (fgl::annotate '(:printed) (env-assign name val env))))
                    (fgl::fgl-progn
                     (fgl::handle-error
                      :intro-bvars-fail
                      (fgl::disallow-boolean-var-intro
                       (b* ((val-msg (fgl::reference-ctrex-object-eval-msg val)))
                         (fgl::syntax-interp
                          (bcw :evisc '(nil 7 12 nil)
                               "env-assign ~x0 = ~@1 (~x2)~%" name val-msg val)))))
                     res)))))





