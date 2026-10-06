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

(include-book "arbvals")
(include-book "std/util/defret-mutual-generate" :dir :system)
(local (std::add-default-post-define-hook :fix))

;; This book defines a translation from an oracle to an arbmap, such that some
;; call of the original interpreter on the given oracle produces the same
;; result as the call of the corresponding arbvals (-*a) interpreter on the
;; same non-oracle arguments and the arbmap produced by this translation.

;; The translation functions take the form of a mutual recursion analogous to
;; the interpreter, where each interpreter function returns the regular
;; interpreter arguments and additionally the arbmap that produces any values
;; read from the oracle within the call. (We considered having them only return
;; the arbmap and use calls of the original interpreter to get any return
;; values needed for further calls, but this seemed less straightforward to
;; implement.) To do this we'll also pass the position-tracking arguments,
;; arbaddr and (usually) arboffset, that go into the -*a functions.

;; We'll suffix the functions of the translator with -*ota ("orac to arbmap").

;; We'll derive the interpreter from the -*a variant instead of the original
;; interpreter because we'd need to replicate all of its arbaddr/arboffset
;; bindings otherwise. (Threading the oracle through everything is relatively
;; easy.)

;; Interpreter macro replacements.

(defmacro evo_normal-*ota (arg)
  `(mv (ev_normal ,arg) orac arbmap))

(define pass-error-*ota ((val eval_result-p) orac arbmap)
  :guard (eval_result-case val :ev_error)
  :inline t
  :enabled t
  (mv (ev_error-fix val) orac arbmap))

(defmacro evo_error-*ota (&rest args)
  `(pass-error-*ota (ev_error . ,args) orac arbmap))

(defmacro evo_throwing-*ota (&rest args)
  `(mv (ev_throwing . ,args) orac arbmap))

(defmacro evo-return-*ota (arg)
  `(mv ,arg orac arbmap))

(defmacro evtailcall-*ota (call)
  `(b* (((mv res orac arbmap1) ,call)
        (arbmap (append arbmap1 arbmap)))
     (mv res orac arbmap)))



(defun ota-call-to-*a-fn (ota-call)
  (b* (((cons ota-fn args) ota-call)
       (a-fn (intern-in-package-of-symbol
              (if (str::strsuffixp "-FN" (symbol-name ota-fn))
                  (concatenate 'string
                               (subseq (symbol-name ota-fn)
                                       0 (- (length (symbol-name ota-fn)) 7))
                               "*A-FN")
                (concatenate 'string
                             (subseq (symbol-name ota-fn)
                                     0 (- (length (symbol-name ota-fn)) 4))
                             "*A"))
              ota-fn))
       (args-without (append (remove-equal 'orac args) '(arbmap))))
    (cons a-fn args-without)))

(defmacro ota-call-to-*a (ota-call)
  (ota-call-to-*a-fn ota-call))

(defun arboffset-for-form-*ota-fn (form)
  (declare (xargs :mode :program))
  (let* ((form (cons (arboffset-normalize-fnsym (car form)) (cdr form)))
         (form (ota-call-to-*a-fn form))
         (name (arboffset-normalize-fnsym (car form)))
         (offset (add-arboffsets-for-form-args
                  (cdr form)
                  (cdr (assoc name *arboffset-for-form-table*)))))
    (or offset '(make-arbaddr-offset))))

(defmacro arboffset-for-form-*ota (form)
  (arboffset-for-form-*ota-fn form))

(defmacro add-arboffset-for-form-*ota (arboffset form)
  (let* ((form (ota-call-to-*a-fn form))
        (form-arboffset (add-arboffsets-for-form-args
                         (cdr form)
                         (cdr (assoc (car form) *arboffset-for-form-table*)))))
    (if form-arboffset
        `(arbaddr-offset-sum ,form-arboffset ,arboffset)
      arboffset)))


(acl2::def-b*-binder evbind-*ota
  :body
  `(b* (((mv ,(car acl2::args) orac arbmap1) . ,acl2::forms)
        (?arboffset (add-arboffset-for-form-*ota arboffset . ,acl2::forms))
        (arbmap (append arbmap1 arbmap)))
     ,acl2::rest-expr))

(acl2::def-b*-binder evbind-nonrec-*ota
  :body
  `(b* (((mv ,(car acl2::args) orac arbmap1) . ,acl2::forms)
        (arbmap (append arbmap1 arbmap)))
     ,acl2::rest-expr))

(acl2::def-b*-binder evo-*ota
  :body
  `(b* ((evresult ,(car acl2::forms)))
     (eval_result-case evresult
       :ev_normal (b* ,(and (not (eq (car acl2::args) '&))
                           `((,(car acl2::args) evresult.res)))
                    ,acl2::rest-expr)
       :otherwise (mv (eval_result-nonnormal-fix evresult) orac arbmap))))

(acl2::def-b*-binder evoo-*ota
  :body
  `(b* (((evbind-*ota evoo-*a-tmp) . ,acl2::forms)
        ((evo-*ota ,(car acl2::args)) evoo-*a-tmp))
     (acl2::check-vars-not-free
      (evoo-*a-tmp)
      ,acl2::rest-expr)))

(acl2::def-b*-binder evs-*ota
  :body
  `(b* (((evoo-*ota cflow) ,(car acl2::forms)))
     (control_flow_state-case cflow
       :returning (evo_normal-*ota
                   (mbe :logic (returning cflow.vals cflow.env)
                        :exec cflow))
       :continuing (b* ,(and (not (eq (car acl2::args) '&))
                             `((,(car acl2::args) cflow.env)))
                     ,acl2::rest-expr))))

;; These "-special" forms are used only in custom definitions below for cases
;; where the current arboffset shouldn't be rebound to its sum with the form's
;; arguments.
(acl2::def-b*-binder evoo-*ota-special
  :body
  ;; don't rebind arboffset, but still include arbmap entries
  `(b* (((evbind-nonrec-*ota evoo-*ota-tmp) . ,acl2::forms)
        ((evo-*ota ,(car acl2::args)) evoo-*ota-tmp))
     (acl2::check-vars-not-free
      (evoo-*ota-tmp)
      ,acl2::rest-expr)))

(acl2::def-b*-binder evs-*ota-special
  :body
  `(b* (((evoo-*ota-special cflow) ,(car acl2::forms)))
     (control_flow_state-case cflow
       :returning (evo_normal-*ota
                   (mbe :logic (returning cflow.vals cflow.env)
                        :exec cflow))
       :continuing (b* ,(and (not (eq (car acl2::args) '&))
                             `((,(car acl2::args) cflow.env)))
                     ,acl2::rest-expr))))

(acl2::def-b*-binder evob-*ota
  :body
  `(b* ((evresult ,(car acl2::forms)))
     (eval_result-case evresult
       :ev_normal (b* ,(and (not (eq (car acl2::args) '&))
                            `((,(car acl2::args) evresult.res)))
                    ,acl2::rest-expr)
       :ev_throwing (evo-return-*ota
                     (init-backtrace
                      (ev_throwing-fix evresult)
                      (local-env->storage (env->local env))
                      pos))
       :otherwise (pass-error-*ota
                   (init-backtrace
                    (ev_error-fix evresult)
                    (local-env->storage (env->local env))
                    pos)
                   orac arbmap))))

(defmacro evbody-*ota (body)
  `(let ((arbmap nil)) ,body))





;; Arbitrary: record the result of the orac lookup in arbmap. Build address by consing an
;; ARB address component (index from the current arbaddr arboffset) onto the
;; incoming arbaddr.
(defconst *e_arbitrary-*ota-def*
  '(b* (((evoo-*ota ty) (resolve-ty-*ota env desc.type))
        ((unless (ty-satisfiable ty))
         ;; this only happens if the type is unsatisfiable
         (evo_error-*ota "DE_AET: " desc (list pos)))
        ((mv val orac) (ty-oracle-val ty orac))
        (arbmap (cons (cons (cons (arbaddr-component-arb (arbaddr-offset->arboffset arboffset))
                                  (arbaddr-fix arbaddr))
                            val)
                      arbmap)))
     (evo_normal-*ota (expr_result val env))))

(local (defconst *asl-*ota-case-replacements*
         `((:e_arbitrary . ,*e_arbitrary-*ota-def*))))



(local (defconst *asl-*ota-xdoc*
         '(:parents (asl-interpreter-functions)
           :short "Modified version of @(see asl-interpreter-mutual-recursion) that produces an
@(see arbmap) object that records values read from the oracle. This value can
then be used instead of the oracle in @(see
asl-interpreter-mutual-recursion-*a)."
           :long "
<p>This is an automatically generated derived version of the ASL interpreter,
@(see asl-interpreter-mutual-recursion). Each function in the original mutual
recursion has an analogous function in this version, suffixed with
@('-*1') (the \"arbvals version\" of the function). </p>")))


(defun pair-both-suffixed (fns key-suffix val-suffix)
  (if (atom fns)
      nil
    (cons (Cons (intern-in-package-of-symbol
                 (concatenate 'string (symbol-name (car fns)) (symbol-name key-suffix))
                 (car fns))
                (intern-in-package-of-symbol
                 (concatenate 'string (symbol-name (car fns)) (symbol-name val-suffix))
                 (car fns)))
          (pair-both-suffixed (cdr fns) key-suffix val-suffix))))
                

(defconst *eval-*ota-substitution*
  (list* (cons 'evs-*a-special 'evs-*ota-special)
         (cons 'add-arboffset-for-form 'add-arboffset-for-form-*ota)
         (pair-both-suffixed (append *asl-interp-fns*
                                     '(evo_normal pass-error evo_error evo_throwing evo-return
                                                  evbind evbind-nonrec evoo evo evob evs evtailcall
                                                  asl-interpreter-mutual-recursion))
                             '-*a '-*ota)))


(local
 (defun add-orac-and-arbmap-to-returns (x)
   (if (atom x)
       x
     (case-match x
       ((':returns x . rest)
        `(:returns (mv ,x new-orac (arbmap arbmap-p)) . ,rest))
       (& (cons (add-orac-and-arbmap-to-returns (car x))
                (add-orac-and-arbmap-to-returns (cdr x))))))))


(defthm arbmap-p-of-append
  (implies (and (arbmap-p x) (arbmap-p y))
           (arbmap-p (append x y))))

(encapsulate nil
  (local (in-theory (disable (:t eval_result-kind)
                             (:t append)
                             (:t pass-error)
                             (:t val-kind)
                             (:t acl2::true-listp-append)
                             (:t eval_expr)
                             (:t expr_result->env)
                             (:t expr_result->val)
                             (:t ev_error)
                             (:t pass-error-*a)
                             (:t ev_normal)
                             default-car
                             default-cdr
                             (tau-system))))

  ;; ---------------------------------------------------------------------------
  ;; Definition of the Arbvals ASL Interpreter (suffixed with *a)
  (with-output
    :off (event)
    (make-event
     (b* ((form *asl-interpreter-mutual-recursion-*a-form*)
          ;; Strip out xdoc
          (form (strip-xdoc form))
          ;; Add xdoc topic for mutual recursion
          (form (add-mutrec-xdoc *asl-*ota-xdoc* form))
          ;; Add xdoc topic for each function
          (form (add-define-xdoc
                 "Version of @(see <NAME>) that produces arbmap from oracle; see @(see asl-interpreter-mutual-recursion-*ota) for overview."
                 form))
          ;; Substitute function names with their -*ota suffixed forms.
          (form (sublis *eval-*ota-substitution* form))
          ;; Add orac and arbmap into returns
          (form (add-orac-and-arbmap-to-returns form))
          ;; Add orac back into formals
          (form (add-define-formals '((orac 'orac)) form))
          ;; Remove arbmap from formals
          (form (remove-define-formals '(((arbmap arbmap-p) 'arbmap)) form))
          ;; Replace function bodies and cases with customized versions.
          (form (replace-case-bodies form *asl-*ota-case-replacements*))
          ;; Bind arbmap to nil at the beginning of each function.
          (form (wrap-define-bodies 'evbody-*ota form))

          ;; ;; Disable the functions, prove the non-trace return values equal to the originals, and verify guards.
          ;; (form (insert-after-///
          ;;        (list
          ;;         '(make-event
          ;;           `(in-theory (disable . ,(fgetprop 'eval_expr-*t-fn 'acl2::recursivep nil (w state)))))
          ;;         (equals-original-thm '*t (w state))
          ;;         ;; '(verify-guards eval_expr-*t-fn)
          ;;         )
          ;;        form))
          )
       `(progn (defconst *asl-interpreter-mutual-recursion-*ota-form* ',form)
               ,form)))))


(defmacro ota-call-to-original (ota-call)
  (b* (((cons ota-fn args) ota-call)
       (non-ota-fn (intern-in-package-of-symbol
                    (concatenate 'string
                                 (subseq (symbol-name ota-fn)
                                         0 (- (length (symbol-name ota-fn)) 7))
                                 "FN")
                    ota-fn))
       (args-without (set-difference-equal args
                                           '(arbaddr arbmap arboffset loop-iter loop-idx))))
    (cons non-ota-fn args-without)))

(encapsulate nil
  (local (in-theory (acl2::disable* equal-of-ev_normal
                                    equal-of-ev_throwing
                                    equal-of-ev_error
                                    equal-of-continuing
                                    equal-of-returning
                                    (tau-system))))
  (local (deflabel before-equals-original))
  (with-output
    :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
    (std::defret-mutual-generate <fn>-equals-original
      :rules ((t (:add-concl (B* (((mv orig-res orig-orac) (ota-call-to-original <call>)))
                               (and (equal res orig-res)
                                    (equal new-orac orig-orac)))))
              ((not (:fnname eval_limit-*ota))
               (:add-keyword
                :hints ((and stable-under-simplificationp
                             '(:expand ((:free (pos otherwise is_while clk) <call>)
                                        (:Free (pos otherwise is_while clk) (ota-call-to-original <call>))))))))
              ((:fnname eval_limit-*ota)
               (:add-keyword
                :hints ((and stable-under-simplificationp
                             '(:expand ((:free (pos otherwise is_while x) <call>)
                                        (:Free (pos otherwise is_while x) (ota-call-to-original <call>)))))))))
      :mutual-recursion asl-interpreter-mutual-recursion-*ota))

  (acl2::def-ruleset! asl-*ota-equals-original-rules
    (set-difference-equal (current-theory :here)
                          (current-theory 'before-equals-original))))
