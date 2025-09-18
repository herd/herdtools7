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

(include-book "interp")
(include-book "std/util/defconsts" :dir :system)
(include-book "clause-processors/just-expand" :dir :System)
(local (include-book "interp-theory"))

(local (in-theory (disable integer-listp))) ;; doubles the time for some deftypes if not disabled
(local (std::add-default-post-define-hook :fix))

;; Define a new version of the interpreter that additionally collects
;; debug/trace information according to a trace specification.


(defxdoc asl-tracing
  :parents (asl)
  :short "Returning trace information from the ASL interpreter."
  :long "<p>The main ASL interpreter (see @(see asl-interpreter-mutual-recursion)) only
produces the final result of the evaluation; in some cases we want to see (and
reason about) what happened internally. To allow this, we derive from that
original interpreter a tracing version @(see
asl-interpreter-mutual-recursion-*t) that additionally produces customizable
trace data, tracking certain function calls or statement executions.</p>

<p>The information to collect is specified by a @(see tracespec) data
structure. The resulting trace information is an @(see asl-tracelist)
object.</p>

<h3>Features</h3>
<ul>

<li>Trace specifiers can be given names (user-provided symbols); each trace is
labelled with the name of the trace specifier it resulted
from. Traces (including subtraces) can be searched by name using @(see
asl-tracelist-find-by-name).</li>

<li>Trace specifiers can match on code location; e.g., if we want to track
calls of \"foo\" only from a particular line of code, the tracespec can
specify that.</li>

<li>Trace specifiers can cause other trace specifiers to be used inside their
scope.  For example, if we want to track calls of \"foo\" but only those that
occur within a call of \"bar\", this can be arranged by having the trace
specifier for \"bar\" include a trace specifier for \"foo\" in its
@(':interior-tracespec') field.</li>

<li>Trace specifiers can cause the interpreter to abort, either before or after
the matching call or statement is executed, by setting the @(':abort') field of
the tracespec.</li>
</ul>")

(std::defenum control_status-p (:returning :continuing)
  :parents (asl-trace))
(defthm control_status-p-of-control_flow_state-kind
  (control_status-p (control_flow_state-kind x))
  :hints(("Goal" :in-theory (enable control_flow_state-kind))))

(deftypes asl-trace
  (deftagsum asl-trace
    :parents (asl-tracing)
    :short "An element of an ASL trace indicating some event that occurred in the program."
    (:calltrace
     ((name
       symbolp
       "Name of the tracespec that produced this trace")
      (fn identifier-p "Function that was called")
      (params vallist-p "Input parameters")
      (args vallist-p "Input arguments")
      (subtraces
       asl-tracelist
       "Traces from within this call, in sequential order.")
      (result eval_result-p
              :reqfix (eval_result-case result
                        :ev_normal (ev_normal (vallist-fix result.res))
                        :otherwise result)
              "Return values, if normal result, or error data")
      (pos posn "File position of the call"))
     :require (eval_result-case result
                :ev_normal (vallist-p result.res)
                :otherwise t)
     :short "A trace of an ASL subprogram call, including inputs, result, and sub-calls.")
    (:stmttrace
     ((name
       symbolp
       "Name of the tracespec that produced this trace")
      (stmt stmt-p "Statement that was traced")
      (initial-vars val-imap-p "Values of tracked variables just before executing this statement")
      (subtraces asl-tracelist
                 "Traces from within this statement, in sequential order.")
      (result eval_result-p
              :reqfix (eval_result-case result
                        :ev_normal (ev_normal (control_status-fix result.res))
                        :otherwise result)
              "The statement's continuing/returing status, if normal, or error data")
      (final-vars val-imap-p
                  "Values of tracked variables after executing this statement"))
     :require (eval_result-case result
                :ev_normal (control_status-p result.res)
                :otherwise t)
     :short "A trace of some ASL statement, and some variable values as of the beginning and
end of its execution")
    :base-case-override :calltrace
    :short-names t
    :measure (acl2::two-nats-measure (acl2-count x) 1)
    :layout :list)
  (deflist asl-tracelist :elt-type asl-trace :true-listp t :elementp-of-nil nil
    :parents (asl-tracing)
    :short "A list of @(see asl-trace) elements."
    :measure (acl2::two-nats-measure (acl2-count x) 0)))

(local
 (defthm true-listp-when-asl-tracelist-p
   (implies (asl-tracelist-p x)
            (true-listp x))
   :rule-classes ((:rewrite :backchain-limit-lst 1))))

(defthm asl-tracelist-p-of-append
  (implies (and (asl-tracelist-p x)
                (asl-tracelist-p y))
           (asl-tracelist-p (append x y))))

(fty::defoption maybe-string stringp)

(std::defenum trace-abort-p (:before :after nil)
  :parents (stmt-tracespec call-tracespec))


(fty::deftypes tracespec
  (defprod tracespec
    :parents (asl-tracing)
    :short "Specification for what calls and statements will be traced."
    :long "<p>Transient and permanent here indicate whether they persist across subroutine
calls.  The elements of each list are either (respectively) @(see
stmt-tracespec) or @(see call-tracespec) objects.</p>"
    ((stmt-specs-permanent stmt-tracespeclist-p)
     (call-specs-permanent call-tracespeclist-p)
     (stmt-specs-transient stmt-tracespeclist-p)
     (call-specs-transient call-tracespeclist-p))
    :measure (acl2::two-nats-measure (acl2-count x) 1)
    :layout :list)

  (fty::defoption maybe-tracespec tracespec
    :parents (tracespec)
    :measure (acl2::two-nats-measure (acl2-count x) 2))
     
  (defprod stmt-tracespec
    :parents (tracespec)
    :short "Specification for a condition under which a statement execution should be
traced, and the variable values that should be collected when tracing. This
condition is a conjunction of the requirements given by the settings of this
object's fields."
    ((name
      symbolp
      "User-provided name for this tracespec, recorded in the trace data")
     (stmttype
      symbolp
      "If NIL, no requirement; otherwise should be one of the possible results of
@('stmt_desc->kind') (one of the keywords assosiated with @(see stmt_desc))."
      :rule-classes :type-prescription)
     (fname
      maybe-string
      "If NIL, no requirement; otherwise, the code position of the statement must be
in the given file."
      :rule-classes :type-prescription)
     (lnum
      acl2::maybe-natp
      "If NIL, no requirement; otherwise, the code position of the statement must have
the given line number.")
     (cnum
      acl2::maybe-natp
      "If NIL, no requirement; otherwise, the code position of the statement must have
the given column number (note: not character number as in a @(see posn).")
     (initial-vars
      identifierlist-p
      "If tracing, collect the given variable values at the beginning of execution")
     (final-vars
      identifierlist-p
      "If tracing, collect the given variable values at the end of execution")
     (interior-tracespec
      maybe-tracespec-p
      "New tracespec used while inside this statement. This is combined with the
current tracespec using @(see combine-tracespecs) with first argument @('nil');
that is, if this tracespec is not provided, then the current set of transient
tracespecs remain in force, but if this tracespec is provided, then its
transient tracespecs replace the ones currently in force and its permanent
tracespecs are added to the current permanent tracespecs for the duration of
this statement's execution.")
     (abort
      trace-abort-p
      "If set, then if a matching statement is encountered, execution ends producing
an @('ev_error') result with special descriptor \"Trace abort\" either before
executing that statement (if set to @(':before')) or after (if set to
@(':after')). An error encountered during execution of the statement takes
precedence over an @(':after') abort."))
    :measure (acl2::two-nats-measure (acl2-count x) 3)
    :layout :list)

  (fty::deflist stmt-tracespeclist :elt-type stmt-tracespec
    :parents (tracespec stmt-tracespec)
    :true-listp t :elementp-of-nil nil
    :measure (acl2::two-nats-measure (acl2-count x) 0))

  (defprod call-tracespec
    :parents (tracespec)
    :short "Entry describing the conditions under which a function call should be traced
and what information should be collected in its trace."
    ((name
      symbolp
      "User-provided name for this tracespec, recorded in the trace data")
     (fn
      maybe-identifier-p
      "If NIL, no requirement; otherwise, the function name to trace."
      :rule-classes :type-prescription)
     (fname
      maybe-string
      "If NIL, no requirement; otherwise, the code position of the statement must be
in the given file."
      :rule-classes :type-prescription)
     (lnum
      acl2::maybe-natp
      "If NIL, no requirement; otherwise, the code position of the statement must have
the given line number."
      :rule-classes :type-prescription)
     (cnum
      acl2::maybe-natp
      "If NIL, no requirement; otherwise, the code position of the statement must have
the given column number (note: not character number as in a @(see posn)."
      :rule-classes :type-prescription)
     (paramsp booleanp "If t, trace parameters")
     (argsp booleanp "If t, trace arguments")
     (resultp booleanp "If t, trace result")
     (interior-tracespec
      maybe-tracespec-p
      "New tracespec used while inside this subroutine call. This is combined with the
current tracespec using @(see combine-tracespecs) with first argument @('t');
that is, the current set of transient tracespecs in force are not used for the
subroutine call (as always, whether a tracespec has matched on that subroutine
call or not), but the ones provided here are (if any), and the permanent
tracespecs provided here are added to the ones currently in force for the
duration of this subroutine call.")
     (abort
      trace-abort-p
      "If set, then if a matching subroutine call is encountered, execution ends
producing an @('ev_error') result with special descriptor \"Trace abort\"
either before executing that statement (if set to @(':before')) or after (if
set to @(':after')). An error encountered during execution of the call takes
precedence over an @(':after') abort."))
    :measure (acl2::two-nats-measure (acl2-count x) 3)
    :layout :list)

  (fty::deflist call-tracespeclist :elt-type call-tracespec
    :parents (tracespec call-tracespec)
    :true-listp t :elementp-of-nil nil
    :measure (acl2::two-nats-measure (acl2-count x) 0)))

(defthm stmt-tracespeclist-of-append
  (implies (and (stmt-tracespeclist-p x)
                (stmt-tracespeclist-p y))
           (stmt-tracespeclist-p (append x y))))

(defthm call-tracespeclist-of-append
  (implies (and (call-tracespeclist-p x)
                (call-tracespeclist-p y))
           (call-tracespeclist-p (append x y))))

(define check-stmt-tracespec ((stmt stmt-p)
                              (x stmt-tracespec-p))
  :returns (spec (iff (stmt-tracespec-p spec) spec))
  (b* (((stmt stmt))
       ((stmt-tracespec x)))
    (and (or (not x.stmttype)
             (eq x.stmttype (stmt_desc-kind stmt.desc)))
         (b* (((posn pos) stmt.pos_start))
           (and (or (not x.fname) (equal pos.fname x.fname))
                (or (not x.lnum) (eql pos.lnum x.lnum))
                (or (not x.cnum) (eql (- pos.cnum pos.bol) x.cnum))))
         (stmt-tracespec-fix x))))

(define stmt-tracespeclist-find ((stmt stmt-p)
                                 (x stmt-tracespeclist-p))
  :returns (spec (iff (stmt-tracespec-p spec) spec))
  (if (atom x)
      nil
    (or (check-stmt-tracespec stmt (car x))
        (stmt-tracespeclist-find stmt (cdr x)))))


(define find-stmt-tracespec ((stmt stmt-p)
                             (x tracespec-p))
  :returns (spec (iff (stmt-tracespec-p spec) spec))
  (b* (((tracespec x)))
    (or (stmt-tracespeclist-find stmt x.stmt-specs-transient)
        (stmt-tracespeclist-find stmt x.stmt-specs-permanent))))

(define check-call-tracespec ((fn identifier-p)
                              (pos posn-p)
                              (x call-tracespec-p))
  :returns (spec (iff (call-tracespec-p spec) spec))
  (b* (((call-tracespec x)))
    (and (or (not x.fn)
             (equal (identifier-fix fn) x.fn))
         (b* (((posn pos)))
           (and (or (not x.fname) (equal pos.fname x.fname))
                (or (not x.lnum) (eql pos.lnum x.lnum))
                (or (not x.cnum) (eql pos.cnum x.cnum))))
         (call-tracespec-fix x))))


(define call-tracespeclist-find ((fn identifier-p)
                                 (pos posn-p)
                                 (x call-tracespeclist-p))
  :returns (spec (iff (call-tracespec-p spec) spec))
  (if (atom x)
      nil
    (or (check-call-tracespec fn pos (car x))
        (call-tracespeclist-find fn pos (cdr x)))))

(define find-call-tracespec ((fn identifier-p)
                             (pos posn-p)
                             (x tracespec-p))
  :returns (spec (iff (call-tracespec-p spec) spec))
  (b* (((tracespec x)))
    (or (call-tracespeclist-find fn pos x.call-specs-transient)
        (call-tracespeclist-find fn pos x.call-specs-permanent))))


(define combine-tracespecs ((callp booleanp)
                            (new-ts maybe-tracespec-p)
                            (x tracespec-p))
  :parents (tracespec)
  :short "Returns a new tracespec formed by combining an ambient tracespec @('x') with an
interior tracespec @('new-ts') from some matching call or statement tracespec."
  :Returns (new tracespec-p)
  (b* (((unless new-ts)
        (if callp
            (change-tracespec x :stmt-specs-transient nil
                              :call-specs-transient nil)
          (tracespec-fix x)))
       ((tracespec new-ts))
       ((tracespec x)))
    (make-tracespec
     :stmt-specs-transient new-ts.stmt-specs-transient
     :call-specs-transient new-ts.call-specs-transient
     :stmt-specs-permanent (append new-ts.stmt-specs-permanent
                                   x.stmt-specs-permanent)
     :call-specs-permanent (append new-ts.call-specs-permanent
                                   x.call-specs-permanent))))
    


(define env-find-vars ((vars identifierlist-p)
                       (env env-p))
  :returns (map val-imap-p)
  (if (atom vars)
      nil
    (b* ((first (env-find (car vars) env))
         (rest (env-find-vars (cdr vars) env)))
      (env_result-case first
        :lk_local (omap::update (identifier-fix (car vars)) first.val rest)
        :lk_global (omap::update (identifier-fix (car vars)) first.val rest)
        :otherwise rest))))



(local
 (mutual-recursion
  (defun find-form-by-car (car x)
    (declare (xargs :mode :program))
    (if (atom x)
        nil
      (if (equal (car x) car)
          x
        (find-form-by-car-list car x))))
  (defun find-form-by-car-list (car x)
    (if (atom x)
        nil
      (or (find-form-by-car car (car x))
          (find-form-by-car-list car (cdr x)))))))

(local (defun keep-define-forms-in-list (x)
         (if (atom x)
             nil
           (if (and (consp (car x))
                    (eq (caar x) 'define))
               (cons (car x) (keep-define-forms-in-list (cdr x)))
             (keep-define-forms-in-list (cdr x))))))

(local (defun strip-post-/// (x)
         (declare (xargs :mode :program))
         (if (atom x)
             x
           (if (eq (car x) '///)
               '(///)
             (cons (strip-post-/// (car x))
                   (strip-post-/// (cdr x)))))))

(local (defun strip-xdoc (x)
         (if (atom x)
             x
           (if (member-eq (car x) '(:short :long :parents))
               (strip-xdoc (cddr x))
             (cons (strip-xdoc (car x))
                   (strip-xdoc (cdr x)))))))



(defconsts *asl-interp-fns*
  (acl2::strip-cadrs
   (keep-define-forms-in-list
    (find-form-by-car
     'defines *asl-interpreter-mutual-recursion-command*))))


(defmacro evo_normal-*t (arg)
  `(mv (ev_normal ,arg) orac trace))

(define pass-error-*t (val &optional (orac 'orac) (trace 'trace))
  :inline t
  :enabled t
  (mv val orac trace))

(defmacro evo_error-*t (&rest args)
  `(pass-error-*t (ev_error . ,args) orac trace))

(defmacro evo_throwing-*t (&rest args)
  `(mv (ev_throwing . ,args) orac trace))

(defmacro evo-return-*t (arg)
  `(mv ,arg orac trace))

(defmacro evtailcall-*t (call)
  `(b* (((evbind-*t res) ,call))
     (evo-return-*t res)))

(acl2::def-b*-binder evbind-*t
  :body
  `(b* (((mv ,(car acl2::args) orac trace-tmp) . ,acl2::forms)
        (trace (append trace-tmp trace)))
     ,acl2::rest-expr))

(acl2::def-b*-binder evo-*t
  :body
  `(b* ((evresult ,(car acl2::forms)))
     (eval_result-case evresult
       :ev_normal (b* ,(and (not (eq (car acl2::args) '&))
                           `((,(car acl2::args) evresult.res)))
                    ,acl2::rest-expr)
       :otherwise (mv evresult orac trace))))

(acl2::def-b*-binder evoo-*t
  :body
  `(b* (((evbind-*t evoo-*t-tmp) . ,acl2::forms)
        ((evo-*t ,(car acl2::args)) evoo-*t-tmp))
     (acl2::check-vars-not-free
      (evoo-*t-tmp)
     ,acl2::rest-expr)))

(acl2::def-b*-binder evs-*t
  :body
    `(b* (((evoo-*t cflow) ,(car acl2::forms)))
     (control_flow_state-case cflow
              :returning (evo_normal-*t cflow)
              :continuing (b* ,(and (not (eq (car acl2::args) '&))
                                    `((,(car acl2::args) cflow.env)))
                            ,acl2::rest-expr))))

(acl2::def-b*-binder evob-*t
  :body
  `(b* ((evresult ,(car acl2::forms)))
     (eval_result-case evresult
       :ev_normal (b* ,(and (not (eq (car acl2::args) '&))
                           `((,(car acl2::args) evresult.res)))
                    ,acl2::rest-expr)
       
       :otherwise (pass-error-*t (init-backtrace evresult pos) orac))))

(defmacro evbody-*t (body)
  `(let ((trace nil))
     ,body))



(local
 (defun pair-suffixed (syms suffix)
   (if (atom syms)
       nil
     (cons (cons (car syms)
                 (intern-in-package-of-symbol
                  (concatenate 'string (symbol-name (car syms))
                               (symbol-name suffix))
                  (car syms)))
           (pair-suffixed (cdr syms) suffix)))))

(defconsts *eval-trace-substitution*
  (pair-suffixed (append *asl-interp-fns*
                         '(evo_normal pass-error evo_error evo_throwing evo-return
                                      evbind evoo evo evob evs evtailcall
                                      asl-interpreter-mutual-recursion))
                 '-*t))
  

(local
 (defconst *eval_subprogram-*t-def*
   '(define eval_subprogram-*t ((env env-p)
                                (name identifier-p)
                                (vparams vallist-p)
                                (vargs vallist-p)
                                &key
                                ((clk natp) 'clk)
                                (orac 'orac)
                                ((pos posn-p) 'pos)
                                ((tracespec tracespec-p) 'tracespec))
      :short "Tracing version of @(see eval_subprogram); see @(see
asl-interpreter-mutual-recursion-*t) for overview."
      :measure (nats-measure clk 1 0 1)
      :returns (mv (res func_eval_result-p) new-orac
                   (trace asl-tracelist-p))
      (b* ((ts-entry (find-call-tracespec name pos tracespec))
           ((when (and ts-entry (eq (call-tracespec->abort ts-entry) :before)))
            (b* (((call-tracespec ts-entry))
                 (trace (list (make-calltrace
                               :name ts-entry.name
                               :fn name
                               :params (and ts-entry.paramsp vparams)
                               :args (and ts-entry.argsp vargs)
                               :result (ev_error "Trace abort" nil nil)
                               :pos pos))))
              (pass-error-*t
               (ev_error "Trace abort" ts-entry (list (posn-fix pos))))))
           (tracespec (combine-tracespecs t
                                          (and ts-entry (call-tracespec->interior-tracespec ts-entry))
                                          tracespec))
           ((mv res orac trace) (eval_subprogram-*t1 env name vparams vargs))
           ((unless ts-entry)
            (mv res orac trace))
           ((call-tracespec ts-entry))
           (trace (list (make-calltrace
                         :name ts-entry.name
                         :fn name
                         :params (and ts-entry.paramsp vparams)
                         :args (and ts-entry.argsp vargs)
                         :subtraces (acl2::rev trace)
                         :result (if ts-entry.resultp
                                     (eval_result-case res
                                       :ev_normal (ev_normal (func_result->vals res.res))
                                       :otherwise res)
                                   (ev_error "Not tracing result" nil nil))
                         :pos pos)))
           ((when (and (not (eval_result-case res :ev_error))
                       (eq ts-entry.abort :after)))
            (pass-error-*t
             (ev_error "Trace abort" ts-entry (list (posn-fix pos))))))
        (mv res orac trace)))))


(local
 (defconst *eval_stmt-*t-def*
   '(define eval_stmt-*t ((env env-p)
                          (s stmt-p)
                          &key
                          ((clk natp) 'clk)
                          (orac 'orac)
                          ((tracespec tracespec-p) 'tracespec))
      :short "Tracing version of @(see eval_stmt); see @(see
asl-interpreter-mutual-recursion-*t) for overview."
      :measure (nats-measure clk 0 (stmt-count* s) 1)
      :returns (mv (res stmt_eval_result-p) new-orac
                   (trace asl-tracelist-p))
      (b* ((ts-entry (find-stmt-tracespec s tracespec))
           ((when (and ts-entry (eq (stmt-tracespec->abort ts-entry) :before)))
            (b* (((stmt-tracespec ts-entry))
                 (trace (list (make-stmttrace
                               :name ts-entry.name
                               :stmt s
                               :initial-vars (env-find-vars ts-entry.initial-vars env)
                               :result (ev_error "Trace abort" nil nil)))))
              (pass-error-*t
               (ev_error "Trace abort" ts-entry (list (stmt->pos_start s))))))
           (tracespec (combine-tracespecs nil
                                          (and ts-entry (stmt-tracespec->interior-tracespec ts-entry))
                                          tracespec))
           ((mv res orac trace) (eval_stmt-*t1 env s))
           ((unless ts-entry) (mv res orac trace))
           ((stmt-tracespec ts-entry))
           (trace (list (make-stmttrace
                         :name ts-entry.name
                         :stmt s
                         :initial-vars (env-find-vars ts-entry.initial-vars env)
                         :subtraces trace
                         :result (eval_result-case res
                                   :ev_normal (ev_normal (control_flow_state-kind res.res))
                                   :otherwise res)
                         :final-vars
                         (and ts-entry.final-vars ;; optimization
                              (b* ((env (eval_result-case res
                                          :ev_normal (control_flow_state-case res.res
                                                       :returning (make-env :global res.res.env :local (empty-local-env))
                                                       :continuing res.res.env)
                                          :ev_throwing res.env
                                          :otherwise nil)))
                                (and env
                                     (env-find-vars ts-entry.final-vars env)))))))
           ((when (and (not (eval_result-case res :ev_error))
                       (eq ts-entry.abort :after)))
            (pass-error-*t
             (ev_error "Trace abort" ts-entry (list (stmt->pos_start s))))))
        (mv res orac trace)))))
   


(local
 (defun find-def-and-rename (name suffix x)
   (if (atom x)
       x
     (case-match x
       (('define !name . rest)
        `(define ,(intern-in-package-of-symbol
                   (concatenate 'string (symbol-name name) "-" (symbol-name suffix) "1")
                   'asl-pkg) . ,rest))
       (& (cons (find-def-and-rename name suffix (car x))
                (find-def-and-rename name suffix (cdr x))))))))

(local
 (defun add-define-to-defines (def x)
   (if (atom x)
       x
     (case-match x
       (('defines arg . rest)
        (if (and (symbolp arg)
                 (not (keywordp arg)))
            `(defines ,arg ,def . ,rest)
          `(defines ,def ,arg . ,rest)))
       (& (cons (add-define-to-defines def (car x))
                (add-define-to-defines def (cdr x))))))))

(local
 (defun add-trace-to-returns (x)
   (if (atom x)
       x
     (case-match x
       ((':returns x . rest)
        `(:returns (,@x (trace asl-tracelist-p)) . ,rest))
       (& (cons (add-trace-to-returns (car x))
                (add-trace-to-returns (cdr x))))))))

(local
 (defun wrap-define-bodies (macro x)
   (if (atom x)
       x
     (if (and (eq (car x) 'define)
              (true-listp x))
         (let ((len (len x)))
           (update-nth (1- len)
                       (list macro (nth (1- len) x))
                       x))
       (cons (wrap-define-bodies macro (car x))
             (wrap-define-bodies macro (cdr x)))))))



  






(local
 (defun eval-return-equiv-thms (names suffix wrld)
   (if (atom names)
       nil
     (cons
      (let* ((name (car names))
             (name-mod (intern-in-package-of-symbol
                        (concatenate 'string (symbol-name name) "-"
                                     (if (or (eq name 'eval_subprogram)
                                             (eq name 'eval_stmt))
                                         (concatenate 'string (symbol-name suffix) "1")
                                       (symbol-name suffix)))
                        name))
             (macro-args (macro-args name wrld))
             (nonkey-formals (take (- (len macro-args)
                                      (len (member '&key macro-args)))
                                   macro-args)))
        `(defret ,(intern-in-package-of-symbol
                   (concatenate 'string "<FN>" "-EQUALS-ORIGINAL")
                   'asl-pkg)
           (b* (((mv res-mod orac-mod &) (,name-mod . ,nonkey-formals))
                ((mv res orac) (,name . ,nonkey-formals)))
             (implies (not (and (eval_result-case res-mod :ev_error)
                                (equal (ev_error->desc res-mod) "Trace abort")))
                      (and (equal res-mod res)
                           (equal orac-mod orac))))
           :hints ((let ((expand (acl2::just-expand-cp-parse-hints
                                  '((:free (,@nonkey-formals clk orac) (,name-mod . ,nonkey-formals))
                                    (:free (,@nonkey-formals clk orac) (,name . ,nonkey-formals)))
                                  world)))
                     `(:computed-hint-replacement
                       ((acl2::expand-marked))
                       :clause-processor (acl2::mark-expands-cp
                                          clause
                                          '(t ;; last-only
                                            t ;; lambdas
                                            ,expand))
                       :do-not-induct t)))
          :rule-classes ((:rewrite)
                         (:forward-chaining
                          :corollary
                          (b* (((mv res-mod & &) (,name-mod . ,nonkey-formals))
                               ((mv res-orig &) (,name . ,nonkey-formals)))
                            (implies (and (equal (eval_result-kind res-mod) key)
                                          (not (equal key :ev_error)))
                                     (equal (eval_result-kind res-orig) key))))
                         (:forward-chaining
                          :corollary
                          (b* (((mv res-mod & &) (,name-mod . ,nonkey-formals))
                               ((mv res-orig &) (,name . ,nonkey-formals)))
                            (implies (and (equal (ev_error->desc res-mod) desc)
                                          (not (equal desc "Trace abort")))
                                     (equal (ev_error->desc res-orig) desc))))
                         )
           :fn ,name-mod))
      (eval-return-equiv-thms (cdr names) suffix wrld)))))

(local
 (defun equals-original-thm (suffix wrld)
   (let ((eval_subprogram-mod (intern-in-package-of-symbol
                               (concatenate 'string "EVAL_SUBPROGRAM-" (symbol-name suffix))
                               'eval_subprogram))
         (eval_stmt-mod (intern-in-package-of-symbol
                         (concatenate 'string "EVAL_STMT-" (symbol-name suffix))
                         'eval_stmt)))
     `(encapsulate nil
        (local (deflabel before-equals-original))
        (std::defret-mutual
        ,(intern-in-package-of-symbol
          (concatenate 'string (symbol-name suffix) "-EQUALS-ORIGINAL")
          'asl-pkg)
        (defret ,(intern-in-package-of-symbol
                  (concatenate 'string "EVAL_SUBPROGRAM-" (symbol-name suffix)
                               "-EQUALS-ORIGINAL")
                  'asl-pkg)
          (b* (((mv res-mod orac-mod &) (,eval_subprogram-mod env name vparams vargs))
               ((mv res orac) (eval_subprogram env name vparams vargs)))
            (implies (not (and (eval_result-case res-mod :ev_error)
                               (equal (ev_error->desc res-mod) "Trace abort")))
                     (and (equal res-mod res)
                          (equal orac-mod orac))))
          :hints ((let ((expand (acl2::just-expand-cp-parse-hints
                                 '((:free (env name vparams vargs clk orac)
                                    (,eval_subprogram-mod env name vparams vargs)))
                                 world)))
                    `(:computed-hint-replacement
                      ((acl2::expand-marked))
                      :clause-processor (acl2::mark-expands-cp
                                         clause
                                         '(t ;; last-only
                                           t ;; lambdas
                                           ,expand))
                      :do-not-induct t)))
          :rule-classes ((:rewrite)
                         (:forward-chaining
                          :corollary
                          (b* (((mv res-mod & &) (,eval_subprogram-mod
                                                         env name vparams vargs))
                               ((mv res-orig &) (eval_subprogram env name vparams vargs)))
                            (implies (and (equal (eval_result-kind res-mod) key)
                                          (not (equal key :ev_error)))
                                     (equal (eval_result-kind res-orig) key))))
                         (:forward-chaining
                          :corollary
                          (b* (((mv res-mod & &) (,eval_subprogram-mod
                                                         env name vparams vargs))
                               ((mv res-orig &) (eval_subprogram env name vparams vargs)))
                            (implies (and (equal (ev_error->desc res-mod) desc)
                                          (not (equal desc "Trace abort")))
                                     (equal (ev_error->desc res-orig) desc)))))
          :fn ,eval_subprogram-mod)
        (defret ,(intern-in-package-of-symbol
                  (concatenate 'string "EVAL_STMT-" (symbol-name suffix)
                               "-EQUALS-ORIGINAL")
                  'asl-pkg)
          (b* (((mv res-mod orac-mod &) (,eval_stmt-mod env s))
               ((mv res orac) (eval_stmt env s)))
            (implies (not (and (eval_result-case res-mod :ev_error)
                               (equal (ev_error->desc res-mod) "Trace abort")))
                     (and (equal res-mod res)
                          (equal orac-mod orac))))
          :hints ((let ((expand (acl2::just-expand-cp-parse-hints
                                 '((:free (env s clk orac)
                                    (,eval_stmt-mod env s)))
                                 world)))
                    `(:computed-hint-replacement
                      ((acl2::expand-marked))
                      :clause-processor (acl2::mark-expands-cp
                                         clause
                                         '(t ;; last-only
                                           t ;; lambdas
                                           ,expand))
                      :do-not-induct t)))
          :rule-classes ((:rewrite)
                         (:forward-chaining
                          :corollary
                          (b* (((mv res-mod & &) (,eval_stmt-mod
                                                         env s))
                               ((mv res-orig &) (eval_stmt env s)))
                            (implies (and (equal (eval_result-kind res-mod) key)
                                          (not (equal key :ev_error)))
                                     (equal (eval_result-kind res-orig) key))))
                         (:forward-chaining
                          :corollary
                          (b* (((mv res-mod & &) (,eval_stmt-mod
                                                         env s))
                               ((mv res-orig &) (eval_stmt env s)))
                            (implies (and (equal (ev_error->desc res-mod) desc)
                                          (not (equal desc "Trace abort")))
                                     (equal (ev_error->desc res-orig) desc)))))
           :fn ,eval_stmt-mod)
        . ,(eval-return-equiv-thms *asl-interp-fns* suffix wrld))
        (acl2::def-ruleset! asl-*t-equals-original-rules
          (set-difference-theories (current-theory :here)
                                 (current-theory 'before-equals-original)))))))

(local
 (defun insert-after-/// (forms x)
   (if (atom x)
       x
     (if (eq (car x) '///)
         (cons '/// forms)
       (cons (insert-after-/// forms (car x))
             (insert-after-/// forms (cdr x)))))))

(local
 (defun add-mutrec-xdoc (xdoc x)
   (if (atom x)
       x
     (case-match x
       (('defines name . rest)
        `(defines ,name ,@xdoc . ,rest))
       (& (cons (add-mutrec-xdoc xdoc (car x))
                (add-mutrec-xdoc xdoc (cdr x))))))))

(local
 (defun add-define-xdoc (short x)
   (declare (xargs :mode :program))
   (if (atom x)
       x
     (case-match x
       (('define name formals . rest)
        `(define ,name ,formals
           :short ,(acl2::template-subst short
                                         :string-str-alist
                                         `(("<NAME>" . ,(symbol-name name))))
           . ,rest))
       (& (cons (add-define-xdoc short (car x))
                (add-define-xdoc short (cdr x))))))))









(local (in-theory (disable (tau-system)
                           len assoc-equal append true-listp loghead hons-assoc-equal floor mod expt take
                           acl2::repeat)))

;; Assumptions about the syntax of the interpeter definition form:
;;  - Only the one occurrence of ///
;;  - No auxiliary functions defined in :prepwork
;;  - Each define form has its body last (after all keyword args).




(local (defconst *asl-*t-xdoc*
         '(:parents (asl-tracing)
           :short "Modified version of @(see asl-interpreter-mutual-recursion) that collects a
trace of a specified set of subprogram calls."
           :long "
<p>This is an automatically generated derived version of the ASL interpreter,
@(see asl-interpreter-mutual-recursion). Each function in the original mutual
recursion has an analogous function in this version, suffixed with
@('-*t') (the \"tracing version\" of the function).  The tracing version of
each function takes the same arguments as the original version, plus an
additional keyword argument @('tracespec'), of @(see tracespec) type, which
determines what events (calls and statements) are traced. The tracing version
of each function also returns the same (two) values as the original function,
plus a third of type @(see asl-tracelist) giving the trace data from events
within that call.</p>

<p>The first two return values of the tracing version of each function are
provably the same as those from the original function, except when the tracing
has caused an abort due to the @(':abort') field in a triggered @(see
call-tracespec) or @(see stmt-tracespec). In that case the result is an
@('ev_error') with descriptor \"Trace abort\"; otherwise, the result and @(see
orac) outputs are the same as from the original functions. This is proved in
theorems @('eval_expr-*t-equals-original'), etc.</p>

<p>The functions @(see eval_subprogram-*t) and @(see eval_stmt-*t) are special
in that they perform the collection of trace data for calls and statements,
respectively. They are implemented as wrappers around the autogenerated
versions @(see eval_subprogram-*t1) and @(see eval_stmt-*t1).</p>")))



(local
 (defun add-define-formals (new-formals x)
   (if (atom x)
       x
     (case-match x
       (('define name formals . rest)
        `(define ,name ,(append formals new-formals) . ,rest))
       (& (cons (add-define-formals new-formals (car x))
                (add-define-formals new-formals (cdr x))))))))
 


(local (xdoc::set-default-parents asl-interpreter-mutual-recursion-*t))

(local
 (defthm eval_result-kind-of-rethrow_implicit
   (equal (eval_result-kind (rethrow_implicit throw blkres backtrace))
          (eval_result-kind blkres))
   :hints(("Goal" :in-theory (enable rethrow_implicit)))))

(local
 (defthm ev_error->desc-of-rethrow_implicit
   (implies (eval_result-case blkres :ev_error)
            (equal (ev_error->desc (rethrow_implicit throw blkres backtrace))
                   (ev_error->desc blkres)))
   :hints(("Goal" :in-theory (enable rethrow_implicit)))))

;; ---------------------------------------------------------------------------
;; Definition of the Tracing ASL Interpreter (suffixed with *t)
(make-event
 (b* ((form *asl-interpreter-mutual-recursion-command*)
      ;; Strip out the events after the /// (theorem about resolved-p-of-resolve-ty)
      (form (strip-post-/// form))
      ;; Strip out xdoc
      (form (strip-xdoc form))
      ;; Add xdoc topic for mutual recursion
      (form (add-mutrec-xdoc *asl-*t-xdoc* form))
      ;; Add xdoc topic for each function
      (form (add-define-xdoc
             "Tracing version of @(see <NAME>); see @(see asl-interpreter-mutual-recursion-*t) for overview."
             form))
      ;; Replace '(define eval_subprogram ...' with '(define eval_subprogram-*ft1'
      ;; since it's going to be wrapped in a call that deals with collecting the trace data.
      (form (find-def-and-rename 'eval_subprogram '*t form))
      (form (find-def-and-rename 'eval_stmt '*t form))
      ;; Substitute function names with their -*t suffixed forms.
      (form (sublis *eval-trace-substitution* form))
      ;; Wrap each define body in a call of evbody-*t.
      (form (wrap-define-bodies 'evbody-*t form))
      ;; Add (trace asl-tracelist-p) to all the :returns forms.
      (form (add-trace-to-returns form))
      ;; Add the tracespec formal to each define form.
      (form (add-define-formals '(((tracespec tracespec-p) 'tracespec)) form))
      ;; Add the definition of eval_subprogram-*t which wraps around eval_subprogram-*t1.
      (form (add-define-to-defines *eval_subprogram-*t-def* form))
      (form (add-define-to-defines *eval_stmt-*t-def* form))
      ;; Disable the functions, prove the non-trace return values equal to the originals, and verify guards.
      (form (insert-after-///
             (list
              '(make-event
                `(in-theory (disable . ,(fgetprop 'eval_expr-*t-fn 'acl2::recursivep nil (w state)))))
              (equals-original-thm '*t (w state))
              '(verify-guards eval_expr-*t-fn))
             form)))
   `(progn (defconst *asl-interpreter-mutual-recursion-*t-form* ',form)
           ,form)))
;; ---------------------------------------------------------------------------


(define find-define (name x)
  (if (atom x)
      nil
    (case-match x
      (('define !name . &) x)
      (& (or (find-define name (car x))
             (find-define name (cdr x)))))))





(defmacro trace-eval_expr-*t ()
  '(trace$ (eval_expr-*t-fn :entry (list 'eval_expr-*t e)
                         :exit (cons 'eval_expr-*t
                                     (let ((value (car values)))
                                       (eval_result-case value
                                         :ev_normal (list 'ev_normal (expr_result->val value.res))
                                         :ev_error value
                                         :ev_throwing (list 'ev_throwing value.throwdata)))))))

(defmacro trace-eval_stmt-*t (&key locals)
  `(trace$ (eval_stmt-*t-fn :entry (list 'eval_stmt-*t s
                                      . ,(and locals '((local-env->storage (env->local env)))))
                         :exit (cons 'eval_stmt-*t
                                     (let ((value (car values)))
                                       (eval_result-case value
                                         :ev_normal (cons 'ev_normal
                                                          (control_flow_state-case value.res
                                                            :returning `(:returning ,value.res.vals)
                                                            :continuing ,(if locals
                                                                             `(list :continuing (local-env->storage (env->local value.res.env)))
                                                                           ''(:continuing))))
                                         :ev_error value
                                         :ev_throwing (list 'ev_throwing
                                                            value.throwdata
                                                            . ,(and locals '((local-env->storage (env->local value.env)))))))))))


(defmacro trace-eval_subprogram-*t (&optional (evisc-tuple '(nil 7 12 nil)))
  `(trace$ (eval_subprogram-*t-fn :entry (list 'eval_subprogram-*t name vparams vargs)
                               :exit (list 'eval_subprogram-*t
                                           name
                                           (let ((value (car values)))
                                             (eval_result-case value
                                               :ev_normal (b* (((func_result value.res)))
                                                            (list 'ev_normal value.res.vals))
                                               :otherwise value)))
                               :evisc-tuple ',evisc-tuple)))


(define asl-trace->name ((x asl-trace-p))
  :returns (name symbolp :rule-classes :type-prescription)
  (asl-trace-case x :calltrace x.name :stmttrace x.name))

(define asl-trace->subtraces ((x asl-trace-p))
  :returns (subtraces asl-tracelist-p)
  (asl-trace-case x :calltrace x.subtraces :stmttrace x.subtraces)
  ///
  (defret asl-tracelist-count-of-asl-trace->subtraces
    (< (asl-tracelist-count subtraces)
       (asl-trace-count x))
    :rule-classes :linear))



(defines asl-trace-find-by-name
  (define asl-trace-find-by-name ((name symbolp)
                                  (x asl-trace-p))
    :measure (asl-trace-count x)
    (if (eq (asl-trace->name x) name)
        (cons (asl-trace-fix x)
              (asl-tracelist-find-by-name name (asl-trace->subtraces x)))
      (asl-tracelist-find-by-name name (asl-trace->subtraces x))))
  (define asl-tracelist-find-by-name ((name symbolp)
                                      (x asl-tracelist-p))
    :measure (asl-tracelist-count x)
    (if (atom x)
        nil
      (append (asl-trace-find-by-name name (car x))
              (asl-tracelist-find-by-name name (cdr x))))))





(defines asl-trace-find-calls
  (define asl-trace-find-calls ((fn identifier-p)
                                (x asl-trace-p))
    :measure (asl-trace-count x)
    (asl-trace-case x
      :calltrace (let ((rest (asl-tracelist-find-calls fn x.subtraces)))
                   (if (equal (identifier-fix fn) x.fn)
                       (cons (asl-trace-fix x) rest)
                     rest))
      :stmttrace (asl-tracelist-find-calls fn x.subtraces)))
  (define asl-tracelist-find-calls ((fn identifier-p)
                                    (x asl-tracelist-p))
    :measure (asl-tracelist-count x)
    (if (atom x)
        nil
      (append (asl-trace-find-calls fn (car x))
              (asl-tracelist-find-calls fn (cdr x))))))
