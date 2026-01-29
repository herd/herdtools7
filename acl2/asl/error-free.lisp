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

(include-book "trace-interp")
(include-book "centaur/fgl/syntax-bind" :dir :system)
(local (include-book "interp-mods"))

(local (include-book "centaur/vl/util/default-hints" :dir :system))
(local (std::add-default-post-define-hook :fix))



(local (define find-first-non-kw-arg (x)
         (cond ((atom x) nil)
               ((atom (cdr x))
                (car x))
               ((keywordp (car x))
                (find-first-non-kw-arg (cddr x)))
               (t (car x)))))

(local (define define-body (x)
         :verify-guards nil
         (find-first-non-kw-arg (nthcdr 3 x))))


(local (define no-error-def (fnname)
         :mode :program
         (b* ((fnname-no-error (intern-in-package-of-symbol
                                (concatenate 'string (symbol-name fnname) "-NO-ERROR")
                                fnname))
              (args (nth 2 (find-define fnname *asl-interpreter-mutual-recursion-*t-form*)))
              (non-kw-args (strip-cars (take (- (len args) (len (member '&key args))) args))))
           `(define ,fnname-no-error ,args
              :non-executable t
              :verify-guards nil
              :no-function t
              :hooks nil
              :enabled t
              (not (eval_result-case
                     (mv-nth 0 (,fnname . ,non-kw-args))
                     :ev_error))))))

(local (define no-error-defs (fnnames)
         :mode :program
         (if (atom fnnames)
             nil
           (cons (no-error-def (car fnnames))
                 (no-error-defs (cdr fnnames))))))


(defun find-fnnames (form)
  (if (atom form)
      nil
    (case-match form
      (('define fn . &) (list fn))
      (& (append (find-fnnames (car form))
                 (find-fnnames (cdr form)))))))

(defconst *asl-interpreter-*t-fns* (find-fnnames *asl-interpreter-mutual-recursion-*t-form*))


(make-event (cons 'progn (no-error-defs *asl-interpreter-*t-fns*)))

;; No change to this for now
;; (defmacro evtailcall-*t (call)
;;   `(b* (((evbind-*t res) ,call))
;;      (evo-return-*t res)))

(define no-error-call (form)
  :mode :program
  (let ((fnname (car form)))
    (cons (intern-in-package-of-symbol
           (concatenate 'string (symbol-name fnname) "-NO-ERROR")
           fnname)
          (cdr form))))

(acl2::def-b*-binder bind-*tef
  :body
  `(fgl::conditionalize
    ,(car acl2::args)
    ,(no-error-call (car acl2::forms))
    (b* (((mv tmp-bind-*tef . ,(cddr acl2::args)) . ,acl2::forms)
         (,(cadr acl2::args) tmp-bind-*tef))
      ,acl2::rest-expr)))


(acl2::def-b*-binder evbind-*tef
  :body
  `(b* (((bind-*tef ,(car acl2::args) tmp-evbind-*tef-res orac trace-tmp) . ,acl2::forms)
        (,(cadr acl2::args) tmp-evbind-*tef-res)
           (trace (append trace-tmp trace)))
     ,acl2::rest-expr))

(defmacro evtailcall-*tef (var call)
  `(b* (((evbind-*tef ,var res) ,call))
     (evo-return-*t res)))

(acl2::def-b*-binder evo-*tef
  :body
  `(b* ((evresult ,(car acl2::forms)))
     (fgl::conditionalize
      ,(car acl2::args)
      (not (eval_result-case evresult :ev_error))
      (eval_result-case evresult
        :ev_normal (b* ,(and (not (eq (cadr acl2::args) '&))
                             `((,(cadr acl2::args) evresult.res)))
                     ,acl2::rest-expr)
        :otherwise (mv (ev_throwing-fix evresult) orac trace)))))

(acl2::def-b*-binder evoo-*tef
  :body
  `(b* (((evbind-*tef ,(car acl2::args) evoo-*t-tmp) . ,acl2::forms)
        ((evo-*t ,(cadr acl2::args)) evoo-*t-tmp))
     (acl2::check-vars-not-free
      (evoo-*t-tmp)
     ,acl2::rest-expr)))

(acl2::def-b*-binder evs-*tef
  :body
  `(b* (((evoo-*tef ,(car acl2::args) cflow) ,(car acl2::forms)))
     (control_flow_state-case cflow
       :returning (evo_normal-*t
                   (mbe :logic (returning cflow.vals cflow.env)
                        :exec cflow))
       :continuing (b* ,(and (not (eq (cadr acl2::args) '&))
                             `((,(cadr acl2::args) cflow.env)))
                     ,acl2::rest-expr))))

(acl2::def-b*-binder evob-*tef
  :body
  `(b* ((evresult ,(car acl2::forms)))
     (fgl::conditionalize
      ,(car acl2::args)
      (not (eval_result-case evresult :ev_error))
      (eval_result-case evresult
        :ev_normal (b* ,(and (not (eq (cadr acl2::args) '&))
                             `((,(cadr acl2::args) evresult.res)))
                     ,acl2::rest-expr)
        :otherwise (mv (init-backtrace
                        (ev_throwing-fix evresult)
                        (local-env->storage (env->local env))
                        pos)
                       orac trace)))))

(local (defconst *binder-alist*
         '((evob-*t . evob-*tef)
           (evs-*t . evs-*tef)
           (evoo-*t . evoo-*tef)
           (evo-*t . evo-*tef)
           (evbind-*t . evbind-*tef)
           (evtailcall-*t . evtailcall-*tef))))

(local (defun replace-binders (form ctr)
         (if (atom form)
             (mv form ctr)
           (b* (((mv binder ctr) (replace-binders (car form) ctr))
                ((mv args ctr) (replace-binders (cdr form) ctr)))
             (if (and (symbolp binder)
                      (assoc binder *binder-alist*))
                 (mv `(,(cdr (assoc binder *binder-alist*))
                       ,(intern-in-package-of-symbol
                         (concatenate 'string "_CONDVAR-" (coerce (explode-atom ctr 10) 'string))
                         'asl-pkg)
                       . ,args)
                     (+ 1 ctr))
               (mv (cons binder args) ctr))))))

(local (defun replace-mv (form fns ctr)
         (if (atom form)
             (mv form ctr)
           (b* (((mv car ctr) (replace-mv (car form) fns ctr))
                ((mv cdr ctr) (replace-mv (cdr form) fns ctr))
                (form (cons car cdr)))
             (case-match form
               (((('mv bind1 . rest-binds) (fn . args)) . rest)
                (if (and (symbolp bind1)
                         (symbolp fn)
                         (member-eq fn fns))
                    (mv `(((bind-*tef ,(intern-in-package-of-symbol
                                        (concatenate 'string "_CONDVAR-" (coerce (explode-atom ctr 10) 'string))
                                        'asl-pkg)
                                      ,bind1 . ,rest-binds)
                           (,fn . ,args))
                          . ,rest)
                        (+ 1 ctr))
                  (mv form ctr)))
               (& (mv form ctr)))))))
                      
         

(local (define def-error-free (fnname)
         :mode :program
         (b* ((fnname-no-error (intern-in-package-of-symbol
                                (concatenate 'string (symbol-name fnname) "-NO-ERROR")
                                fnname))
              (define (find-define fnname *asl-interpreter-mutual-recursion-*t-form*))
              (body (define-body define))
              (args (nth 2 (find-define fnname *asl-interpreter-mutual-recursion-*t-form*)))
              (non-kw-args (strip-cars (take (- (len args) (len (member '&key args))) args)))
              ((mv body1 cnt) (replace-binders body 0))
              ((mv body2 ?cnt) (replace-mv body1 '(eval_subprogram-*t1 eval_stmt-*t1) cnt))
              (thmname (intern-in-package-of-symbol
                        (concatenate 'string (symbol-name fnname) "-WHEN-ERROR-FREE")
                        fnname)))
           `(defthm ,thmname
              (implies (,fnname-no-error . ,non-kw-args)
                       (equal (,fnname . ,non-kw-args)
                              ,body2))
              :hints((acl2::just-expand ((,fnname-no-error . ,non-kw-args)))
                     (acl2::just-expand ((,fnname . ,non-kw-args)))
                     '(:in-theory (enable fgl::conditionalize1 fgl::conditionalize2)))
              :rule-classes nil))))


(local (define def-error-free-fns (fns)
         :mode :program
         (if (atom fns)
             nil
           (cons (def-error-free (car fns))
                 (def-error-free-fns (cdr fns))))))
           
                                      
                              
(local (in-theory (acl2::disable* asl-*t-equals-original-rules)))


(make-event (cons 'progn (def-error-free-fns *asl-interpreter-*t-fns*)))


(local (defun error-free-thmnames (fnnames)
         (if (atom fnnames)
             nil
           (cons (let ((fnname (car fnnames)))
                   (intern-in-package-of-symbol
                    (concatenate 'string (symbol-name fnname) "-WHEN-ERROR-FREE")
                    fnname))
                 (error-free-thmnames (cdr fnnames))))))

(defconsts *error-free-thmnames* (error-free-thmnames *asl-interpreter-*t-fns*))

(local (defun no-error-fnnames (fnnames)
         (if (atom fnnames)
             nil
           (cons (let ((fnname (car fnnames)))
                   (intern-in-package-of-symbol
                    (concatenate 'string (symbol-name fnname) "-NO-ERROR")
                    fnname))
                 (no-error-fnnames (cdr fnnames))))))

(defconsts *no-error-fnnames* (no-error-fnnames *asl-interpreter-*t-fns*))

(defmacro fgl-disable-asl-no-error-defs ()
  `(fgl::remove-fgl-rewrites . ,*no-error-fnnames*))

(defmacro error-free-fgl-theory ()
  `(progn (fgl-disable-asl-no-error-defs)
          (fgl::add-fgl-rewrites . ,*error-free-thmnames*)))

