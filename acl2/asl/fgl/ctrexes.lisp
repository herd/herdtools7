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
(include-book "centaur/fgl/backtrace" :dir :system)
(include-book "centaur/fgl/fgl-object" :dir :system)
(include-book "centaur/fgl/ctrex-utils" :dir :system)
(include-book "centaur/fgl/helper-utils" :dir :system)
(include-book "defs")





(define-continue counterexample-with-backtrace
  (fgl::remove-fgl-rewrite counterexample-with-backtrace)
  (fgl::disable-execution counterexample-with-backtrace)
  (fgl::def-fgl-rewrite counterexample-with-backtrace-impl
    (equal (counterexample-with-backtrace sat-config err)
           (b* (((unless (fgl::syntax-bind
                          ctrex-enabledp
                          (fgl::fgl-config->counterexample-analysis-enabledp
                           (fgl::interp-st->config 'interp-st))))
                 (fgl::fgl-prog2
                  (fgl::fgl-error
                   :msg "Counterexample-with-backtrace encountered (but not analyzing counterexamples)")
                  nil))
                ((list env orac backtrace)
                 ;; Collect the oracle, final environment, assignment position,
                 ;; and ASL interpreter backtrace from the stack.
                 (fgl::syntax-bind
                  e-o-b
                  ;; Backtrace: collect the function call stack from all EVAL_SUBPROGRAM calls,
                  ;; and the positions of those calls from EVAL_CALL.
                  (b* ((backtrace (fgl::interp-st-stack-backtrace (list (fgl::backtrace-spec
                                                                         '(:FORMULA EVAL_SUBPROGRAM-FN)
                                                                         '(name vparams vargs))
                                                                        (fgl::backtrace-spec
                                                                         '(:formula eval_call-fn) '(pos))
                                                                        (fgl::backtrace-spec
                                                                         '(:FORMULA EVAL_SUBPROGRAM-*t-FN)
                                                                         '(name vparams vargs))
                                                                        (fgl::backtrace-spec
                                                                         '(:formula eval_call-*t-fn) '(pos)))
                                                                  'interp-st))
                       (backtrace-obj (fgl::backtrace-to-obj backtrace))
                       (subprog-frame (fgl::interp-st-stack-backtrace-top
                                       (list (fgl::backtrace-spec
                                              '(:formula eval_subprogram-fn)
                                              '(env orac))
                                             (fgl::backtrace-spec
                                              '(:formula eval_call-fn)
                                              '(env orac))
                                             (fgl::backtrace-spec
                                              '(:formula eval_subprogram-*t-fn)
                                              '(env orac))
                                             (fgl::backtrace-spec
                                              '(:formula eval_call-*t-fn)
                                              '(env orac)))
                                       'interp-st))
                       ((mv env orac)
                        (if subprog-frame
                            (b* ((bindings (fgl::backtrace-frame->objs subprog-frame))
                                 (env (cdr (hons-assoc-equal 'env bindings)))
                                 (orac (cdr (hons-assoc-equal 'orac bindings))))
                              ;; (fmt-to-comment-window "env: ~x0~%orac: ~x1~%frame: ~x2~%"
                              ;;                        `((#\0 . ,env) (#\1 . ,orac) (#\2 . ,subprog-frame))
                              ;;                        0 '(nil 5 7 nil) nil)
                              (mv env orac))
                          (prog2$ (cw "no subprog-frame found~%")
                                  (mv nil nil)))))
                    (fgl::g-cons env
                                 (fgl::g-cons orac
                                              (fgl::g-cons backtrace-obj nil))))))
                ;; Add oracle-marker to the Boolean variable database so we can
                ;; derive an oracle for the counterexample
                (?ignore (and (oracle-marker orac) t)))
             (fgl::fgl-prog2
              (fgl::fgl-prog2
               (fgl::syntax-interp
                (b* (((mv sat-err &) (fgl::interp-st-sat-counterexample (fgl::g-concrete->val sat-config) 'interp-st 'state))
                     ((when sat-err) (fgl::fgl-error :msg (msg "interp-st-sat-counterexample error in counterexample-with-backtrace: ~@0"
                                                               sat-err)))
                     ((mv bindings var-vals &) (fgl::interp-st-counterex-bindings/print-errors `((backtrace . ,backtrace) (env . ,env)) 'interp-st 'state)))
                  (fmt-to-comment-window "Counterexample: ~x0~%"
                                         `((#\0 . ,var-vals))
                                         0 '(nil 10 100 nil) nil)
                  (cw "Backtrace: ~x0~%" (cdr (hons-assoc-equal 'backtrace bindings)))
                  (fgl::interp-st-put-user-scratch :env (cdr (hons-assoc-equal 'env bindings)) 'interp-st)
                  (fgl::interp-st-run-ctrex (fgl::g-concrete->val sat-config) 'interp-st 'state)))
               (fgl::fgl-error :msg err))
              nil)))))





(define show-write-example (sat-config name)
  :non-executable t
  :verify-guards nil
  (declare (ignorable sat-config name))
  nil
  ///
  (fgl::remove-fgl-rewrite show-write-example)
  (fgl::def-fgl-rewrite show-write-example-impl
    (equal (show-write-example sat-config name)
           (b* (((unless (fgl::syntax-bind
                          ctrex-enabledp
                          (fgl::fgl-config->counterexample-analysis-enabledp
                           (fgl::interp-st->config 'interp-st))))
                 (fgl::fgl-prog2
                  (fgl::fgl-error
                   :msg "Show-write-example encountered (but not analyzing counterexamples)")
                  nil))
                ((list env orac lx backtrace)
                 ;; Collect the oracle, final environment, assignment position,
                 ;; and ASL interpreter backtrace from the stack.
                 (fgl::syntax-bind
                  e-o-b
                  ;; Backtrace: collect the function call stack from all EVAL_SUBPROGRAM calls,
                  ;; and the positions of those calls from EVAL_CALL.
                  (b* ((backtrace (fgl::interp-st-stack-backtrace (list (fgl::backtrace-spec
                                                                         '(:FORMULA EVAL_SUBPROGRAM-FN)
                                                                         '(name vparams vargs))
                                                                        (fgl::backtrace-spec
                                                                         '(:formula eval_call-fn) '(pos)))
                                                                  'interp-st))
                       (backtrace-obj (fgl::backtrace-to-obj backtrace))
                       ;; Env, oracle, and lexpr (assignment): find the top frame of any rule targeting
                       ;; EVAL_LEXPR-FN and get the env/orac/lx from that.
                       (eval_lexpr-specs (fgl::function-rules-to-backtrace-spec
                                          'eval_lexpr-fn '(env orac lx) 'interp-st (w 'state)))
                       (eval_lexpr-frame (fgl::interp-st-stack-backtrace-top
                                          eval_lexpr-specs
                                          'interp-st))
                       (bindings (fgl::backtrace-frame->objs eval_lexpr-frame)))
                    (fgl::g-cons (cdr (hons-assoc-equal 'env bindings))
                                 (fgl::g-cons (cdr (hons-assoc-equal 'orac bindings))
                                              (fgl::g-cons (cdr (hons-assoc-equal 'lx bindings))
                                                           (fgl::g-cons backtrace-obj nil))))))))
             ;; Add oracle-marker to the Boolean variable database so we can
             ;; derive an oracle for the counterexample
             (let ((ignore (and (oracle-marker orac) t)))
               (declare (ignore ignore))
               (fgl::fgl-prog2
                (fgl::fgl-prog2
                 (fgl::syntax-interp
                  (b* (((mv sat-err &) (fgl::interp-st-sat-counterexample (fgl::g-concrete->val sat-config) 'interp-st 'state))
                       ((when sat-err) (fgl::fgl-error :msg (msg "interp-st-sat-counterexample error in show-write-example: ~@0"
                                                                 sat-err)))
                       ((mv bindings var-vals &) (fgl::interp-st-counterex-bindings/print-errors `((backtrace . ,backtrace) (env . ,env) (lx . ,lx)) 'interp-st 'state)))
                    (cw "Counterexample: ~x0~%" var-vals)
                    (cw "Lx: ~x0~%" (cdr (hons-assoc-equal 'lx bindings)))
                    (cw "Backtrace: ~x0~%" (cdr (hons-assoc-equal 'backtrace bindings)))
                    (fgl::interp-st-put-user-scratch :env (cdr (hons-assoc-equal 'env bindings)) 'interp-st)))
                 (fgl::fgl-error :msg (msg "Found write of ~s0" name)))
                nil))))))


(defmacro def-counterexample-on-write (thmname &key lhs syntaxp name)
  `(fgl::def-fgl-rewrite ,thmname
     (implies (and ,@(and syntaxp `((syntaxp ,syntaxp)))
                   (equal sat-config (fgl::make-fgl-ipasir-config))
                   (fgl::fgl-sat-check sat-config t))
              (equal ,lhs
                     (b* ((?ign (show-write-example sat-config ,name)))
                       (fgl::abort-rewrite ,lhs))))))


;; NOTE: This assumes that our global storage is initially a variable named storage!
(defmacro def-counterexample-on-write-global-var (thmname var)
  (b* ((thmname-1 (intern-in-package-of-symbol (concatenate 'string (symbol-name thmname) "-1") thmname))
       (thmname-2 (intern-in-package-of-symbol (concatenate 'string (symbol-name thmname) "-2") thmname)))
    `(progn (def-counterexample-on-write ,thmname-1
              :lhs (val-imap-put ,var val storage)
              :syntaxp (equal storage '(:g-var . storage))
              :name ,var)
            (def-counterexample-on-write ,thmname-2
              :lhs (val-imap-put ,var val (val-imap-put-pairs pairs storage))
              :syntaxp (equal storage '(:g-var . storage))
              :name ,var))))

(defmacro def-counterexample-on-write-global-record-field (thmname var field)
  (b* ((thmname-1 (intern-in-package-of-symbol (concatenate 'string (symbol-name thmname) "-1") thmname))
       (thmname-2 (intern-in-package-of-symbol (concatenate 'string (symbol-name thmname) "-2") thmname))
       (name (concatenate 'string var "." field)))
    `(progn (def-counterexample-on-write ,thmname-1
              :lhs (val-imap-put ,field val (v_record->rec (val-imap-lookup ,var storage)))
              :syntaxp (equal storage '(:g-var . storage))
              :name ,name)
            (def-counterexample-on-write ,thmname-2
              :lhs (val-imap-put ,field val (val-imap-put-pairs pairs (v_record->rec (val-imap-lookup ,var storage))))
              :syntaxp (equal storage '(:g-var . storage))
              :name ,name))))

