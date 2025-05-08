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
(include-book "centaur/meta/variable-free" :dir :system)
(include-book "tools/easy-simplify" :dir :system)

(define find-nth-form-aux ((n natp)
                           (tag symbolp)
                           x)
  :returns (mv (new-n acl2::maybe-natp :rule-classes :type-prescription)
               form)
  (b* (((when (atom x)) (mv (lnfix n) nil))
       ((when (and (zp n) (eq (car x) tag)))
        (mv nil x))
       (next-n (if (eq (car x) tag) (1- n) n))
       ((mv new-n form) (find-nth-form-aux next-n tag (car x)))
       ((unless new-n) (mv nil form)))
    (find-nth-form-aux new-n tag (cdr x))))

(define find-nth-form ((n natp) (tag symbolp) x)
  (b* (((mv & form) (find-nth-form-aux n tag x)))
    form))




;; Induction scheme for while loops
(define loop-induct ((env env-p) (whilep) (limit acl2::maybe-integerp) (test expr-p) (body stmt-p)
                     &key ((clk natp) 'clk) (orac 'orac))
  :verify-guards nil
  :non-executable t
  :measure (nfix clk)
  (b* (((mv (ev_normal cev) orac) (eval_expr env test))
       ((expr_result cev.res))
       ((unless (iff (ev_normal->res (v_to_bool cev.res.val)) whilep))
        env)
       (limit (ev_normal->res (tick_loop_limit limit)))
       ((mv (ev_normal blkev) orac) (eval_block cev.res.env body))
       ((continuing blkev.res))
       ((when (zp clk))
        blkev.res.env))
    (loop-induct blkev.res.env whilep limit test body :clk (1- clk))))
(in-theory (enable (:i loop-induct)))


(define for-induct ((env env-p) (index_name identifier-p) (limit acl2::maybe-integerp) (start integerp) (dir for_direction-p) (end integerp) (body stmt-p) &key ((clk natp) 'clk) (orac 'orac))
  :verify-guards nil
  :non-executable t
  :measure (for_loop-measure start end dir)
  (b* (((when (for_loop-test start end dir))
        env)
       (limit (ev_normal->res (tick_loop_limit limit)))
       ((mv (ev_normal blkev) orac) (eval_block env body))
       ((continuing blkev.res))
       ((mv step env2) (eval_for_step blkev.res.env index_name start dir)))
    (for-induct env2 index_name limit step dir end body)))
(in-theory (enable (:i for-induct)))

    

(define spmatch-force-exec (x) x
  ///
  (in-theory (disable (:t spmatch-force-exec))))

(cmr::def-force-execute force-execute-spmatch-force-exec spmatch-force-exec)

(in-theory (enable force-execute-spmatch-force-exec))



(define all-entries-match (subprogs (ref-subprogs alistp))
  :returns (ok)
  (if (atom ref-subprogs)
      t
    (and (equal (hons-assoc-equal (caar ref-subprogs) subprogs)
                (car ref-subprogs))
         (all-entries-match subprogs (cdr ref-subprogs))))
  ///
  (defret lookup-when-<fn>
    (implies (and ok
                  (equal ref-look (hons-assoc-equal id ref-subprogs))
                  ref-look)
             (equal (hons-assoc-equal id subprogs)
                    ref-look))))

(define all-subprograms-match ((env static_env_global-p)
                               (ref-env static_env_global-p))
  :returns (ok)
  (all-entries-match (static_env_global->subprograms env)
                     (static_env_global->subprograms ref-env))
  ///
  (defret lookup-when-<fn>
    (implies (and (syntaxp (quotep name))
                  ok
                  (equal look (spmatch-force-exec
                               (hons-assoc-equal name (static_env_global->subprograms ref-env))))
                  (syntaxp (quotep look))
                  look)
             (equal (hons-assoc-equal name (static_env_global->subprograms env))
                    look))
    :hints(("Goal" :in-theory (enable spmatch-force-exec)))))

(define all-subprograms-match ((env static_env_global-p)
                               (ref-env static_env_global-p))
  :returns (ok)
  (all-entries-match (static_env_global->subprograms env)
                     (static_env_global->subprograms ref-env))
  ///
  (defret lookup-when-<fn>
    (implies (and (syntaxp (quotep name))
                  ok
                  (equal look (spmatch-force-exec
                               (hons-assoc-equal name (static_env_global->subprograms ref-env))))
                  (syntaxp (quotep look))
                  look)
             (equal (hons-assoc-equal name (static_env_global->subprograms env))
                    look))
    :hints(("Goal" :in-theory (enable spmatch-force-exec)))))

(define subprograms-match ((names identifierlist-p)
                           (env static_env_global-p)
                           (env1 static_env_global-p))
  (if (atom names)
      t
    (and (equal (hons-assoc-equal (car names)
                                  (static_env_global->subprograms env))
                (hons-assoc-equal (car names)
                                  (static_env_global->subprograms env1)))
         (subprograms-match (cdr names) env env1)))
  ///
  (defthm subprogram-lookup-when-match
    (implies (and (syntaxp (quotep name))
                  (subprograms-match names env env1)
                  (spmatch-force-exec (member-equal name names))
                  (equal look (spmatch-force-exec
                               (hons-assoc-equal name (static_env_global->subprograms env1))))
                  (syntaxp (quotep look)))
             (equal (hons-assoc-equal name
                                      (static_env_global->subprograms env))
                    look))
    :hints(("Goal" :in-theory (enable spmatch-force-exec))))

  (local (defthm subprogram-match-when-member
           (implies (and (subprograms-match names1 env env1)
                         (member-equal name names1))
                    (equal (equal (hons-assoc-equal name
                                                    (static_env_global->subprograms env))
                                  (hons-assoc-equal name
                                                    (static_env_global->subprograms env1)))
                           t))))

  (defthm subprograms-match-when-subsetp
    (implies (and (syntaxp (quotep names))
                  (subprograms-match names1 env env1)
                  (syntaxp (quotep names1))
                  (subsetp-equal names names1))
             (subprograms-match names env env1))))





                    
                       
(program)

(defun loop-local-var-bindings (local-vars)
  (if (atom local-vars)
      nil
    (let* ((rest (loop-local-var-bindings (cdr local-vars)))
           (first (car local-vars)))
      (case-match first
        (((ctor name) str . &)
         (if (and (stringp str)
                  (member ctor '(v_int v_bool v_real v_string v_bitvector v_label v_record v_array))
                  (symbolp name))
             (append (acl2::template-subst '((<name>-look (hons-assoc-equal <str> env.local.storage))
                                             ((<ctor> <name>) (cdr <name>-look)))
                                           :atom-alist `((<ctor> . ,ctor)
                                                         (<name> . ,name)
                                                         (<str> . ,str))
                                           :str-alist `(("<NAME>" . ,(symbol-name name)))
                                           :pkg-sym 'asl-pkg)
                     rest)
           (er hard? 'loop-local-var-bindings "Bad local-vars entry: ~x0" first)))
        (& (er hard? 'loop-local-var-bindings "Bad local-vars entry: ~x0" first))))))

;; (loop-local-var-bindings '(((v_bitvector x) "__stdlib_local_x") ((v_int n) "__stdlib_local_N")))

(defun loop-local-var-hyps (local-vars)
  (if (atom local-vars)
      nil
    (let* ((rest (loop-local-var-hyps (cdr local-vars)))
           (first (car local-vars)))
      (case-match first
        (((ctor name) & . &)
         (append (acl2::template-subst '(<name>-look
                                         (val-case <name> <kind>))
                                       :atom-alist `((<kind> . ,(intern-in-package-of-symbol
                                                                 (symbol-name ctor) :keyword-pkg))
                                                     (<name> . ,name))
                                       :str-alist `(("<NAME>" . ,(symbol-name name)))
                                       :pkg-sym 'asl-pkg)
                 rest))
        (& rest)))))

(defun loop-local-vars-final-env (local-vars)
  (if (atom local-vars)
      'env.local.storage
    (let* ((rest (loop-local-vars-final-env (cdr local-vars)))
           (first (car local-vars)))
      (case-match first
        ((& str final-val)
         `(put-assoc-equal ,str ,final-val ,rest))
        (& rest)))))

;; (loop-local-var-hyps '(((v_bitvector x) "__stdlib_local_x") ((v_int n) "__stdlib_local_N")))



(defconst *defloop-template*
  '(defsection <name>
     (defconst *<name>*
       (find-nth-form <nth> <looptype>
                      (hons-assoc-equal <fn>
                                        (static_env_global->subprograms <static-env>))))

     (:@ (not :s_for)
      (defconst *<name>-test*
        (<looptype>->test *<name>*)))

     (defconst *<name>-body*
       (<looptype>->body *<name>*))

     <prepwork>
     
     (local (in-theory (acl2::e/d* (<defloop-enables>
                                    <user-enables>)
                                   (<defloop-disables>
                                    <user-disables>))))
     (defthm <name>-correct
       (b* (((env env))
            ((local-env env.local))
            <local-var-bindings>
            <user-bindings>)
         (implies (and <local-var-hyps>
                       ;; <measure-hyps>
                       <invariants>
                       (no-duplicatesp-equal (acl2::alist-keys env.local.storage)))
                  (b* (((mv (ev_normal res) &) <loop-form>))
                    <concl>)))
       :hints (;; copied from just-induct-and-expand
               (if (equal (car id) '(0))
                   (let* ((expand-hints (acl2::just-expand-cp-parse-hints
                                         '(<loop-form-expand>) (w state)))
                          (cproc `(acl2::mark-expands-cp clause '(nil t ,expand-hints))))
                     `(:computed-hint-replacement
                       ((and (equal (car id) '(0)) '(:clause-processor acl2::clause-to-term))
                        (and (equal (car id) '(0)) '(:induct <induction>)))
                       :clause-processor ,cproc))
                 (and (equal (car id) '(0 1))
                      (acl2::expand-marked :last-only t)))
               <user-hints>))))





(define defloop-fn (name args state)
  (b* (((std::extract-keyword-args
         :other-args bad-args
         ;; :kwd-alist kwd-alist
         function
         (looptype :while)
         (nth 0)

         ;; format: ((val_type acl2-varname) "asl-varname" [ final value ] )* --
         ;; e.g. (((v_bitvector x) "__stdlib_local_x" x-final-value) ((v_int n) "__stdlib_local_N"))
         ;; For loop index is separately listed as index-var
         local-vars
         index-var
         (start-var 'start)
         (end-var 'end)

         ;; For now we just support loops that execute normally (no
         ;; throw/error) -- eventually add conditions for throwing/erroring.

         ;; When loop finishes (continuing), then the local-vars above give the
         ;; final values stored in the updated environment. If it returns
         ;; early, we need to know the return condition and return values.
         (return-cond 'nil)
         return-values
         
         enable
         disable
         hints
         prepwork
         
         (invariants 't)
         bindings
         (static-env '(stdlib-static-env)))
        args)
       ((when bad-args)
        (er soft 'defloop "Bad arguments: ~x0" bad-args))
       ((unless (stringp function))
        (er soft 'defloop "Function should be a string: ~x0" function))
       (orig-looptype looptype)
       (looptype (case orig-looptype
                   ((:while :s_while) :s_while)
                   ((:for :s_for) :s_for)
                   ((:repeat :s_repeat) :s_repeat)
                   (t nil)))
       ((unless looptype)
        (er soft 'defloop "Bad looptype: ~x0" orig-looptype))
       ((unless (natp nth))
        (er soft 'defloop "Bad nth: ~x0" nth))
       ((acl2::er (cons & static-env-val))
        (acl2::simple-translate-and-eval static-env nil nil
                                         (msg "static env ~x0" static-env)
                                         'defloop (w state) state t))
       ((unless (static_env_global-p static-env-val))
        (er soft 'defloop "Bad static env (evaluation of ~x0): doesn't satisfy static_env_global-p" static-env))
       (fn-struct (hons-assoc-equal function
                                    (static_env_global->subprograms static-env-val)))
       ((unless fn-struct)
        (er soft 'defloop "Bad function ~x0: not found in static env" function))
       ;; if found, then it's the right type
       (form (find-nth-form nth looptype fn-struct))
       ((unless form)
        (er soft 'defloop "Loop not found: function ~x0 looptype ~x1 nth ~x2" function looptype nth))

       ((when (and (eq looptype :s_for)
                   (not index-var)))
        (er soft 'defloop "Index var must be specified for for loops"))
       ((when (and index-var (not (eq looptype :s_for))))
        (er soft 'defloop "Index var specified for non-for loop"))
       
       (local-vars (if (eq looptype :s_for)
                       (cons `((v_int ,index-var)
                               ,(s_for->index_name form)
                               (v_int
                                ,(if (eq (s_for->dir form) :up)
                                     `(+ 1 ,end-var)
                                   `(+ -1 ,end-var))))
                             local-vars)
                     local-vars))
                               
       (local-var-bindings (loop-local-var-bindings local-vars))
       (local-var-hyps (loop-local-var-hyps local-vars))
       (local-var-hyps (if (eq looptype :s_for)
                           (append local-var-hyps
                                   `((equal (v_int->val ,index-var) ,start-var)
                                     (integerp ,end-var)
                                     ,(if (eq (s_for->dir form) :up)
                                          `(<= ,start-var (+ 1 ,end-var))
                                        `(<= (+ -1 ,end-var) ,start-var))))
                         local-var-hyps))
        
       (local-var-final-env (loop-local-vars-final-env local-vars))

       (continuing-concl
        `(and (equal (control_flow_state-kind res.res) :continuing)
              (b* (((continuing res.res)))
                (equal res.res.env
                       (change-env env
                                   :local
                                   (change-local-env
                                    env.local
                                    :storage ,local-var-final-env))))))
       (returning-concl
        `(and (equal (control_flow_state-kind res.res) :returning)
              (b* (((returning res.res)))
                (and (equal res.res.vals ,return-values)
                     (equal res.res.env env.global)))))

       (normal-concl
        `(and (equal (eval_result-kind res) :ev_normal)
              ,@(and (not (eq return-cond t))
                     `(,(if (eq return-cond nil)
                            continuing-concl
                          `(implies (not ,return-cond)
                                    ,continuing-concl))))
              ,@(and (not (eq return-cond nil))
                     `(,(if (eq return-cond t)
                            returning-concl
                          `(implies ,return-cond
                                    ,returning-concl))))))
                       
              
       ((acl2::tmplsubst template)
        (acl2::make-tmplsubst
         :atoms `((<name> . ,name)
                  (<nth> . ,nth)
                  (<looptype> . ,looptype)
                  (<fn> . ,function)
                  (<static-env> . ,static-env)
                  (<invariants> . ,invariants)
                  (<concl> . ,normal-concl))
         :splices `((<defloop-enables> . (asl-code-proof-enables))
                    (<defloop-disables> . (asl-code-proof-disables))
                    (<user-enables> . ,enable)
                    (<user-disables> . ,disable)
                    (<local-var-bindings> . ,local-var-bindings)
                    (<local-var-hyps> . ,local-var-hyps)
                    (<user-hints> . ,hints)
                    (<user-bindings> . ,bindings)
                    (<prepwork> . ,prepwork))
         :strs `(("<NAME>" . ,(symbol-name name))
                 ("<LOOPTYPE>" . ,(symbol-name looptype)))
         :features (list looptype)
         :pkg-sym 'asl-pkg))

       (body-const (acl2::template-subst-top '*<name>-body* template))
       (test-const (acl2::template-subst-top '*<name>-test* template))
       
       ((mv loop-form expand induction)
        (case looptype
          (:s_for
           (let ((args `(env ,(s_for->index_name form)
                             ,(and (s_for->limit form) 'limit)
                             ,start-var ,(s_for->dir form) ,end-var
                             ,body-const)))
             (mv `(eval_for  . ,args)
                 `(:free (limit ,start-var ,end-var)
                   (eval_for . ,args))
                 `(for-induct . ,args))))
          (t
           (let ((args `(env ,(eq looptype :s_while)
                             ,(and (if (eq looptype :s_while)
                                       (s_while->limit form)
                                     (s_repeat->limit form))
                                   'limit)
                             ,test-const ,body-const)))
             (mv `(eval_loop . ,args)
                 `(:free (limit) (eval_loop . ,args))
                 `(loop-induct . ,args))))))

       (template (acl2::change-tmplsubst
                  template :atoms `((<loop-form> . ,loop-form)
                                    (<induction> . ,induction)
                                    (<loop-form-expand> . ,expand)
                                    . ,template.atoms)))
                      
                      
       (event
        (acl2::template-subst-top *defloop-template* template)))
    (value event)))

(defxdoc defloop
  :parents (asl-proofs)
  :short "Utility for proving the result of evaluating an ASL loop statement"
  :long "
<p>Usage:</p>
@({
 (defloop user-loopname
   :function \"asl_function_name\"
   :looptype :s_for                 ;; or :s_while, :s_repeat
   :nth 0                           ;; which occurence of this type of loop in the function -- default 0
   :index-var i                     ;; ACL2 variable corresponding to the index of a for loop
 
   :static-env (stdlib-static-env)  ;; Expression for static environment in which the function
                                    ;; and its dependencies are defined -- default (stdlib-static-env)
 
   :local-vars ((n \"asl_var_n\")   ;; ACL2 variable n corresponds to ASL variable \"asl_var_n\", read only
                (res \"result\"     ;; ACL2 variable res corresponds to ASL variable \"res\", which is updated
                 (v_int spec)))     ;; such that its final value is (v_int (my-spec n.val))
 
   :bindings ((spec (my-spec n.val))) ;; B* bindings using the local-vars and accessors for their
                                      ;; respective value types. May be used in the final value arguments
                                      ;; (as above for res).
 
   :invariants (and (<= 0 n.val)    ;; Hypotheses/inductive invariants about the loop
                    (equal end 10)  
                    (equal res.val (v_int (my-partial-spec i.val n.val))))
 
   :start-var my-start    ;; Name for start and end variables -- default start, end
   :end-var   end
 
   :return-cond (>= n.val 10)      ;; Condition under which the loop causes a return at some point
                                   ;; instead of finishing
   :return-values (list (v_int (- n.val i.val))) ;; Values that are returned when early returning
 
   :enable (foo bar)               ;; rules to enable
   :disable (baz)                  ;; rules to disable
   :hints ((and stable-under-simplificationp
            '(:expand ((:free (n) (my-partial-spec start n))))))
                                   ;; computed hints to add to the default induction hint
 
   :prepwork ((local (defthm lemma ...))) ;; events to do before the proof
   )
 })

<p>This generates a few events: it defines a constant for the loop form and its
body, another for the loop test unless it is a for loop, a preparatory theory
event, and then the correctness proof for the loop.  The format of the proof is
as follows, more or less following the above made-up example:</p>

@({
 (defthm user-loopname-correct
   (b* (((env env))
        ((local-env env.local))

        ;; Bind lookups of local variables from the environment, including the loop index
        (i-look (hons-assoc-equal \"loop_index_var\" env.local.storage))
        ((v_int i) (cdr i-look))
        (n-look (hons-assoc-equal \"asl_var_n\" env.local.storage))
        ((v_real n) (cdr n-look))
        (res-look (hons-assoc-equal \"result\" env.local.storage))
        ((v_int res) (cdr res-look))

        ;; Additional user-specified bindings:
        (spec (my-spec n.val)))
     (implies (and ;; local variables are present and of the right value kind:
                   i-look
                   (val-case i :v_int)
                   n-look
                   (val-case n :v_real)
                   res-look
                   (val-case res :v_int)
                   ;; loop index always equals start variable:
                   (equal i.val my-start)

                   ;; user-specified invariant assumptions:
                   (and (<= 0 n.val)
                        (equal end 10)  
                        (equal res.val (v_int (my-partial-spec i.val n.val))))

                   ;; additional invariant about well formedness of local storage alist:
                   (no-duplicatesp-equal (acl2::alist-keys env.local.storage)))
              (b* (((mv (ev_normal res) &)
                    ;; Autogenerated evaluation form for loop
                    (eval_for env \"loop_index_var\" nil my-start :up end *user-loopname-body*)))
                ;; automatic conclusions: result is normal and continuning except in return condition
                (and (equal (eval_result-kind res) :ev_normal)
                     (implies (>= n.val 10) ;; return condition
                              ;; conditions when returning:
                              (and (equal (control_flow_state-kind res.res) :returning))
                                   (b* (((returning res.res)))
                                     ;; user specified return value
                                     (and (equal res.res.vals (list (v_int (- n.val i.val))))
                                          ;; loop doesn't mess up the global env...
                                          (equal res.res.env env.global))))
                     (implies (not (>= n.val 10)) ;; not returning condition
                              (and (equal (control_flow_state-kind res.res) :continuing)
                                   (b* (((continuing res.res)))
                                     ;; resulting environment is the same as before,
                                     ;; but with local storage updated as specified in local-vars
                                     ;; and the loop index one past the end:
                                     (equal res.res.env
                                            (change-env
                                             env :local
                                             :local
")

(defmacro defloop (name &rest args)
  `(make-event (defloop-fn ',name ',args state)))



(table asl-subprogram-table)


(define subprogram-param-bindings ((params symbol-listp)
                                   (fn-params maybe-typed_identifierlist-p))
  :guard (equal (len params) (len fn-params))
  (b* (((when (atom params)) nil)
       ((maybe-typed_identifier p1) (car fn-params))
       (ctor (if p1.type
                 (b* ((ty (ty->val p1.type)))
                   (type_desc-case ty
                     (:t_int 'v_int)
                     (:t_bits 'v_bitvector)
                     (:t_real 'v_real)
                     (:t_string 'v_string)
                     (:t_bool 'v_bool)
                     (:t_enum 'v_label)
                     (:t_tuple 'v_array)
                     (:t_array 'v_array)
                     (:t_record 'v_record)
                     (:t_exception 'v_record)
                     (:t_collection 'v_record)
                     (:otherwise nil)))
               'v_int))
       ((unless ctor)
        (er hard? 'def-asl-subprogram "Couldn't understand parameter: ~x0" p1)))
    (cons `((,ctor ,(car params)))
          (subprogram-param-bindings (cdr params) (cdr fn-params)))))

(define subprogram-arg-bindings ((args symbol-listp)
                                   (fn-args typed_identifierlist-p))
  :guard (equal (len args) (len fn-args))
  (b* (((when (atom args)) nil)
       ((typed_identifier p1) (car fn-args))
       (ctor (b* ((ty (ty->val p1.type)))
               (type_desc-case ty
                 (:t_int 'v_int)
                 (:t_bits 'v_bitvector)
                 (:t_real 'v_real)
                 (:t_string 'v_string)
                 (:t_bool 'v_bool)
                 (:t_enum 'v_label)
                 (:t_tuple 'v_array)
                 (:t_array 'v_array)
                 (:t_record 'v_record)
                 (:t_exception 'v_record)
                 (:t_collection 'v_record)
                 (:otherwise nil))))
       ((unless ctor)
        (er hard? 'def-asl-subprogram "Couldn't understand arg: ~x0" p1)))
    (cons `((,ctor ,(car args)))
          (subprogram-arg-bindings (cdr args) (cdr fn-args)))))




(logic)
(define constraint_kind-remove-parameters ((x constraint_kind-p))
  :returns (new-x constraint_kind-p)
  (constraint_kind-case x
    :parametrized (unconstrained)
    :otherwise (constraint_kind-fix x)))


(defines ty-remove-parameters
  :ruler-extenders :lambdas
  :verify-guards nil
  (define ty-remove-parameters ((x ty-p))
    :measure (ty-count x)
    :returns (new-x ty-p)
    (b* ((?orig x)
         (x (ty->val x)))
      (change-ty orig
                 :val
                 (type_desc-case x
                   :t_int (t_int (constraint_kind-remove-parameters x.constraint))
                   :t_tuple (t_tuple (tylist-remove-parameters x.types))
                   :t_array (change-t_array x :type (ty-remove-parameters x.type))
                   :t_record (t_record (typed_identifierlist-remove-parameters x.fields))
                   :t_exception (t_exception (typed_identifierlist-remove-parameters x.fields))
                   :t_collection (t_collection (typed_identifierlist-remove-parameters x.fields))
                   :otherwise x))))
  (define tylist-remove-parameters ((x tylist-p))
    :measure (tylist-count x)
    :returns (new-x tylist-p)
    (if (atom x)
        nil
      (cons (ty-remove-parameters (car x))
            (tylist-remove-parameters (cdr x)))))

  (define typed_identifierlist-remove-parameters ((x typed_identifierlist-p))
    :measure (typed_identifierlist-count x)
    :returns (new-x typed_identifierlist-p)
    (if (atom x)
        nil
      (cons (b* (((typed_identifier x1) (car x)))
              (change-typed_identifier x1 :type (ty-remove-parameters x1.type)))
            (typed_identifierlist-remove-parameters (cdr x)))))
  ///
  (verify-guards ty-remove-parameters)
  (fty::deffixequiv-mutual ty-remove-parameters))
                          

(program)

(define simplify-for-def-asl-subprogram (term hyp state)
  ;; NOTE: always simplifies under IFF
  (acl2::easy-simplify-term-fn term hyp
                               '(:expand (:lambdas)
                                 :in-theory (acl2::e/d* (asl-code-proof-enables)
                                                        (asl-code-proof-disables))) ;; hints
                               'iff   ;; equiv
                               t      ;; normalize
                               t      ;; rewrite
                               1000   ;; repeat
                               1000   ;; backchain-limit
                               t      ;; untrans-result
                               state))


(define subprogram-arg-hyp ((var symbolp)
                            (type ty-p)
                            hyps 
                            (local-storage-term)
                            state)
  (b* ((env-term `(change-env
                   env
                   :local (change-local-env
                           (empty-local-env)
                           :storage ,local-storage-term)))
       (type (ty-remove-parameters type))
       (is-val-term `(mv-let (res orac) (resolve-ty ,env-term ',type :clk 10000)
                       (declare (ignore orac))
                       (and (eval_result-case res :ev_normal)
                            (ty-satisfied ,var (ev_normal->res res))))))
    (simplify-for-def-asl-subprogram is-val-term `(and . ,hyps) state)))

(define subprogram-param-hyps ((params symbol-listp)
                               (fn-params maybe-typed_identifierlist-p)
                               hyps
                               (local-storage-term)
                               state)
  :guard (equal (len params) (len fn-params))
  (b* (((when (atom params)) (value (cons hyps local-storage-term)))
       ((maybe-typed_identifier p1) (car fn-params))
       (new-local-storage-term `(put-assoc-equal ,p1.name ,(car params) ,local-storage-term))
       ((unless p1.type)
        (subprogram-param-hyps (cdr params) (cdr fn-params) hyps new-local-storage-term state))
       ((er first) (subprogram-arg-hyp (car params) p1.type hyps local-storage-term state))
       ((when (eq first nil))
        (er soft 'def-asl-subprogram "Unsatisfiable parameter type: ~x0" p1)))
    (subprogram-param-hyps (cdr params) (cdr fn-params)
                           (if (eq first t)
                               hyps
                             (cons first hyps))
                           new-local-storage-term state)))


(define subprogram-arg-hyps ((args symbol-listp)
                               (fn-args maybe-typed_identifierlist-p)
                               hyps
                               (local-storage-term)
                               state)
  :guard (equal (len args) (len fn-args))
  (b* (((when (atom args)) (value (cons hyps local-storage-term)))
       ((maybe-typed_identifier p1) (car fn-args))
       (new-local-storage-term `(put-assoc-equal ,p1.name ,(car args) ,local-storage-term))
       ((er first) (subprogram-arg-hyp (car args) p1.type hyps local-storage-term state))
       ((when (eq first nil))
        (er soft 'def-asl-subprogram "Unsatisfiable parameter type: ~x0" p1)))
    (subprogram-arg-hyps (cdr args) (cdr fn-args)
                         (if (eq first t)
                             hyps
                           (cons first hyps))
                         new-local-storage-term state)))
    
    



(defconst *def-asl-subprogram-template*
  '(progn

     (local (in-theory (acl2::e/d* (<subprogram-enables>
                                    <user-enables>)
                                   (<subprogram-disables>
                                    <user-disables>))))
     (defthm <name>
       (b* (<param-bindings>
            <arg-bindings>
            <user-bindings>)
         (implies (and (subprograms-match '<subprograms>
                                          (global-env->static (env->global env))
                                          <static-env>)
                       <hyps>
                       <measure-reqs>)
                  (let* ((res (mv-nth 0 (eval_subprogram
                                         env <fn>
                                         <params>
                                         <args>)))
                         (spec (ev_normal (func_result <retvals> (env->global env)))))
                    <concl>)))
       :hints ((:@ (not :no-expand-hint)
                ("goal" :expand ((:free (params args)
                                 (eval_subprogram env <fn> params args :clk clk)))))
               <hints>))

     (table asl-subprogram-table
            <fn> (list '<subprograms>
                       '<direct-subprograms>
                       '<clk-expr>
                       '<name>))))

(define collect-direct-subprograms (body acc)
  (if (atom body)
      acc
    (case (tag body)
      (:e_call (collect-direct-subprograms
                (cdr body)
                (add-to-set-equal (call->name (ec-call (e_call->call body))) acc)))
      (:s_call (collect-direct-subprograms
                (cdr body)
                (add-to-set-equal (call->name (ec-call (s_call->call body))) acc)))
      (t (collect-direct-subprograms
          (cdr body)
          (collect-direct-subprograms (car body) acc))))))

(define collect-transitive-subprograms (lst table acc)
  (b* (((when (atom lst)) acc)
       (first (car lst))
       (acc (add-to-set-equal first acc))
       (look (cdr (hons-assoc-equal first table)))
       (acc (if look
                (union-equal (car look) acc)
              acc)))
    (collect-transitive-subprograms (cdr lst) table acc)))
    


(define maximize-const-clocks (subprogram-lst table const-acc)
  (b* (((when (atom subprogram-lst))
        const-acc)
       (first (car subprogram-lst))
       (look (cdr (hons-assoc-equal first table)))
       ((unless look)
        (maximize-const-clocks (cdr subprogram-lst) table const-acc))
       (look-clk (caddr look)))
    (maximize-const-clocks (cdr subprogram-lst) table
                           (if (integerp look-clk)
                               (max look-clk const-acc)
                             const-acc))))
    


(define cleanup-hyps (hyplist)
  (b* (((when (atom hyplist)) nil)
       (hyp (car hyplist))
       (rest (cleanup-hyps (cdr hyplist))))
    (case-match hyp
      (('and  . hyps)
       (append (cleanup-hyps hyps) rest))
      (('not ('< x y))
       (cons `(<= ,y ,x) rest))
      (& (cons hyp rest)))))


(define binding-alist-to-rev-subst (x var)
  (b* (((when (atom x)) nil)
       ((cons field acc) (car x)))
    (cons (cons `(,acc ,var)
                (intern-in-package-of-symbol
                 (concatenate 'string (symbol-name var) "." (symbol-name field))
                 'asl-pkg))
          (binding-alist-to-rev-subst (cdr x) var))))

(define val-bindings-rev-subst (x state) ;; list of bindings e.g. (((v_int n)) ((v_bitvector x)))
  (b* (((when (atom x)) nil)
       (binding (car x))
       (rest (val-bindings-rev-subst (cdr x) state)))
    (case-match binding
      (((ctor var) . &)
       (b* ((ctor-macro (intern-in-package-of-symbol
                         (concatenate 'string "PATBIND-" (symbol-name ctor))
                         ctor))
            (body (fgetprop ctor-macro 'acl2::macro-body nil (w state))))
         (case-match body
           (('std::da-patbind-fn & ('quote alist) . &)
            (append (binding-alist-to-rev-subst alist var) rest))
           (& (er hard? 'val-bindings-rev-subst "Bad ctor patbind macro ~x0" ctor-macro)))))
      (& (er hard? 'val-bindings-rev-subst "Bad binding body" binding)))))


(define sublis-subtrees (subst tree)
  (let ((pair (assoc-equal tree subst)))
    (if pair
        (cdr pair)
      (if (atom tree)
          tree
        (cons (sublis-subtrees subst (car tree))
              (sublis-subtrees subst (cdr tree)))))))
    
         


(define def-asl-subprogram-fn (name args state)
  (b* (((std::extract-keyword-args
         :other-args bad-args
         :allowed-keys '(:prepwork)
         ;; :kwd-alist kwd-alist
         function
         params
         args
         safe-clock
         
         return-values
         (hyps 't)
         
         enable
         disable
         hints
         no-expand-hint
         ;; prepwork

         ;; Either provide normal-cond/nonnormal-res,
         ;; no normal/error/throwing-conds,
         ;; or no normal-cond but error and or throwing-conds.
         normal-cond
         nonnormal-res
         error-cond
         error-res
         throwing-cond
         throwing-res
         
         bindings
         (static-env '(stdlib-static-env)))
        args)
       
       ((when bad-args)
        (er soft 'def-asl-subprogram "Bad arguments: ~x0" bad-args))
       ((unless (stringp function))
        (er soft 'def-asl-subprogram "Function should be a string: ~x0" function))
       ((acl2::er (cons & static-env-val))
        (acl2::simple-translate-and-eval static-env nil nil
                                         (msg "static env ~x0" static-env)
                                         'def-asl-subprogram (w state) state t))
       ((unless (static_env_global-p static-env-val))
        (er soft 'def-asl-subprogram "Bad static env (evaluation of ~x0): doesn't satisfy static_env_global-p" static-env))
       (fn-struct (cdr (hons-assoc-equal function
                                         (static_env_global->subprograms static-env-val))))
       ((unless fn-struct)
        (er soft 'def-asl-subprogram "Bad function ~x0: not found in static env" function))
       ((func-ses fn-struct))
       ((func f) fn-struct.fn)

       ((unless (symbol-listp params))
        (er soft 'def-asl-subprogram "Params should be a symbol-list"))
       ((unless (eql (len params) (len f.parameters)))
        (er soft 'def-asl-subprogram "~x0 params were given but ~s1 has ~x2 parameters" (len params) function (len f.parameters)))
       ((unless (symbol-listp args))
        (er soft 'def-asl-subprogram "Args should be a symbol-list"))
       ((unless (eql (len args) (len f.args)))
        (er soft 'def-asl-subprogram "~x0 args were given but ~s1 has ~x2 args" (len args) function (len f.args)))

       (param-bindings (subprogram-param-bindings params f.parameters))
       (arg-bindings (set-difference-equal (subprogram-arg-bindings args f.args) param-bindings))
       (binding-subst (val-bindings-rev-subst (append param-bindings arg-bindings) state))

       ((er hyps) (simplify-for-def-asl-subprogram `(b* (,@param-bindings ,@arg-bindings) ,hyps) t state))
       (hyp-list (reverse (cleanup-hyps (list hyps))))
       ((er (cons hyp-list storage-term)) (subprogram-param-hyps params f.parameters hyp-list 'env.local.storage state))
       ((er (cons hyp-list &)) (subprogram-arg-hyps args f.args hyp-list storage-term state))
       (hyps (sublis-subtrees binding-subst (cleanup-hyps (reverse hyp-list))))

       (direct-subprograms (collect-direct-subprograms f.body nil))
       (table  (table-alist 'asl-subprogram-table (w state)))
       (subprograms (cons function (collect-transitive-subprograms direct-subprograms table nil)))

       (clk-val
        (or safe-clock
            (+ 1 (maximize-const-clocks direct-subprograms table -1))))

       (measure-reqs (if (eql clk-val 0)
                         t
                       `(<= ,clk-val (ifix clk))))

       (concl (if normal-cond
                  (if nonnormal-res
                      `(equal res (if ,normal-cond
                                      spec
                                    ,nonnormal-res))
                    `(and (implies ,normal-cond (equal res spec))
                          (implies (not ,normal-cond)
                                   (not (equal (eval_result-kind res) :ev_normal)))))
                (if error-cond
                    (if error-res
                        `(equal res
                                (cond (,error-cond ,error-res)
                                      ,@(and throwing-cond
                                             `((,throwing-cond ,throwing-res)))
                                      (t spec)))
                      (let ((error-concl `(implies ,error-cond
                                                   ,(if error-res
                                                        `(equal res ,error-res)
                                                      '(equal (eval_result-kind res) :ev_error)))))
                        (if throwing-cond
                            `(and ,error-concl
                                  (implies ,throwing-cond
                                           (equal res ,throwing-res))
                                  (implies (and (not ,error-cond)
                                                (not ,throwing-cond))
                                           (equal res spec)))
                          `(and ,error-concl
                                (implies (not ,error-cond)
                                         (equal res spec))))))
                  (if throwing-cond
                      `(equal res
                              (if ,throwing-cond
                                  ,throwing-res
                                spec))
                    '(equal res spec)))))

       (template (acl2::make-tmplsubst
                  :atoms `((<name> . ,name)
                           (<subprograms> . ,subprograms)
                           (<direct-subprograms> . ,direct-subprograms)
                           (<static-env> . ,static-env)
                           (<measure-reqs> . ,measure-reqs)
                           (<fn> . ,function)
                           (<clk-expr> . ,clk-val)
                           (<params> . (list . ,params))
                           (<args>   . (list . ,args))
                           (<retvals> . (list . ,return-values))
                           (<concl> . ,concl))
                  :splices `((<subprogram-enables> . (asl-code-proof-enables))
                             (<subprogram-disables> . (asl-code-proof-disables))
                             (<user-enables> . ,enable)
                             (<user-disables> . ,disable)
                             (<param-bindings> . ,param-bindings)
                             (<arg-bindings> . ,arg-bindings)
                             (<user-bindings> . ,bindings)
                             (<hyps> . ,hyps)
                             (<hints> . ,hints))
                  :features (and no-expand-hint '(:no-expand-hint)))))

    (value (acl2::template-subst-top *def-asl-subprogram-template* template))))

(defmacro def-asl-subprogram (name &rest args)
  (let* ((prepwork (cadr (assoc-keyword :prepwork args))))
    `(defsection ,name
       ,@prepwork
       (make-event (def-asl-subprogram-fn ',name ',args state)))))


                             
                             
                             

       
       
       
        







(acl2::def-ruleset! asl-code-proof-enables
  '(check_recurse_limit
    declare_local_identifiers
    declare_local_identifier
    remove_local_identifier
    env-find
    env-assign
    env-assign-local
    env-assign-global
    env-push-stack
    env-pop-stack
    pop_scope
    tick_loop_limit
    v_to_bool
    eval_for_step
    for_loop-step
    for_loop-test
    check-bad-slices
    slices_sub
    check_non_overlapping_slices
    check_non_overlapping_slices-1
    slices-width
    write_to_bitvector
    write_to_bitvector-aux
    vbv-to-int
    v_to_int))

(acl2::def-ruleset asl-code-proof-disables nil)
