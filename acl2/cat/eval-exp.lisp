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

(include-book "built-in-ops")
(include-book "centaur/fty/multicase" :dir :system)
(local (include-book "std/basic/arith-equivs" :dir :System))
(local (std::add-default-post-define-hook :fix))

(defprod errmsg
  ((name stringp)
   (msg))
  :layout :list)

(defmacro norm (&rest args)
  `(mv nil . ,args))

(defmacro err (name msg &key (nvals '1))
  `(mv (errmsg ,name ,msg) . ,(make-list nvals)))


(acl2::def-b*-binder expval
  :parents (cat-interpreter-functions)
  :short "Binds some number of values preceded by an error, returning @('(mv err nil)')
if it is an error, otherwise continuing the computation."
  :body
  `(b* (((mv err . ,acl2::args) . ,acl2::forms))
     (if err
         (mv err nil)
       ,acl2::rest-expr)))

(acl2::def-b*-binder expval2
  :parents (cat-interpreter-functions)
  :short "Binds some number of values preceded by an error, returning @('(mv err nil nil)')
if it is an error, otherwise continuing the computation."
  :body
  `(b* (((mv err . ,acl2::args) . ,acl2::forms))
     (if err
         (mv err nil nil)
       ,acl2::rest-expr)))


(define special-vars ()
  :returns (specials varlist-p)
  '("0" "B" "ext" "F" "id" "IW" "loc" "M" "narrower" "po" "R" "rf" "rmw" "W" "wider")
  ///
  (in-theory (disable (special-vars)))) 

(define eval-var-special ((v var-p)
                          (ex execgraph-p))
  :guard (member-equal v (special-vars))
  :prepwork ((local (in-theory (enable (special-vars)))))
  :returns (mv err (val (iff (val-p val) (not err))))
  (b* ((v (var-fix v))
       (events (execgraph->evts ex))
       ((when (equal v "0"))
        (norm (v_rel nil))) ;; empty relation
       ((when (equal v "ext"))
        (norm (v_rel (ext-relation events))))
       ((when (equal v "id"))
        (norm (v_rel (id-relation events))))
       ((when (equal v "loc"))
        (norm (v_rel (loc-relation events))))
       ((when (equal v "po"))
        (norm (v_rel (po-relation events))))
       ((when (equal v "rf"))
        (norm (v_rel (rf-relation events))))
       ((when (equal v "B"))
        (norm (v_set (evtlist-filter-type :evt-b events))))
       ((when (equal v "F"))
        (norm (v_set (evtlist-filter-type :evt-f events))))
       ((when (equal v "R"))
        (norm (v_set (evtlist-filter-type :evt-r events))))
       ((when (equal v "W"))
        (norm (v_set (evtlist-filter-type :evt-w events))))
       ((when (equal v "M"))
        (norm (v_set (append (evtlist-filter-type :evt-w events)
                             (evtlist-filter-type :evt-r events)))))
       ((when (equal v "IW"))
        (norm (v_set (evtlist-filter-init-writes events)))))
    (err "Unimplemented" (msg "special variable ~s0" v))))


(define eval-var ((v var-p)
                  (ex execgraph-p)
                  (env env-p))
  :returns (mv err (val (iff (val-p val) (not err))))
  (b* ((v (var-fix v))
       ((unless (member-equal v (special-vars)))
        (b* ((val (env-lookup v env))
             ((unless val)
              (err "Unbound var" v)))
          (val-case val
            :v_enum (err "Unimplemented" "Special case for enum vars")
            :otherwise (norm val)))))
    (eval-var-special v ex)))





(define eval-op1 ((op op1-p) (arg val-p))
  (declare (ignorable arg))
  :returns (mv err (val (iff (val-p val) (not err))))
  (b* ((op (op1-fix op)))
    (case op
      (:inv (val-case arg
              :v_rel (norm (v_rel (inverse arg.rel)))
              :otherwise (err "Unsupported"
                              (msg "unary operator ~x0 on ~x1 type" op (val-kind arg)))))
      (:toid (val-case arg
               :v_set (norm (v_rel (id-relation arg.evts)))
               :otherwise (err "Unsupported"
                               (msg "unary operator ~x0 on ~x1 type" op (val-kind arg)))))
      (:plus (val-case arg
               :v_rel (norm (v_rel (transitive-closure arg.rel)))
               :otherwise (err "Unsupported"
                               (msg "unary operator ~x0 on ~x1 type" op (val-kind arg)))))
      (otherwise ;; (:plus :star :opt :comp :inv :toid)
       (err "Unimplemented" (msg "unary operator ~x0" op))))))


(local (defthm evtlist-p-of-set-difference
         (implies (evtlist-p x)
                  (evtlist-p (set-difference-equal x y)))))

(local (defthm vallist-p-of-set-difference
         (implies (vallist-p x)
                  (vallist-p (set-difference-equal x y)))))

(local (defthm vallist-p-of-union
         (implies (and (vallist-p x)
                       (vallist-p y))
                  (vallist-p (union-equal x y)))))

(local (defthm evtlist-p-of-union
         (implies (and (evtlist-p x)
                       (evtlist-p y))
                  (evtlist-p (union-equal x y)))))

(local (defthm vallist-p-of-intersection
         (implies (and (vallist-p x))
                  (vallist-p (intersection-equal x y)))))

(local (defthm evtlist-p-of-intersection
         (implies (and (evtlist-p x))
                  (evtlist-p (intersection-equal x y)))))

(define eval-op2 ((op op2-p) (args vallist-p))
  :returns (mv err (val (iff (val-p val) (not err))))
  :measure (len args)
  :verify-guards nil
  (b* ((op (op2-fix op)))
    (case op
      (:cartesian (cond ((and (eql (len args) 2)
                              (val-case (car args) '(:v_set :v_empty))
                              (val-case (cadr args) '(:v_set :v_empty)))
                         (if (or (val-case (car args) :v_empty)
                                 (val-case (cadr args) :v_empty))
                             (norm (v_empty))
                           (norm (v_rel (cartesian-product (v_set->evts (car args))
                                                           (v_set->evts (cadr args)))))))
                        (t (err "Bad operator arguments"
                                (msg "op: ~x0 args: ~x1" op (vallist-fix args))))))
      (:seq (b* (((When (atom args))
                  (err "Bad operator arguments"
                       (msg "op: ~x0 args: ~x1" op (vallist-fix args))))
                 (arg1 (car args))
                 ((when (val-case arg1 :v_empty))
                  (norm (v_empty)))
                 ((unless (val-case arg1 :v_rel))
                  (err "Bad operator arguments" (msg "op: ~x0 arg: ~x1" :seq (val-fix (car args)))))
                 ((when (atom (cdr args)))
                  (norm (val-fix arg1)))
                 ((expval rest) (eval-op2 :seq (cdr args)))
                 ((v_rel arg1)))
              (val-case rest
                :v_empty (norm (v_empty))
                :v_rel (norm (v_rel (compose arg1.rel rest.rel)))
                :otherwise (err "Impossible" "seq op returned non-rel value"))))
      (:diff (b* (((unless (eql (len args) 2))
                   (err "Bad operator arguments"
                        (msg "op: ~x0 args: ~x1" op (vallist-fix args))))
                  (arg1 (car args))
                  (arg2 (cadr args)))
               (fty::multicase
                 ((val-case arg1)
                  (val-case arg2))
                 ((:v_empty -) (norm (val-fix arg1)))
                 ((:v_rel :v_empty) (norm (val-fix arg1)))
                 ((:v_set :v_empty) (norm (val-fix arg1)))
                 ((:v_valset :v_empty) (norm (val-fix arg1)))
                 ((:v_rel :v_rel) (norm (v_rel (difference arg1.rel arg2.rel))))
                 ((:v_set :v_set) (norm (v_set (set-difference-equal arg1.evts arg2.evts))))
                 ((:v_valset :v_valset) (norm (v_valset (set-difference-equal arg1.elts arg2.elts))))
                 (- (err "Bad operator arguments"
                         (msg "op: ~x0 args: ~x1" op (vallist-fix args)))))))
      (:union (b* (((when (atom args))
                    (err "Bad operator arguments"
                         (msg "op: ~x0 args: ~x1" op (vallist-fix args))))
                   (arg1 (car args))
                   ((when (atom (cdr args)))
                    (if (val-case arg1 '(:v_empty :v_set :v_valset :v_rel))
                        (norm (val-fix arg1))
                      (err "Bad operator arguments"
                           (msg "op: ~x0 args: ~x1" op (vallist-fix args)))))
                   ((expval rest) (eval-op2 :union (cdr args))))
                (fty::multicase
                  ((val-case arg1)
                   (val-case rest))
                 ((:v_empty -) (norm rest))
                 ((:v_rel :v_empty) (norm (val-fix arg1)))
                 ((:v_set :v_empty) (norm (val-fix arg1)))
                 ((:v_valset :v_empty) (norm (val-fix arg1)))
                 ((:v_rel :v_rel) (norm (v_rel (union arg1.rel rest.rel))))
                 ((:v_set :v_set) (norm (v_set (union-equal arg1.evts rest.evts))))
                 ((:v_valset :v_valset) (norm (v_valset (union-equal arg1.elts rest.elts))))
                 (- (err "Bad operator arguments"
                         (msg "op: ~x0 args: ~x1" op (vallist-fix args)))))))
      
      (:inter (b* (((when (atom args))
                    (err "Bad operator arguments"
                         (msg "op: ~x0 args: ~x1" op (vallist-fix args))))
                   (arg1 (car args))
                   ((when (atom (cdr args)))
                    (if (val-case arg1 '(:v_empty :v_set :v_valset :v_rel))
                        (norm (val-fix arg1))
                      (err "Bad operator arguments"
                           (msg "op: ~x0 args: ~x1" op (vallist-fix args)))))
                   ((expval rest) (eval-op2 :inter (cdr args))))
                (fty::multicase
                  ((val-case arg1)
                   (val-case rest))
                 ((:v_empty -) (norm (val-fix arg1)))
                 ((:v_rel :v_empty) (norm rest))
                 ((:v_set :v_empty) (norm rest))
                 ((:v_valset :v_empty) (norm rest))
                 ((:v_rel :v_rel) (norm (v_rel (intersect arg1.rel rest.rel))))
                 ((:v_set :v_set) (norm (v_set (intersection-equal arg1.evts rest.evts))))
                 ((:v_valset :v_valset) (norm (v_valset (intersection-equal arg1.elts rest.elts))))
                 (- (err "Bad operator arguments"
                         (msg "op: ~x0 args: ~x1" op (vallist-fix args)))))))
      (:tuple (b* (((when (eql (len args) 1))
                    (err "Bad operator arguments"
                         (msg "op: ~x0 args: ~x1" op (vallist-fix args)))))
                (norm (v_tuple args))))
                   
      (otherwise ;; (:union :inter :diff :add :tuple)
       (err "Unimplemented" (msg "operator ~x0" op)))))
  ///
  (verify-guards eval-op2)
  (local (in-theory (enable vallist-fix))))
    


(define primitive-p ((x exp-p))
  (exp-case x
    :e_var (and (member-equal x.var '("classes" "linearisations" "fromto" "tag2events" "tag2scope")) t)
    :otherwise nil))

(define eval-primitive ((x exp-p) (v val-p))
  (declare (ignorable v))
  :guard (primitive-p x)
  :guard-hints (("goal" :in-theory (enable primitive-p)))
  :returns (mv err (val (iff (val-p val) (not err))))
  (b* (((e_var x))
       (fn x.var)
       ((when (equal fn "classes"))
        (err "Unimplemented" (msg "primitive ~s0~%" fn))))
    (err "Unimplemented" (msg "primitive ~s0~%" fn))))


(define pair-pat0-tuple ((p pat0-list-p) (v vallist-p))
  :guard (eql (len p) (len v))
  :returns (env env-p)
  (if (atom p)
      nil
    (if (car p)
        (env-update (car p) (car v)
                    (pair-pat0-tuple (cdr p) (cdr v)))
      (pair-pat0-tuple (cdr p) (cdr v)))))

(define match-pat ((p pat-p) (v val-p))
  :returns (mv err (env env-p))
  (pat-case p
    :pvar (norm (and p.var
                     (env-update p.var v nil)))
    :ptuple (val-case v
              :v_tuple (if (eql (len v.elts) (len p.tuple))
                           (norm (pair-pat0-tuple p.tuple v.elts))
                         (err "Bad pattern binding" (msg "pat: ~x0 val: ~x1" (pat-fix p) (val-fix v))))
              :otherwise (err "Bad pattern binding" (msg "pat: ~x0 val: ~x1" (pat-fix p) (val-fix v))))))

(define check-fixpoint-binding ((v var-p) (val val-p) (env env-p))
  :returns (mv err changedp)
  (b* ((v (var-fix v))
       ((expval prev-val) (b* ((look (env-lookup v env))
                               ((unless look)
                                (err "Unbound var" v)))
                            (norm look))))
    (val-case val
      :v_empty (val-case prev-val
                 :v_empty (norm nil)
                 :otherwise (err "Fixpoint error" (msg "binding of ~s0 got smaller" v)))
      :v_set (val-case prev-val
               :v_empty (norm (consp val.evts))
               :v_set (cond ((subsetp-equal prev-val.evts val.evts)
                             (if (subsetp-equal val.evts prev-val.evts)
                                 (norm nil)
                               (norm t)))
                            (t (err "Fixpoint error"
                                    (msg "binding of ~s0 got smaller" v))))
               :otherwise (err "Fixpoint error"
                               (msg "type of ~s0 changed" v)))
      ;; :v_valset (val-case prev-val
      ;;             :v_empty (norm (consp val.elts))
      ;;             :v_valset (cond ((subsetp-equal prev-val.elts val.elts)
      ;;                              (if (subsetp-equal val.elts prev-val.elts)
      ;;                                  (norm nil)
      ;;                                (norm t)))
      ;;                             (t (err "Fixpoint error"
      ;;                                     (msg "binding of ~s0 got smaller" v))))
      ;;             :otherwise (err "Fixpoint error"
      ;;                             (msg "type of ~s0 changed" v)))
      :v_rel (val-case prev-val
               :v_empty (norm (not (emptyp val.rel)))
               :v_rel (if (subset val.rel prev-val.rel)
                          (norm nil)
                        (norm t))
               ;; (cond ((subset prev-val.rel val.rel)
               ;;               (if (subset val.rel prev-val.rel)
               ;;                   (norm nil)
               ;;                 (norm t)))
               ;;              (t (err "Fixpoint error"
               ;;                      (msg "binding of ~s0 got smaller" v))))
               :otherwise (err "Fixpoint error"
                               (msg "type of ~s0 changed" v)))
      :otherwise (err "Fixpoint error"
                      (msg "bad value type ~x0" (val-kind val))))))
                              
         
      
    
  
      
    

(define fndeflist-closure-bindings ((x fndeflist-p) (all-recdefs fndeflist-p) (env env-p))
  :returns (closure-env env-p)
  (if (atom x)
      nil
    (b* (((fndef x1) (car x)))
      (env-update
       x1.name (v_fun x1.formals x1.body all-recdefs env)
       (fndeflist-closure-bindings (cdr x) all-recdefs env)))))

(define fndeflist->names ((x fndeflist-p))
  :returns (names varlist-p)
  (if (atom x)
      nil
    (cons (fndef->name (car x))
          (fndeflist->names (cdr x)))))
                  
(define function-bindings-p ((x bindinglist-p))
  (if (atom x)
      t
    (and (b* (((binding x1) (car x)))
           (and (pat-case x1.pat :pvar x1.pat.var :otherwise nil)
                (exp-case x1.exp :e_fun)))
         (function-bindings-p (cdr x)))))

(define function-bindings-to-fndefs ((x bindinglist-p))
  :guard (function-bindings-p x)
  :guard-hints (("goal" :in-theory (enable function-bindings-p)))
  :returns (fns fndeflist-p)
  (b* (((when (atom x)) nil)
       ((binding x1) (car x))
       ((pvar x1.pat))
       ((e_fun x1.exp)))
    (cons (fndef x1.pat.var x1.exp.formals x1.exp.body)
          (function-bindings-to-fndefs (cdr x)))))


(defthm varlist-p-of-set-difference
  (implies (varlist-p x)
           (varlist-p (set-difference-equal x y))))

(define recursive-function-bindings-to-closures-aux ((x bindinglist-p)
                                                     (defs fndeflist-p)
                                                     (env env-p))
  :guard (function-bindings-p x)
  :guard-hints (("goal" :in-theory (enable function-bindings-p)))
  :returns (rec-env env-p)
  :verify-guards nil
  (if (atom x)
      nil
    (b* (((binding x1) (car x))
         ((pvar x1.pat))
         ((e_fun x1.exp)))
      (env-update x1.pat.var (v_fun x1.exp.formals x1.exp.body defs
                                    (env-extract (set-difference-equal x1.exp.free (fndeflist->names defs))
                                                 env))
                  (recursive-function-bindings-to-closures-aux (cdr x) defs env))))
  ///
  (verify-guards recursive-function-bindings-to-closures-aux))

(define recursive-function-bindings-to-closures ((x bindinglist-p) (env env-p))
  :guard (function-bindings-p x)
  :returns (rec-env env-p)
  (b* ((fndefs (function-bindings-to-fndefs x)))
    (recursive-function-bindings-to-closures-aux x fndefs env)))

(define add-empty-bindings-to-env ((x bindinglist-p) (env env-p))
  :returns (new-env env-p)
  :verify-guards nil
  (if (atom x)
      (env-fix env)
    (b* (((binding x1) (car x))
         ;; Note: we don't support tuple bindings in recursive relations/sets
         (var (and (pat-case x1.pat :pvar)
                   (pvar->var x1.pat))))
      (if var
          (env-update var (v_empty)
                      (add-empty-bindings-to-env (cdr x) env))
        (add-empty-bindings-to-env (cdr x) env))))
  ///
  (verify-guards add-empty-bindings-to-env))

(define eval-binary-condition ((x condition-p) (val1 val-p) (val2 val-p))
  :guard (not (condition-case x :cond_variant))
  :returns (mv err (res booleanp))
  (condition-case x
      :cond_in
    (val-case val2
      :v_valset (norm (and (member-equal (val-fix val1) val2.elts) t))
      :otherwise (err "Unsupported" (msg "member check with non-valset arg: ~x0"
                                         (val-kind val2))))
    :cond_eq
    (fty::multicase
      ((val-case val1)
       (val-case val2))
      ((:v_empty :v_empty)   (norm t))
      ((:v_empty :v_rel)     (norm (not (consp val2.rel))))
      ((:v_empty :v_set)     (norm (not (consp val2.evts))))
      ((:v_empty :v_valset)  (norm (not (consp val2.elts))))
      ((:v_rel :v_empty)     (norm (not (consp val1.rel))))
      ((:v_set :v_empty)     (norm (not (consp val1.evts))))
      ((:v_valset :v_empty)  (norm (not (consp val1.elts))))
      ((:v_rel :v_rel)       (norm (acl2::set-equiv val1.rel val2.rel)))
      ((:v_set :v_set)       (norm (acl2::set-equiv val1.evts val2.evts)))
      ((:v_valset :v_valset) (norm (acl2::set-equiv val1.elts val2.elts)))
      ((:v_tag :v_tag)       (norm (equal val1.tag val2.tag)))
      (-                     (err "Unsupported" (msg "equality comparison of ~x0, ~x1"
                                                     (val-kind val1) (val-kind val2)))))
    :otherwise ;; :cond_subset
    (fty::multicase
      ((val-case val1)
       (val-case val2))
      ((:v_empty :v_empty)   (norm t))
      ((:v_empty :v_rel)     (norm t))
      ((:v_empty :v_set)     (norm t))
      ((:v_empty :v_valset)  (norm t))
      ((:v_rel :v_empty)     (norm (not (consp val1.rel))))
      ((:v_set :v_empty)     (norm (not (consp val1.evts))))
      ((:v_valset :v_empty)  (norm (not (consp val1.elts))))
      ((:v_rel :v_rel)       (norm (subsetp-equal val1.rel val2.rel)))
      ((:v_set :v_set)       (norm (subsetp-equal val1.evts val2.evts)))
      ((:v_valset :v_valset) (norm (subsetp-equal val1.elts val2.elts)))
      (-                     (err "Unsupported" (msg "subset comparison of ~x0, ~x1"
                                                     (val-kind val1) (val-kind val2)))))))





(with-output
   ;; makes it so it won't take forever to print the induction scheme
   :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
   :off (event)
   (defines eval-exp
     :flag-local nil
     (define eval-exp ((x exp-p)
                       &key
                       ((ex execgraph-p) 'ex)
                       ((env env-p) 'env)
                       ((reclimit natp) 'reclimit))
       :returns (mv err (res (iff (val-p res) (not err))))
       :measure (acl2::nat-list-measure (list reclimit (exp-count x) 10))
       ;; :measure-debug t
       :verify-guards nil
       (exp-case x
         :e_konst (err "Unimplemented" "konst expression")
         :e_tag (norm (v_tag x.val))
         :e_var (eval-var x.var ex env)
         :e_op1 (b* (((expval v) (eval-exp x.arg)))
                  (eval-op1 x.op v))
         :e_op (b* (((expval args) (eval-explist x.args)))
                 (eval-op2 x.op args))
         :e_app (b* (((when (primitive-p x.fn))
                      (b* (((expval arg) (eval-exp x.arg)))
                        (eval-primitive x.fn arg)))
                     ((expval fn) (eval-exp x.fn)))
                  (val-case fn
                    :v_fun (b* (((expval arg) (eval-exp x.arg))
                                ((expval argenv) (match-pat fn.formals arg))
                                (rec-bindings (fndeflist-closure-bindings fn.recdefs fn.recdefs fn.env))
                                ((when (zp reclimit))
                                 (err "Hit recursion limit"
                                      (msg "in call of" x.fn))))
                             (eval-exp fn.body :env (env-combine argenv (env-combine rec-bindings fn.env)) :reclimit (1- reclimit)))
                    :otherwise (err "Bad function call" (msg "unexpected function value ~x0" fn))))
         :e_bind (b* (((expval env) (eval-bindings x.bindings)))
                   (eval-exp x.body))
         :e_bindrec
         (if (function-bindings-p x.bindings)
             (b* ((fn-env (recursive-function-bindings-to-closures x.bindings env)))
               (eval-exp x.body :env (env-combine fn-env env)))
           (b* ((env (add-empty-bindings-to-env x.bindings env))
                ((expval env) (eval-fixpoint x.bindings)))
             (eval-exp x.body)))
         :e_fun (norm (v_fun x.formals x.body nil (env-extract x.free env)))
         :e_explicitset (b* (((expval vals) (eval-explist x.elems)))
                          (norm (v_valset vals)))
         :e_match (err "Unimplemented" "match expression")
         :e_matchset (err "Unimplemented" "matchset expression")
         :e_try (err "Unimplemented" "try expression")
         :e_if (b* (((expval test) (eval-condition x.test)))
                 (if test
                     (eval-exp x.then)
                   (eval-exp x.else)))))

     (define eval-explist ((x explist-p)
                           &key
                           ((ex execgraph-p) 'ex)
                           ((env env-p) 'env)
                           ((reclimit natp) 'reclimit))
       :returns (mv err (res vallist-p))
       :measure (acl2::nat-list-measure (list reclimit (explist-count x) 10))
       (b* (((when (atom x)) (norm nil))
            ((expval first) (eval-exp (car x)))
            ((expval rest) (eval-explist (cdr x))))
         (norm (cons first rest))))

     (define eval-bindings ((x bindinglist-p)
                            &key
                            ((ex execgraph-p) 'ex)
                            ((env env-p) 'env)
                            ((reclimit natp) 'reclimit))
       :measure (acl2::nat-list-measure (list reclimit (bindinglist-count x) 10))
       :returns (mv err (res env-p))
       (b* (((when (atom x)) (norm (env-fix env)))
            ((expval env1) (eval-binding (car x)))
            ((expval env2) (eval-bindings (cdr x))))
         (norm (env-combine env1 env2))))

     (define eval-binding ((x binding-p)
                           &key
                           ((ex execgraph-p) 'ex)
                           ((env env-p) 'env)
                           ((reclimit natp) 'reclimit))
       :returns (mv err (res env-p))
       :measure (acl2::nat-list-measure (list reclimit (binding-count x) 10))
       (b* (((binding x))
            ((when (pat-case x.pat :pvar (not x.pat.var) :otherwise nil))
             (norm nil))
            ((expval val) (eval-exp x.exp))
            ((expval bindings) (match-pat x.pat val)))
         (norm bindings)))


     (define eval-fixpoint ((x bindinglist-p)
                            &key
                            ((ex execgraph-p) 'ex)
                            ((env env-p) 'env)
                            ((reclimit natp) 'reclimit))
       :measure (acl2::nat-list-measure (list reclimit (bindinglist-count x) 10))
       :returns (mv err (res env-p))
       (b* (((expval new-env changedp) (eval-fixpoint1 x))
            ((unless changedp) (norm new-env))
            ((when (zp reclimit))
             (err "Hit recursion limit" "in eval-fixpoint")))
         (eval-fixpoint x :env new-env :reclimit (1- reclimit))))

     (define eval-fixpoint1 ((x bindinglist-p)
                             &key
                             ((ex execgraph-p) 'ex)
                             ((env env-p) 'env)
                             ((reclimit natp) 'reclimit))
       :measure (acl2::nat-list-measure (list reclimit (bindinglist-count x) 8))
       :returns (mv err (res env-p) changedp)
       (b* (((when (atom x)) (norm (env-fix env) nil))
            ((expval2 env changedp1) (eval-fixpoint-binding (car x)))
            ((expval2 env changedp2) (eval-fixpoint1 (cdr x))))
         (norm env (or changedp1 changedp2))))

     (define eval-fixpoint-binding ((x binding-p)
                                    &key
                                    ((ex execgraph-p) 'ex)
                                    ((env env-p) 'env)
                                    ((reclimit natp) 'reclimit))
       :measure (acl2::nat-list-measure (list reclimit (binding-count x) 10))
       :returns (mv err (res env-p) changedp)
       (b* (((binding x))
            ((unless (pat-case x.pat :pvar))
             (err "Unimplemented" "Recursive binding of tuple" :nvals 2))
            ((pvar x.pat))
            ((unless x.pat.var)
             (norm (env-fix env) nil))
            ((expval2 val) (eval-exp x.exp))
            ((expval2 changedp)
             (check-fixpoint-binding x.pat.var val env))
            (env (env-update x.pat.var val env)))
         (norm env changedp)))

     (define eval-condition ((x condition-p)
                             &key
                             ((ex execgraph-p) 'ex)
                             ((env env-p) 'env)
                             ((reclimit natp) 'reclimit))
       :measure (acl2::nat-list-measure (list reclimit (condition-count x) 10))
       :returns (mv err (res booleanp))
       (condition-case x
           :cond_variant (err "Unimplemented" "variant condition")
           :otherwise
           (b* (((mv arg1 arg2) (condition-case x
                                    :cond_eq     (mv x.arg1 x.arg2)
                                    :cond_subset (mv x.arg1 x.arg2)
                                    :otherwise   (mv (cond_in->arg1 x)
                                                     (cond_in->arg2 x))))
                ((expval val1) (eval-exp arg1))
                ((expval val2) (eval-exp arg2)))
             (eval-binary-condition x val1 val2))))
     ///
     (verify-guards eval-exp-fn)

     (fty::deffixequiv-mutual eval-exp)))


          
        
