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
(include-book "centaur/fgl/congruence-rules" :dir :system)
(include-book "centaur/fgl/config" :dir :system)
(include-book "interp-types" :dir :acl2asl)
(include-book "ty-utils" :dir :acl2asl)
(include-book "proof-utils" :dir :acl2asl) ;; for termination-error-p
(local (std::Add-default-post-define-hook :fix))


(defmacro define-continue (name &rest events)
  `(defsection ,name
     ;; :extension ,name
     (local (std::set-define-current-function ,name))
     (local (in-theory (enable ,name)))
     . ,events))


(define v_array-len ((x val-p))
  :guard (val-case x :v_array)
  (len (v_array->arr x)))


(define v_array-nth ((n natp) (x val-p))
  :guard (and (val-case x :v_array)
              (< n (v_array-len x)))
  :prepwork ((local (in-theory (enable v_array-len))))
  :returns (val val-p)
  (val-fix (nth n (v_array->arr x))))

(define v_array-update-nth ((n natp) (v val-p) (x val-p))
  :guard (and (val-case x :v_array)
              (< n (v_array-len x)))
  :prepwork ((local (in-theory (enable v_array-len))))
  :returns (new-x val-p)
  :hooks ((:fix :omit (v)))
  (v_array (update-nth n v (v_array->arr x))))

(define v_array-update-nths (lst x)
  :verify-guards nil
  :returns (new-x val-p)
  (b* (((when (atom lst)) (v_array (v_array->arr x)))
       ((unless (and (consp (car lst))
                     (natp (caar lst))
                     (< (caar lst) (v_array-len x))))
        (v_array-update-nths (cdr lst) x)))
    (v_array-update-nth (caar lst) (cdar lst)
                        (v_array-update-nths (cdr lst) x))))

(define val-imap-lookup ((key identifier-p) (x val-imap-p))
  :verify-guards nil
  :returns (val val-p)
  (val-fix (omap::lookup (identifier-fix key) (val-imap-fix x))))

(define val-imap-has-key ((key identifier-p) (x val-imap-p))
  :verify-guards nil
  (and (omap::assoc (identifier-fix key) (val-imap-fix x)) t))

(define val-imap-put ((key identifier-p) (val val-p) (x val-imap-p))
  :verify-guards nil
  :returns (new-x val-imap-p)
  (if (val-imap-has-key key x)
      (omap::update (identifier-fix key) (val-fix val) (val-imap-fix x))
    (val-imap-fix x)))

(define val-imap-add ((key identifier-p) (val val-p) (x val-imap-p))
  :verify-guards nil
  :returns (new-x val-imap-p)
  (omap::update (identifier-fix key) (val-fix val)
                (val-imap-fix x)))

(define val-imap-add-pairs (pairs x)
  :verify-guards nil
  :returns (new-x val-imap-p)
  (if (atom pairs)
      (val-imap-fix x)
    (if (and (consp (car pairs))
             (identifier-p (caar pairs)))
        (val-imap-add (caar pairs) (cdar pairs)
                      (val-imap-add-pairs (cdr pairs) x))
      (val-imap-add-pairs (cdr pairs) x))))

(define val-imap-put-pairs (pairs (x val-imap-p))
  :verify-guards nil
  :returns (new-x val-imap-p)
  (if (atom pairs)
      (val-imap-fix x)
    (if (and (consp (car pairs))
             (identifier-p (caar pairs)))
        (val-imap-put (caar pairs) (cdar pairs)
                      (val-imap-put-pairs (cdr pairs) x))
      (val-imap-put-pairs (cdr pairs) x))))

  

(define bind-ty-satisfied (ty x)
  :verify-guards nil
  (if (ty-satisfied x ty)
      ty
    nil)
  ///
  (fgl::remove-fgl-rewrite bind-ty-satisfied)

  (fgl::def-fgl-brewrite bind-ty-satisfied-fallback
    (implies (equal ty nil)
             (equal (bind-ty-satisfied ty x) ty))))


(define fgl-mark (name x)
  :enabled t
  (declare (ignore name))
  x
  ///
  (defcong iff iff (fgl-mark name x) 2)
  (fgl::add-fgl-congruence iff-implies-iff-fgl-mark-2))



(defun printed-annotation-index (annot)
  (cadr (assoc-keyword :printed annot)))


(define ty-global_decl_keyword-imap-resolve-names ((types ty-global_decl_keyword-imap-p)
                                                   (static_env static_env_global-p))
  :returns (res (and (eval_result-p res)
                     (implies (eval_result-case res :ev_normal)
                              (ty-global_decl_keyword-imap-p (ev_normal->res res)))))
  :hooks nil
  (if (atom types)
      (ev_normal nil)
    (if (mbt (and (consp (car types))
                  (identifier-p (caar types))))
        (b* (((ev first) (name-resolve-ty static_env (ty-global_decl_keyword->ty (cdar types)) :clk 1000))
             ;; ((ev first)
             ;;  (b* (((acl2::local-stobjs orac) (mv val orac))
             ;;       (env (init-env)))
             ;;    (resolve-ty env (ty-global_decl_keyword->ty (cdar types)) :clk 1000)))
             ((ev rest) (ty-global_decl_keyword-imap-resolve-names (cdr types) static_env)))
          (ev_normal (cons (cons (caar types) (change-ty-global_decl_keyword (cdar types) :ty first)) rest)))
      (ty-global_decl_keyword-imap-resolve-names (cdr types) static_env))))

(define ty-global_decl_keyword-imap-resolve ((x ty-global_decl_keyword-imap-p)
                                             (env env-p)
                                             (orac))
  :returns (mv (res (and (eval_result-p res)
                         (implies (eval_result-case res :ev_normal)
                                  (ty-global_decl_keyword-imap-p (ev_normal->res res)))))
               new-orac)
  (if (atom x)
      (evo_normal nil)
    (if (mbt (and (consp (car x))
                  (identifier-p (caar x))))
        (b* (((evoo first)
              (resolve-ty env (ty-global_decl_keyword->ty (cdar x))
                          :clk 1000))
             ((evoo rest)
              (ty-global_decl_keyword-imap-resolve (cdr x) env orac)))
          (evo_normal (cons (cons (caar x) (change-ty-global_decl_keyword (cdar x) :ty first))
                            rest)))
      (ty-global_decl_keyword-imap-resolve (cdr x) env orac)))
  ///
  (local (in-theory (enable ty-global_decl_keyword-imap-fix))))


(define resolve-storage-types ((config-env env-p))
  :returns (types ty-global_decl_keyword-imap-p)
  :hooks nil
  (b* (((static_env_global static_env) (global-env->static (env->global config-env)))
       ((acl2::local-stobjs orac)
        (mv new-types orac))
       ((mv res orac) (ty-global_decl_keyword-imap-resolve static_env.storage_types config-env orac)))
    (mv (and (eval_result-case res :ev_normal)
             (ev_normal->res res))
        orac)))

(define ty-global_decl_keyword-imap-resolved-p ((types ty-global_decl_keyword-imap-p))
  (if (atom types)
      t
    (and (or (not (mbt (and (consp (car types))
                            (identifier-p (caar types)))))
             (ty-resolved-p (ty-global_decl_keyword->ty (cdar types))))
         (ty-global_decl_keyword-imap-resolved-p (cdr types))))
  ///
  (local (in-theory (enable ty-global_decl_keyword-imap-fix))))
          

;; This is just an IF that indicates that the two branches might be
;; choose-values and in any case should be merged into one.
(define choose-value-if (test then else)
  (if test then else))




;; To find common suffixes for two alists, we use two identical binders. One is
;; used at the top level; we first even out the lengths of the two alists and
;; then use the other binder to find the common suffix when the alists' lengths
;; are equal.

;; To be used when the alists have the same length.
(define common-suffix-binder2 (suff alist1 alist2)
  (if (and (acl2::suffixp suff alist1)
           (acl2::suffixp suff alist2))
      suff
    nil))

;; To be used when we don't know if the alists have the same length.
(define common-suffix-binder (suff alist1 alist2)
  (if (and (acl2::suffixp suff alist1)
           (acl2::suffixp suff alist2))
      suff
    nil))


(defun oracle-marker (x) x)


(define counterexample-with-backtrace (sat-config err)
  :non-executable t
  :verify-guards nil
  (declare (ignorable sat-config err))
  nil)

(define val-imap-keys ((x val-imap-p))
  :returns (keys identifierlist-p)
  (omap::keys (val-imap-fix x))
  ///
  (fgl::remove-fgl-rewrite val-imap-keys)
  (defthm in-of-val-imap-keys
    (iff (in k (val-imap-keys x))
         (and (identifier-p k)
              (val-imap-has-key k x)))
    :hints(("Goal" :in-theory (enable val-imap-has-key
                                      omap::in-of-keys-to-assoc))))

  (local (defthm member-when-setp
           (implies (setp x)
                    (iff (member k x)
                         (in k x)))
           :hints(("Goal" :in-theory (enable in head tail emptyp
                                             setp)))))
           

  (defret setp-of-val-imap-keys
    (setp keys))
    
  (defthm member-of-val-imap-keys
    (iff (member k (val-imap-keys x))
         (and (identifier-p k)
              (val-imap-has-key k x)))
    :hints(("Goal" :in-theory (enable val-imap-has-key
                                      omap::in-of-keys-to-assoc)))))

(define v_array-nth-resolve-equal-indices (n x type)
  (Declare (ignore n x type))
  t
  ///
  (fgl::remove-fgl-rewrite v_array-nth-resolve-equal-indices))




;; Termination-error tools

(define some-value (x (lst consp))
  (if (or (equal x (car lst))
          (atom (cdr lst)))
      (car lst)
    (some-value x (cdr lst)))
  ///
  (defthm member-of-some-value
    (implies (consp lst)
             (member-equal (some-value x lst) lst)))

  (defthm some-value-when-member
    (implies (member-equal x lst)
             (equal (some-value x lst) x))))

(define termination-errmsg-p ((x stringp))
  (and (member-equal (acl2::str-fix x)
                     '("DE_LE: Recursion limit ran out"
                       "DE_LE: Loop limit ran out"
                       "DE_LE: Recursion limit ran out"
                       "Clock ran out resolving named type"))
       t)
  ///
  (defthm termination-error-p-of-ev_error-errmsg
    (iff (termination-error-p (ev_error desc data bt))
         (termination-errmsg-p desc))
    :hints(("Goal" :in-theory (enable termination-error-p))))

  (fgl::add-fgl-rewrite termination-error-p-of-ev_error-errmsg)
  (fgl::remove-fgl-rewrite termination-errmsg-p)

  (fgl::enable-split-ifs termination-errmsg-p))

(define termination-errmsg-fix ((x stringp))
  :hooks nil
  (some-value x
              '("DE_LE: Recursion limit ran out"
                "DE_LE: Loop limit ran out"
                "DE_LE: Recursion limit ran out"
                "Clock ran out resolving named type"))
  ///
  (local (defthm stringp-of-some-value
           (implies (and (string-listp lst)
                         (consp lst))
                    (stringp (some-value x lst)))
           :hints(("Goal" :in-theory (enable some-value)))))

  (defthm stringp-of-termination-errmsg-fix
    (stringp (termination-errmsg-fix x)))

  (defthm termination-errmsg-p-of-termination-errmsg-fix
    (termination-errmsg-p (termination-errmsg-fix x))
    :hints(("Goal" :in-theory (enable termination-errmsg-p))))

  (fgl::add-fgl-rewrite termination-errmsg-p-of-termination-errmsg-fix)

  (defthm termination-errmsg-fix-when-termination-errmsg-p
    (implies (and (termination-errmsg-p x)
                  (stringp x))
             (equal (termination-errmsg-fix x) x))
    :hints(("Goal" :in-theory (enable termination-errmsg-p))))

  )

(define termination-error-fix ((x eval_result-p))
  :guard (termination-error-p x)
  :returns (new-x eval_result-p)
  ;; :prepwork ((local (in-theory (enable termination-error-p))))
  :verify-guards nil
  (b* (((ev_error x)))
    (ev_error (termination-errmsg-fix x.desc)
              x.data x.backtrace))
  ///
  
  (defret termination-error-p-of-<fn>
    (termination-error-p new-x))

  (defret <fn>-when-termination-error-p
    (implies (termination-error-p x)
             (equal new-x (eval_result-fix x)))
    :hints(("Goal" :in-theory (enable termination-error-p))))

  (fgl::def-fgl-rewrite eval_result-kind-of-termination-error-fix
    (equal (eval_result-kind (termination-error-fix x))
           :ev_error)
    :hints(("Goal" :in-theory (enable termination-error-fix)))))

(fgl::def-fgl-rewrite termination-error-p-of-ev_normal
  (not (termination-error-p (ev_normal res))))

(fgl::def-fgl-rewrite termination-error-p-of-ev_throwing
  (not (termination-error-p (ev_throwing throwdata env backtrace))))

(fgl::remove-fgl-rewrites termination-error-p termination-errmsg-fix)


;; Tell FGL counterexample generation to use the given initial environment to
;; provide default values for global variables not assigned by the
;; counterexample.
(defmacro install-eval_subprogram-wrappers (config-env)
  `(progn (table fgl::magitastic-ev-definitions
                 'eval_subprogram-fn
                 (list '(env name vparams vargs clk orac)
                       '(eval_subprogram-wrap env ,config-env name vparams vargs clk orac)))
          (table fgl::magitastic-ev-definitions
                 'eval_subprogram-*t-fn
                 (list '(env name vparams vargs clk orac pos static-env tracespec)
                       '(eval_subprogram-*t-wrap env ,config-env static-env name vparams vargs clk orac pos tracespec)))
          (table fgl::magitastic-ev-definitions
                 'storage-with-default-values
                 (list '(storage)
                       '(storage-add-default-values storage ,config-env)))))



(define bcw-separate-initial-keyword-args (args)
  :mode :program
  (cond  ((atom args)
          (mv nil nil))
         ((or (not (keywordp (car args)))
              (atom (cdr args)))
          (mv nil args))
         (t (b* (((mv kws rest) (bcw-separate-initial-keyword-args (cddr args))))
              (mv (cons (cons (car args) (cadr args)) kws)
                  rest)))))
        

(define bcw-fn (args !p)
  :mode :program
  (b* (((mv kw-alist cw-args)
        (bcw-separate-initial-keyword-args args))
       (alist `(pairlis2 acl2::*base-10-chars* (list . ,(cdr cw-args))))
       (str (car cw-args))
       (col (or (cdr (assoc :column kw-alist))
                (cdr (assoc :col kw-alist))
                0))
       (evisc-tuple (or (cdr (assoc :evisc-tuple kw-alist))
                        (cdr (assoc :evisc kw-alist))
                        (cdr (assoc :ev kw-alist))))
       (print-base-radix (or (cdr (assoc :print-base-radix kw-alist))
                             (cdr (assoc :pbr kw-alist)))))
    `(,(if !p 'fmt-to-comment-window! 'fmt-to-comment-window)
      ,str ,alist ,col ,evisc-tuple ,print-base-radix)))



(defmacro bcw (&rest args)
  (bcw-fn args nil))

(defmacro bcw! (&rest args)
  (bcw-fn args t))

;; (bcw :evisc '(nil 4 7 nil) "~x0~%" x)
