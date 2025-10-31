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
(include-book "ihs/logops-definitions" :dir :system)
(include-book "utils/omaps")
(local (include-book "std/lists/nth" :dir :system))
(local (include-book "std/lists/repeat" :dir :system))
(local (include-book "std/lists/take" :dir :system))
(local (include-book "centaur/bitops/ihsext-basics" :dir :system))

(local (table fty::deftagsum-defaults :short-names t))
(local (in-theory (disable (tau-system))))

(defxdoc asl-types
  :parents (asl)
  :short "Types used in the ASL interpreter (umbrella topic)"
  :long "<p>See also @(see asl-ast).</p>")

(local (xdoc::set-default-parents asl-types))

(deftypes val
  :short "An ASL value"
  (deftagsum val
    (:v_int ((val integerp :rule-classes :type-prescription)))
    (:v_bool ((val booleanp)))
    (:v_real ((val rationalp :rule-classes :type-prescription)))
    (:v_string ((val stringp :rule-classes :type-prescription)))
    (:v_bitvector ((len natp :rule-classes :type-prescription)
                   (val integerp :reqfix (loghead len val)))
     :require (unsigned-byte-p len val))
    (:v_label ((val identifier-p)))
    (:v_record ((rec val-imap)))
    (:v_array  ((arr vallist))))
  (fty::deflist vallist :elt-type val :true-listp t)
  (fty::defomap val-imap :key-type identifier :val-type val ;; :true-listp t
    :short "Mapping from identifiers to ASL values. Used both for the values of
records (see @(see val)) as well as the storage of local and global variables.")
  ///

  (defthm vallist-p-of-update-nth
    (implies (and (vallist-p x)
                  (val-p v)
                  (<= (nfix n) (len x)))
             (vallist-p (update-nth n v x)))
    :hints(("Goal" :in-theory (enable update-nth vallist-p))))

  (defthm vallist-p-vals-of-val-imap-p
    (implies (val-imap-p x)
             (vallist-p (omap::key-ord-values x)))
    :hints(("Goal" :in-theory (enable omap::key-ord-values))))

  (defthm identifierlist-p-keys-of-val-imap-p
    (implies (val-imap-p x)
             (identifierlist-p (omap::keys x)))
    :hints(("Goal" :in-theory (enable omap::keys-redef
                                      (:i omap::keys)))))

  (defthm val-imap-p-of-from-lists
    (implies (and (identifierlist-p x)
                  (vallist-p y)
                  (equal (len x) (len y)))
             (val-imap-p (omap::from-lists x y)))
    :hints(("Goal" :in-theory (enable omap::from-lists)))))


(defthm identifierlist-p-of-insert
  (implies (and (identifierlist-p x)
                (identifier-p k))
           (identifierlist-p (insert k x)))
  :hints(("Goal" :in-theory (enable insert
                                    tail emptyp head))))

(defthm identifierlist-p-of-mergesort
  (implies (identifierlist-p x)
           (identifierlist-p (mergesort x)))
  :hints(("Goal" :in-theory (enable mergesort))))

(fty::defmap val-alist :key-type identifier :Val-type val :true-listp t
  ///

  (defthm val-alist-p-of-pairlis$
    (implies (and (identifierlist-p keys)
                  (vallist-p vals)
                  (equal (len keys) (len vals)))
             (val-alist-p (pairlis$ keys vals)))))


(fty::defoption maybe-val val)


(fty::defmap pos-imap :key-type identifier :val-type posp :true-listp t)

(defprod global-env
  :short "ASL interpreter global environment"
  ((static static_env_global
           "Static environment, containing type declarations and subprogram declarations, among other things")
   (storage val-imap "Maps global variables to their values")
   (stack_size pos-imap "Maps subprogram names to their current number of nested calls"))
  :layout :list)

(defprod unit () :layout :list
  ///
  (defthm unit-p-compound-recognizer
    (implies (unit-p x)
             (equal x nil))
    :hints(("Goal" :in-theory (enable unit-p)))
    :rule-classes :compound-recognizer))

(in-theory (enable (:t unit)
                   (:t unit-fix)))

(fty::deflist integer-list :pred integer-listp :elt-type integerp :true-listp t
  :elementp-of-nil nil)

;; Hack: remove the latest integer-listp topic from the xdoc table so it's not duplicated
(table xdoc::xdoc 'xdoc::doc (let ((topics (xdoc::get-xdoc-table world)))
                               (remove-equal (xdoc::find-topic 'integer-listp topics) topics)))


(fty::deflist val-imaplist :elt-type val-imap :true-listp t :elementp-of-nil t)

(defprod local-env
  :short "ASL interpreter local environment."
  :long "<p>Subprogram calls always start in an empty local environment (see @(see empty-local-env)).
Some statements create local scopes (blocks), which cause a new frame to be
added to @('storage'); after that block is finished, that frame is removed,
deleting all the variables declared in that block.</p>"
  ((storage val-imaplist
            "List of nested local scopes of variable values. The @('car') is the innermost
scope.")
   (scope unit "unused")
   (unroll integer-list "unused")
   (declared identifierlist "unused"))
  :layout :list)

(define empty-local-env ()
  :parents (local-env asl-interpreter-functions)
  :short "Create an empty local environment, as when starting evaluation of a subprogram
body (see @(see eval_subprogram))."
  :returns (empty local-env-p)
  (make-local-env :storage '(nil)))


(defprod env
  :short "ASL environment"
  :long "<p>The global and local environment are treated very differently by the
interpreter. The global environment is updated almost single-threadedly, except
in cases where there are supposed to be no modifications made to it. The local
environment is created anew for every subroutine call, and thrown out
afterward.</p>"
  ((global global-env)
   (local local-env))
  :layout :list)


(defprod val_read_from
  :short "not used"
  ((val val)
   (name identifier)
   (scope unit))
  :layout :list)

(fty::deflist val_read_from-list :elt-type val_read_from :true-listp t)

(defprod throwdata
  :short "Exception data produced by a @(see s_throw) statement"
  ((val val-p)
   (ty ty))
  :layout :list)




(defoption maybe-throwdata throwdata)

(deftagsum eval_result
  :short "A result of evaluation in the interpreter."
  :long "<p>This is \"generic\" type in that the @('res') field of an @('ev_normal') can
be of several different types. Throughout the interpreter, we generally want to
return either a normal result or produce (or pass on) an error or exception;
this is our mechanism for doing this everywhere. The macro @(see
def-eval_result) helps define an eval_result subtype specifying the type of the
ev_normal @('res') field.</p>"
  (:ev_normal (res)
   :short "Normal evaluation result. @('res') is of different types in different
functions. E.g., @(see eval_expr) produces an eval_result where @('res') is an
@(see expr_result); @(see eval_binop) produces an eval_result where @('res') is
a @(see val).")
  (:ev_throwing ((throwdata maybe-throwdata "Exception value and type; if empty, this must be a re-throw inside a catcher.")
                 (env env "Environment resulting from evaluation up until the exception was thrown.")
                 (backtrace))
   :short "Result when an exception was thrown. May be caught by an @(see s_try) statement.")
  (:ev_error    ((desc stringp)
                 (data)
                 (backtrace))
   :short "Result of a runtime error."))


(defxdoc def-eval_result
  :parents (eval_result)
  :short "Define a predicate for an object that is an @(see eval_result) and, when @(see
ev_normal), the @('res') field is of the given type.")

(defmacro def-eval_result (pred res-pred)
  `(define ,pred (x)
     (and (eval_result-p x)
          (eval_result-case x
            :ev_normal (,res-pred x.res)
            :otherwise t))
     ///
     (defthm ,(intern-in-package-of-symbol
               (concatenate 'string (symbol-name pred) "-IMPLIES") pred)
       (implies (,pred x)
                (and (eval_result-p x)
                     (implies (eval_result-case x :ev_normal)
                              (,res-pred (ev_normal->res x))))))

     (defthm ,(intern-in-package-of-symbol
               (concatenate 'string (symbol-name pred) "-WHEN-EVAL_RESULT-P") pred)
       (implies (and (eval_result-p x)
                     (or (not (eval_result-case x :ev_normal))
                         (,res-pred (ev_normal->res x))))
                (,pred x)))))


(defprod intpair/env
  :short "Result of evaluating a bitvector slice -- see @(see eval_slice)"
  ((pair intpair-p)
   (env  env-p))
  :layout :fulltree)

(def-eval_result slice_eval_result-p intpair/env-p)

(defprod intpairlist/env
  :short "Result of evaluating a list of bitvector slices -- see @(see eval_slice_list)"
  ((pairlist intpairlist-p)
   (env  env-p))
  :layout :fulltree)

(def-eval_result slices_eval_result-p intpairlist/env-p)



(acl2::def-b*-binder ev
  :parents (asl-interpreter-functions)
  :short "Binds an eval_result object. If it is an ev_error or ev_throwing, returns it
immediately, unchanged. If it is an @('ev_normal'), bind the argument to its
@('res') field and continue to evaluate the rest of the bindings and body. See
also @(see patbind-evo) which returns the @('orac') alongside the
error/throwing results."
  :body
  `(b* ((evresult ,(car acl2::forms)))
     (eval_result-case evresult
       :ev_normal (b* ,(and (not (eq (car acl2::args) '&))
                            `((,(car acl2::args) evresult.res)))
                    ,acl2::rest-expr)
       :otherwise evresult)))

(defxdoc ev
  :short "@(csee B*) binder: see @(see patbind-ev)")



(defprod expr_result
  :short "Type of the result from evaluating an ASL expression"
  ((val val)
   (env env)))

(def-eval_result expr_eval_result-p expr_result-p)

(defprod exprlist_result
  :short "Type of the result from evaluating a list of ASL expressions"
  ((val vallist)
   (env env)))

(def-eval_result exprlist_eval_result-p exprlist_result-p)

(deftagsum control_flow_state
  :short "Type of result from evaluating a statement"
  (:returning ((vals vallist)
               (env global-env))
   :short "Indicates that a return has been encountered")
  (:continuing ((env env))
   :short "Indicates that no return has been encountered and execution of the current
function continues"))

(def-eval_result stmt_eval_result-p control_flow_state-p)

(defprod func_result ((vals vallist ;; val_read_from-list
                            )
                      (env global-env))
  :short "Type of result from evaluating a function call: a list of return values and an
updated global environment")

(def-eval_result func_eval_result-p func_result-p)

