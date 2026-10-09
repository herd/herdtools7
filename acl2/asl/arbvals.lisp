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
(include-book "interp-mods")
(local (std::add-default-post-define-hook :fix))

;; Our ASL interpreter currently models ARBITRARY values by passing an
;; "oracle" object through all interpreter calls.  Whenever an ARBITRARY
;; expression is run, this oracle object is updated, replacing the
;; previous oracle with a new version. This means every interpreter
;; function both takes the oracle as input and returns it as output, and
;; after every return, the subsequent interpreter call takes the new
;; oracle single-threadedly.

;; This complicates reasoning in many cases because if two interpreter
;; calls are sequenced, the subsequent one must depend on the prior one's
;; oracle result even if the two have no relation. In particular, any
;; ARBITRARY expression results in the subsequent call actually depend on
;; whether there were ARBITRARY expressions encountered in the prior
;; call.

;; E.g., if we have:
;;   if (test)
;;      a = ARBITRARY : a_type;
;;   endif;
;;   b = ARBITRARY : b_type;

;; The assignment to b here depends on whether test was true or false; in
;; particular, it is something like
;;   read_oracle_val(b_type, read_oracle_orac(a_type, oracle)) if test,
;;   read_oracle_val(b_type, oracle) if not.

;; It doesn't have to be this way. Really, we just want all evaluations
;; of ARBITRARY expressions in any interpreter execution to be able to
;; produce any welltyped value, independent of anything else that happens
;; in the execution. That is, for any evaluation of an ARBITRARY
;; expression, there should be some field of some input (call it arbvals)
;; that determines its value but doesn't affect anything else in the
;; execution.  This is easy enough in concept: the input should map
;; objects designating evaluations of ARBITRARY expressions to the values
;; they produce.  We can get something similar to the oracle scheme from
;; this framework, if we take the count of previously executed ARBITRARY
;; evaluations to be the designator for an evaluation of an ARBITRARY.

;; E.g., for our example code above, the assignment for b would be
;;     read_arbval(b_type, 1, arbval) if test,
;;     read_arbval(b_type, 0, arbval) if not.
;; We can map between this and the oracle scheme (assuming the type input
;; in each case just fixes the value to that type): given an oracle, we
;; can create an isomorphic arbval by mapping arbval(i) to
;; oracle_val(oracle_orac^i(orac)) and vice versa. 

;; This still means that the designator for an ARBITRARY execution
;; depends dynamically on what came before it (the designator for the
;; ARBITRARY execution assigning b is 1 if test, 0 if not). If we could
;; avoid this, making designators only depend on static factors, then our
;; evaluation of the statement assigning b wouldn't have any dependency
;; on the previous execution.

;; Suppose we define an 'address' for an ARBITRARY execution based on
;; call stack, loop iteration, and code position, as in the following
;; example:
;;  the 2nd (static) ARBITRARY form
;;    inside the 3rd iteration of the 2nd toplevel loop
;;      inside the 2nd iteration of the 1st toplevel loop
;;        inside the (static) 3rd call of function Foo
;;          inside the (static) 1st call of function Bar.

;; Here static means not accounting for IF branches. E.g., in the example
;; above, the assignment of a contains the first ARBITRARY form and the
;; assignment of b contains the second ARBITRARY form. Loop bodies and
;; conditions are skipped when computing these indices. This should
;; uniquely, statically identify a certain execution of ARBITRARY; that
;; is, in any execution there can be at most one execution of ARBITRARY
;; fitting the above description, and a unique such description fitting
;; any execution of ARBITRARY. For this to be true we need to know that
;; loops and function calls are the only constructs that cause
;; expressions/statements to evaluate multiple times.

;; Supposing these addresses satisfy that property, we can then use these
;; as a key to a lookup in arbvals and keep the full generality of using
;; the oracle for ARBITRARY. We can prove this by showing that for an
;; interpreter call with any oracle input, there is an arbvals input for
;; which the analogous arbvals interpreter call gets the same
;; (non-oracle) results. For this, we need an interpreter-like function
;; that maps oracles to arbvals.

;; Here is a sketch of the work involved in getting there:
;;  - Define arbvals and address structures
;;      * Combine these structures into a single object that can be passed
;;        through the interpreter, encapsulating the arbvals object and the
;;        constant suffix of all addresses used in the current interpreter
;;        call.
;;  - Implement interpreter that operates on arbvals ("arbvals interpreter")
;;      * Replace oracle input with arbvals input, remove oracle output
;;      * Implement ARBITRARY (look up address in arbvals, type-fix result)
;;      *** Implement address composition (count past calls/arbitraries/
;;          loops, correctly nest address components).
;;  - Implement oracle to arbvals translation based on interpreter
;;    ("translating interpreter")
;;      * For each recursive call of original interpreter, does a
;;        corresponding recursive call of the translating interpreter
;;        and returns arbvals 
;;  - Prove that arbvals interpreter applied to oracle-to-arbvals translation
;;    gets same result as original interpreter
;;      * Show that arbvals returned by different calls of translating
;;        interpreter can be combined without interference
;;      * Show that only arbvals changes within scope affect the output of
;;        arbvals interpreter

;; Something else we may want to consider is how decodable/user-friendly
;; our addresses are. For example, we may need to refer to certain
;; nondeterministic values in theorems -- e.g., the descriptor read in
;; level 1 of address translation. Variants of the address representation
;; may make this easier or harder. Consistency of these addresses across
;; different versions of the ASL code also might depend a lot on the
;; representation. For example, it might be more stable to refer to "the
;; first toplevel call of FullTranslate" than to refer to "the 5th
;; toplevel function call".


;; An address is a list of components, giving a nesting of scopes (inside-out
;; order). The indices indicate which occurrence of the construct within the
;; outer scope (and not within any other scopes inside it); e.g., ((:call "foo"
;; 2) (:while 1 2)) would be the third call of "foo" within the second
;; iteration of the first while loop, omitting any other loops/calls within
;; that loop. BOZO -- it only makes sense for an :arb to be the first
;; (innermost) such construct; other things aren't nested inside the scope of
;; an arbitrary.
(deftagsum arbaddr-component
  (:call ((fn identifier :rule-classes :type-prescription)
          (idx natp :rule-classes :type-prescription)))
  (:arb ((idx natp :rule-classes :type-prescription)))
  (:for ((idx natp :rule-classes :type-prescription)
         ;; Note: it might be user-friendly to reuse the for loop's iterator
         ;; value here. may be problems due to the downto case though?
         (iter natp :rule-classes :type-prescription)))
  (:while ((idx natp :rule-classes :type-prescription)
           (iter natp :rule-classes :type-prescription)))
  (:repeat ((idx natp :rule-classes :type-prescription)
            (iter natp :rule-classes :type-prescription)))
  :layout :list)

(deflist arbaddr :elt-type arbaddr-component :true-listp t :elementp-of-nil nil)

;; An arbmap holds the actual nondeterministic data, mapping addresses to values.
(fty::defmap arbmap :key-type arbaddr :val-type val :true-listp t :valp-of-nil nil)

;; Lookup an address in an arbmap, returning an object of a given type.
(define arbmap-lookup ((key arbaddr-p) (ty ty-p) (map arbmap-p))
  :guard (ty-resolved-p ty)
  :returns (res (iff (val-p res) res))
  (and (ty-satisfiable ty)
       (ec-call (ty-fix-val (cdr (hons-assoc-equal (arbaddr-fix key)
                                                   (arbmap-fix map)))
                            ty)))
  ///
  (defret arbmap-lookup-satisfies-type
    (implies (ty-satisfiable ty)
             (ty-satisfied res ty))))

;; An arbaddr-offset keeps track of how many of each kind of address element
;; have been passed within the current scope. Used to get the right index
;; when creating an address scope.

(fty::defset identifierset :elt-type identifier)

;; Component of an arbaddr-offset that maps function names to offset indices
;; (positive because they should be absent if zero)
(fty::defomap call-count-map :key-type identifier :val-type posp
  ///
  (defthm posp-assoc-of-call-count-map
    (implies (and (call-count-map-p x)
                  (omap::assoc key x))
             (posp (cdr (omap::assoc key x))))
    :rule-classes :type-prescription)

  (defthm posp-assoc-of-call-count-map-fix
    (implies (omap::assoc key (call-count-map-fix x))
             (posp (cdr (omap::assoc key (call-count-map-fix x)))))
    :rule-classes :type-prescription)

  (defthm identifierset-p-keys-of-call-countmap
    (implies (call-count-map-p x)
             (identifierset-p (omap::keys x)))
    :hints(("Goal" :in-theory (enable omap::keys)))))

(defprod arbaddr-offset
  ((callmap call-count-map)
   (arboffset natp :rule-classes :type-prescription :default 0)
   (foroffset natp :rule-classes :type-prescription :default 0)
   (whileoffset natp :rule-classes :type-prescription :default 0)
   (repeatoffset natp :rule-classes :type-prescription :default 0))
  :layout :list)

(define call-count-map-entry ((key identifier-p) (x call-count-map-p))
  :returns (val natp :rule-classes :type-prescription)
  (b* ((look (omap::assoc (identifier-fix key) (call-count-map-fix x))))
    (if look (cdr look) 0)))

;; The sum of two arbaddr-offsets gives the sum of all the offsets of both
(define call-count-map-sum-aux ((keys identifierset-p)
                                (x call-count-map-p)
                                (y call-count-map-p))
  :verify-guards nil
  :returns (new call-count-map-p)
  :measure (acl2-count (identifierset-fix keys))
  :hints (("goal" :in-theory (enable identifierset-fix)))
  (b* ((keys (identifierset-fix keys)))
    (if (set::emptyp keys)
        nil
      (let* ((key (set::head keys))
             (val (+ (call-count-map-entry key x)
                     (call-count-map-entry key y))))
        (if (eql val 0)
            (call-count-map-sum-aux (set::tail keys) x y)
          (omap::update key
                        val
                        (call-count-map-sum-aux (set::tail keys) x y))))))
  ///
  (verify-guards call-count-map-sum-aux)
  (defret assoc-of-<fn>
    (equal (omap::assoc key new)
           (and (set::in key (identifierset-fix keys))
                (let ((val (+ (call-count-map-entry key x)
                              (call-count-map-entry key y))))
                  (and (not (equal 0 val))
                       (cons key val))))))
  (defret call-count-map-entry-of-<fn>
    (equal (call-count-map-entry key new)
           (if (set::in (identifier-fix key) (identifierset-fix keys))
               (+ (call-count-map-entry key x)
                  (call-count-map-entry key y))
             0))
    :hints(("Goal" :in-theory (enable call-count-map-entry)
            :do-not-induct t))))
                

(local (include-book "std/lists/sets" :dir :System))
(local (fty::deflist identifierlist :elt-type identifier :true-listp t))

(define call-count-map-sum ((x call-count-map-p) (y call-count-map-p))
  :returns (new call-count-map-p)
  :verify-guards nil
  (mbe :logic (b* ((x (call-count-map-fix x))
                   (y (call-count-map-fix y)))
                (call-count-map-sum-aux (set::union (omap::keys x) (omap::keys y)) x y))
       :exec
       (cond ((atom x) y)
             ((atom y) x)
             (t (call-count-map-sum-aux (set::union (omap::keys x) (omap::keys y)) x y))))
  ///
  (local (defthm call-count-map-sum-aux-identity
           (implies (call-count-map-p x)
                    (equal (call-count-map-sum-aux (omap::keys x) x nil)
                           x))
           :hints (("goal" :use ((:instance omap::diff-key-when-unequal
                                  (x x) (y (call-count-map-sum-aux (omap::keys x) x nil))))
                    :in-theory (enable call-count-map-entry
                                       omap::in-of-keys-to-assoc-under-iff)))))
  (local (defthm call-count-map-sum-aux-identity2
           (implies (call-count-map-p x)
                    (equal (call-count-map-sum-aux (omap::keys x) nil x)
                           x))
           :hints (("goal" :use ((:instance omap::diff-key-when-unequal
                                  (x x) (y (call-count-map-sum-aux (omap::keys x) nil x))))
                    :in-theory (enable call-count-map-entry
                                       omap::in-of-keys-to-assoc-under-iff)))))
  (verify-guards call-count-map-sum)
  (defret assoc-of-<fn>
    (equal (omap::assoc key new)
           (and (or (omap::assoc key (call-count-map-fix x))
                    (omap::assoc key (call-count-map-fix y)))
                (cons key
                      (+ (call-count-map-entry key x)
                         (call-count-map-entry key y)))))
    :hints(("Goal" :in-theory (enable omap::in-of-keys-to-assoc-under-iff
                                      call-count-map-entry))))
  (defret call-count-map-entry-of-<fn>
    (equal (call-count-map-entry key new)
           (+ (call-count-map-entry key x)
              (call-count-map-entry key y)))
    :hints(("Goal" :in-theory (enable call-count-map-entry
                                      omap::in-of-keys-to-assoc-under-iff)
            :do-not-induct t)))

  (defthm call-count-map-sum-identity1
    (equal (call-count-map-sum nil x)
           (call-count-map-fix x)))

  (defthm call-count-map-sum-identity2
    (equal (call-count-map-sum x nil)
           (call-count-map-fix x)))

  (local (in-theory (disable call-count-map-sum)))
  (defthmd call-count-map-sum-commutative
    (equal (call-count-map-sum y x)
           (call-count-map-sum x y))
    :hints (("goal" :use ((:instance omap::diff-key-when-unequal
                           (x (call-count-map-sum y x))
                           (y (call-count-map-sum x y)))))))
  (defthmd call-count-map-sum-commutative-2
    (equal (call-count-map-sum y (call-count-map-sum x z))
           (call-count-map-sum x (call-count-map-sum y z)))
    :hints (("goal" :use ((:instance omap::diff-key-when-unequal
                           (x (call-count-map-sum y (call-count-map-sum x z)))
                           (y (call-count-map-sum x (call-count-map-sum y z))))))))
  (defthmd call-count-map-sum-associative
    (equal (call-count-map-sum (call-count-map-sum x y) z)
           (call-count-map-sum x (call-count-map-sum y z)))
    :hints (("goal" :use ((:instance omap::diff-key-when-unequal
                           (x (call-count-map-sum (call-count-map-sum x y) z))
                           (y (call-count-map-sum x (call-count-map-sum y z)))))))))

(define arbaddr-offset-sum ((x arbaddr-offset-p) (y arbaddr-offset-p))
  :Returns (new arbaddr-offset-p)
  (b* (((arbaddr-offset x))
       ((arbaddr-offset y)))
    (make-arbaddr-offset
     :callmap (call-count-map-sum x.callmap y.callmap)
     :arboffset (+ x.arboffset y.arboffset)
     :whileoffset (+ x.whileoffset y.whileoffset)
     :foroffset (+ x.foroffset y.foroffset)
     :repeatoffset (+ x.repeatoffset y.repeatoffset)))
  ///
  (defthmd arbaddr-offset-sum-commutative
    (equal (arbaddr-offset-sum y x)
           (arbaddr-offset-sum x y))
    :hints(("Goal" :in-theory (enable call-count-map-sum-commutative))))
  (defthmd arbaddr-offset-sum-commutative-2
    (equal (arbaddr-offset-sum y (arbaddr-offset-sum x z))
           (arbaddr-offset-sum x (arbaddr-offset-sum y z)))
    :hints(("Goal" :in-theory (enable call-count-map-sum-commutative-2))))
  (defthmd arbaddr-offset-sum-associative
    (equal (arbaddr-offset-sum (arbaddr-offset-sum x y) z)
           (arbaddr-offset-sum x (arbaddr-offset-sum y z)))
    :hints(("Goal" :in-theory (enable call-count-map-sum-associative))))

  (make-event
   `(defthm arbaddr-offset-sum-identity-1
      (equal (arbaddr-offset-sum ',(make-arbaddr-offset) x)
             (arbaddr-offset-fix x))))

  (make-event
   `(defthm arbaddr-offset-sum-identity-2
      (equal (arbaddr-offset-sum x ',(make-arbaddr-offset))
             (arbaddr-offset-fix x)))))





;; Computing arbaddr-offsets of AST elements.
;; This is a fairly straightforward visitor function.

(include-book "centaur/fty/visitor" :dir :system)

(fty::defvisitor-template arbaddr-offset ((x :object))
  :returns (offset (:join (arbaddr-offset-sum offset1 offset)
                    :initial (make-arbaddr-offset)
                    :tmp-var offset1)
                   arbaddr-offset-p)
  :renames ((expr expr-arbaddr-offset-aux)
            (call call-arbaddr-offset-aux)
            (lexpr lexpr-arbaddr-offset-aux)
            (stmt stmt-arbaddr-offset-aux)
            (ty ty-arbaddr-offset-aux))
  :type-fns ((expr expr-arbaddr-offset)
             (call call-arbaddr-offset)
             (lexpr lexpr-arbaddr-offset)
             (stmt stmt-arbaddr-offset)
             (ty ty-arbaddr-offset))
  ;; Skip over loop subscopes because they're a different address namespace.
  :prod-fns ((s_for (body :skip))
             (s_while (body :skip) (test :skip))
             (s_repeat (body :skip) (test :skip)))
  :fnname-template <type>-arbaddr-offset)


(define fnname-arbaddr-offset ((name identifier-p))
  :returns (offset arbaddr-offset-p)
  (make-arbaddr-offset :callmap (omap::update (identifier-fix name) 1 nil)))

(fty::defvisitor-multi expr-arbaddr-offset
  (define expr-arbaddr-offset ((x expr-p))
    :measure (acl2::nat-list-measure (list (expr-count x) 1))
    :returns (offset arbaddr-offset-p)
    (arbaddr-offset-sum
     (b* ((desc (expr->desc x)))
       (expr_desc-case desc
         :e_arbitrary (make-arbaddr-offset :arboffset 1)
         :otherwise (make-arbaddr-offset)))
     (expr-arbaddr-offset-aux x)))

  (define ty-arbaddr-offset ((x ty-p))
    :measure (acl2::nat-list-measure (list (ty-count x) 1))
    :returns (offset arbaddr-offset-p)
    (arbaddr-offset-sum
     (b* ((desc (ty->desc x)))
       (type_desc-case desc
         :t_named (fnname-arbaddr-offset desc.name)
         :otherwise (make-arbaddr-offset)))
     (ty-arbaddr-offset-aux x)))

  (define call-arbaddr-offset ((x call-p))
    :measure (acl2::nat-list-measure (list (call-count x) 1))
    :returns (offset arbaddr-offset-p)
    (arbaddr-offset-sum
     (b* (((call x)))
       (fnname-arbaddr-offset x.name))
     (call-arbaddr-offset-aux x)))

  (fty::defvisitor :template arbaddr-offset
    :type expr
    :measure (acl2::nat-list-measure (list :count 0))))

(fty::defvisitor-multi lexpr-arbaddr-offset
  (define lexpr-arbaddr-offset ((x lexpr-p))
    :measure (acl2::nat-list-measure (list (lexpr-count x) 1))
    :returns (offset arbaddr-offset-p)
    (arbaddr-offset-sum
     (b* ((desc (lexpr->desc x)))
       (lexpr_desc-case desc
         :le_slice (expr-arbaddr-offset (expr_of_lexpr desc.base))
         :le_setarray (expr-arbaddr-offset (expr_of_lexpr desc.base))
         :le_setfield (expr-arbaddr-offset (expr_of_lexpr desc.base))
         :le_setfields (expr-arbaddr-offset (expr_of_lexpr desc.base))
         :otherwise (make-arbaddr-offset)))
     (lexpr-arbaddr-offset-aux x)))

  (fty::defvisitor :template arbaddr-offset
    :type lexpr
    :measure (acl2::nat-list-measure (list :count 0))))


(fty::defvisitor :template arbaddr-offset
  :type maybe-expr)

(local (in-theory (enable maybe-stmt-some->val)))
(local (defthm stmt-count-less-than-maybe-stmt-count
         (implies x
                  (< (stmt-count x) (maybe-stmt-count x)))
         :hints(("Goal" :in-theory (enable maybe-stmt-count)))
         :rule-classes :linear))
(fty::defvisitor-multi stmt-arbaddr-offset
  (define stmt-arbaddr-offset ((x stmt-p))
    :measure (acl2::nat-list-measure (list (stmt-count x) 1))
    :returns (offset arbaddr-offset-p)
    (arbaddr-offset-sum
     (b* ((desc (stmt->desc x)))
       (stmt_desc-case desc
         :s_for (make-arbaddr-offset :foroffset 1)
         :s_while (make-arbaddr-offset :whileoffset 1)
         :s_repeat (make-arbaddr-offset :repeatoffset 1)
         :otherwise (make-arbaddr-offset)))
     (stmt-arbaddr-offset-aux x)))

  (fty::defvisitor :template arbaddr-offset
    :type stmt
    :measure (acl2::nat-list-measure (list :count 0))))
         


;; Interpreter macro replacements. The main things here are the removal of
;; oracle return values everywhere, and in evbind-*a, adding the arboffset for
;; the thing that was evaluated to the current arboffset.
(defmacro evo_normal-*a (arg)
  `(ev_normal ,arg))

(define pass-error-*a ((val eval_result-p))
  :guard (eval_result-case val :ev_error)
  :inline t
  :enabled t
  (ev_error-fix val))

(defmacro evo_error-*a (&rest args)
  `(pass-error-*a (ev_error . ,args)))

(defmacro evo_throwing-*a (&rest args)
  `(ev_throwing . ,args))

(defmacro evo-return-*a (arg)
  arg)

(defmacro evtailcall-*a (call)
  call)

(acl2::def-b*-binder evbind-*a
  :body
  `(b* ((,(car acl2::args) . ,acl2::forms)
        (?arboffset (add-arboffset-for-form arboffset . ,acl2::forms)))
     ,acl2::rest-expr))

(acl2::def-b*-binder evbind-nonrec-*a
  :body
  `(b* ((,(car acl2::args) . ,acl2::forms))
     ,acl2::rest-expr))

(acl2::def-b*-binder evo-*a
  :body
  `(b* ((evresult ,(car acl2::forms)))
     (eval_result-case evresult
       :ev_normal (b* ,(and (not (eq (car acl2::args) '&))
                           `((,(car acl2::args) evresult.res)))
                    ,acl2::rest-expr)
       :otherwise (eval_result-nonnormal-fix evresult))))

(acl2::def-b*-binder evoo-*a
  :body
  `(b* (((evbind-*a evoo-*a-tmp) . ,acl2::forms)
        ((evo-*a ,(car acl2::args)) evoo-*a-tmp))
     (acl2::check-vars-not-free
      (evoo-*a-tmp)
      ,acl2::rest-expr)))

(acl2::def-b*-binder evs-*a
  :body
  `(b* (((evoo-*a cflow) ,(car acl2::forms)))
     (control_flow_state-case cflow
       :returning (evo_normal-*a
                   (mbe :logic (returning cflow.vals cflow.env)
                        :exec cflow))
       :continuing (b* ,(and (not (eq (car acl2::args) '&))
                             `((,(car acl2::args) cflow.env)))
                     ,acl2::rest-expr))))


;; These "-special" forms are used only in custom definitions below for cases
;; where the current arboffset shouldn't be rebound to its sum with the form's
;; arguments.
(acl2::def-b*-binder evoo-*a-special
  :body
  `(b* (((evbind-nonrec-*a evoo-*a-tmp) . ,acl2::forms) ;; don't rebind arboffset
        ((evo-*a ,(car acl2::args)) evoo-*a-tmp))
     (acl2::check-vars-not-free
      (evoo-*a-tmp)
      ,acl2::rest-expr)))

(acl2::def-b*-binder evs-*a-special
  :body
  `(b* (((evoo-*a-special cflow) ,(car acl2::forms)))
     (control_flow_state-case cflow
       :returning (evo_normal-*a
                   (mbe :logic (returning cflow.vals cflow.env)
                        :exec cflow))
       :continuing (b* ,(and (not (eq (car acl2::args) '&))
                             `((,(car acl2::args) cflow.env)))
                     ,acl2::rest-expr))))


(acl2::def-b*-binder evob-*a
  :body
  `(b* ((evresult ,(car acl2::forms)))
     (eval_result-case evresult
       :ev_normal (b* ,(and (not (eq (car acl2::args) '&))
                            `((,(car acl2::args) evresult.res)))
                    ,acl2::rest-expr)
       :ev_throwing (init-backtrace
                         (ev_throwing-fix evresult)
                         (local-env->storage (env->local env))
                         pos)
       :otherwise (pass-error-*a
                   (init-backtrace
                    (ev_error-fix evresult)
                    (local-env->storage (env->local env))
                    pos)))))


;; Add-arboffset-for-form is a macro that takes an interpreter call form as
;; input and expands to an arbaddr-offset-sum of the program elements that the
;; call "evaluates" (including, e.g., types passed into resolve-ty and
;; is-val-of-type).

(defconst *arboffset-for-form-table*
  '((eval_expr-*a                    (expr-arbaddr-offset 1))
    (resolve-int_constraints-*a      (int_constraintlist-arbaddr-offset 1))
    (resolve-constraint_kind-*a      (constraint_kind-arbaddr-offset 1))
    (resolve-tylist-*a               (tylist-arbaddr-offset 1))
    (resolve-typed_identifierlist-*a (typed_identifierlist-arbaddr-offset 1))
    (resolve-ty-*a                   (ty-arbaddr-offset 1))
    (eval_pattern-*a                 (pattern-arbaddr-offset 2))
    (eval_pattern_list-*a            (patternlist-arbaddr-offset 2))
    (eval_pattern_matcher-*a         (pattern_matcher-arbaddr-offset 2))
    (eval_expr_list-*a               (exprlist-arbaddr-offset 1))
    (eval_call-*a                    (fnname-arbaddr-offset 0)
                                     (exprlist-arbaddr-offset 2)
                                     (exprlist-arbaddr-offset 3))
    (eval_subprogram-*a              )
    (eval_lexpr-*a                   (lexpr-arbaddr-offset 1))
    (eval_lexpr_list-*a              (lexprlist-arbaddr-offset 1))
    (eval_limit-*a                   (maybe-expr-arbaddr-offset 1))
    (eval_stmt-*a                    (stmt-arbaddr-offset 1))
    (eval_catchers-*a                (catcherlist-arbaddr-offset 1)
                                     (maybe-stmt-arbaddr-offset 2))
    (eval_slice-*a                   (slice-arbaddr-offset 1))
    (eval_slice_list-*a              (slicelist-arbaddr-offset 1))
    (eval_block-*a                   (stmt-arbaddr-offset 1))
    (is_val_of_type_tuple-*a         (tylist-arbaddr-offset 2))
    (check_int_constraints-*a        (int_constraintlist-arbaddr-offset 2))
    (is_val_of_type-*a               (ty-arbaddr-offset 2))))


(defun add-arboffsets-for-form-args (form-args table-offsets)
  (declare (xargs :mode :program))
  (if (atom table-offsets)
      nil
    (b* (((list offsetfn argnum) (car table-offsets))
         (term `(,offsetfn ,(nth argnum form-args)))
         (rest (add-arboffsets-for-form-args form-args (cdr table-offsets))))
      (if rest
          `(arbaddr-offset-sum ,term ,rest)
        term))))


(include-book "std/strings/suffixp" :dir :system)
;; removes the -fn at the end of function symbols, makes it so we can use
;; arboffset-for-form in the defret-mutual-generate below
(defun arboffset-normalize-fnsym (sym)
  (declare (xargs :mode :program))
  (if (str::strsuffixp "-FN" (symbol-name sym))
      (intern-in-package-of-symbol
       (subseq (symbol-name sym) 0 (- (length (symbol-name sym)) 3))
       sym)
    sym))

(defun arboffset-for-form-fn (form)
  (declare (xargs :mode :program))
  (let* ((name (arboffset-normalize-fnsym (car form)))
         (offset (add-arboffsets-for-form-args
                  (cdr form)
                  (cdr (assoc name *arboffset-for-form-table*)))))
    (or offset '(make-arbaddr-offset))))

(defmacro arboffset-for-form (form)
  (arboffset-for-form-fn form))

(defmacro add-arboffset-for-form (arboffset form)
  (let ((form-arboffset (add-arboffsets-for-form-args
                         (cdr form)
                         (cdr (assoc (car form) *arboffset-for-form-table*)))))
    (if form-arboffset
        `(arbaddr-offset-sum ,form-arboffset ,arboffset)
      arboffset)))
    


;; New definitions for various interpreter cases.

;; Arbitrary: lookup in arbvals instead of orac. Build address by consing an
;; ARB address component (index from the current arbaddr arboffset) onto the
;; incoming arbaddr.
(defconst *e_arbitrary-*a-def*
  '(b* (((evoo-*a ty) (resolve-ty-*a env desc.type))
        (val (arbmap-lookup (cons (arbaddr-component-arb (arbaddr-offset->arboffset arboffset))
                                  arbaddr)
                            ty arbmap))
        ((unless val)
         (evo_error-*a "DE_AET: " desc (list pos))))
     (evo_normal-*a (expr_result val env))))

;; E_cond, s_cond: Need to include the offset for the then branch when executing the else branch.
(defconst *e_cond-*a-def*
  '(b* (((evoo-*a (expr_result test)) (eval_expr-*a env desc.test))
        ((evo-*a choice) (val-case test.val
                           :v_bool (ev_normal test.val.val)
                           :otherwise (ev_error "bad test in e_cond" test.val (list pos)))))
     (if choice
         (evtailcall-*a (eval_expr-*a test.env desc.then))
       (b* ((arboffset (add-arboffset-for-form arboffset (eval_expr-*a test.env desc.then))))
         (evtailcall-*a (eval_expr-*a test.env desc.else))))))

(defconst *s_cond-*a-def*
  '(b* (((evoo-*a (expr_result test)) (eval_expr-*a env s.test))
        ((evo-*a choice) (val-case test.val
                           :v_bool (ev_normal test.val.val)
                           :otherwise (ev_error "Non-boolean test result in s_cond" s.test (list pos)))))
     (if choice
         (evtailcall-*a (eval_block-*a test.env s.then))
       (b* ((arboffset (add-arboffset-for-form arboffset (eval_block-*a test.env s.then))))
         (evtailcall-*a (eval_block-*a test.env s.else))))))

(defconst *s_for-*a-def*
  ;; Bind loop-iter and loop-idx before calling eval_for
  '(b* (((evoo-*a (expr_result startr)) (eval_expr-*a env s.start_e))
        ((evoo-*a (expr_result endr))   (eval_expr-*a env s.end_e))
        ((evoo-*a limit)                (eval_limit-*a env s.limit))
        (env (push_scope env))
        (env (declare_local_identifier env s.index_name startr.val))
        ;; Type constraints ensure that start and end are integers,
        ;; will do this here so we don't have to wrap them in values
        ((evob-*a startv) (v_to_int startr.val))
        ((evob-*a endv)   (v_to_int endr.val))
        (loop-iter 0)
        (loop-idx (arbaddr-offset->foroffset arboffset))
        ((evs-*a-special env2)
         (eval_for-*a env s.index_name limit
                      startv s.dir endv s.body))
        (env3 (pop_scope env2)))
     (evo_normal-*a (continuing env3))))

(defconst *s_while-*a-def*
  ;; Bind loop-iter and loop-idx before calling eval_loop
  '(b* (((evoo-*a limit) (eval_limit-*a env s.limit))
        (loop-iter 0)
        (loop-idx (arbaddr-offset->whileoffset arboffset)))
     (evtailcall-*a (eval_loop-*a env t limit s.test s.body))))

(defconst *s_repeat-*a-def*
  ;; Bind loop-iter and loop-idx before calls, build address for body eval
  '(b* (((evoo-*a limit) (eval_limit-*a env s.limit))
        ((evob-*a limit2) (tick_loop_limit limit))
        (loop-iter 0)
        (loop-idx (arbaddr-offset->repeatoffset arboffset))
        (arboffset (make-arbaddr-offset))
        (internal-arbaddr (cons (arbaddr-component-repeat loop-idx loop-iter) arbaddr))
        ((evs-*a env1) (eval_block-*a env s.body :arbaddr internal-arbaddr)))
     (evtailcall-*a (eval_loop-*a env1 nil limit2 s.test s.body :loop-iter 1))))

(defconst *t_named-*a-def*
  '(b* ((decl_types (static_env_global->declared_types
                     (global-env->static (env->global env))))
        (look (hons-assoc-equal ty.name decl_types))
        ((unless look)
         (evo_error-*a "Named type not found" (ty-fix x)
                       (list pos)))
        ((when (zp clk))
         (evo_error-*a "Clock ran out resolving named type"
                       (ty-fix x)
                       (list pos)))
        (type (ty-timeframe->ty (cdr look)))
        (arbaddr (cons (arbaddr-component-call
                        ty.name
                        (call-count-map-entry
                         ty.name (arbaddr-offset->callmap arboffset)))
                       arbaddr))
        (arboffset (make-arbaddr-offset)))
     (evtailcall-*a (resolve-ty-*a env type :clk (1- clk)))))

(defconst *eval_for-*a-def*
  ;; Bind arbaddr for body call, increment loop-iter before recurring
  '(b* (((when (for_loop-test v_start v_end dir))
         (evo_normal-*a (continuing env)))
        ((evo-*a limit1) (tick_loop_limit limit))
        (arboffset (make-arbaddr-offset))
        ((evs-*a-special env1)
         (let ((arbaddr (cons (arbaddr-component-for loop-idx loop-iter) arbaddr)))
           (eval_block-*a env body)))
        ((mv v_step env2) (eval_for_step env1 index_name v_start dir))
        (loop-iter (+ 1 (lnfix loop-iter))))
     (evtailcall-*a (eval_for-*a env2 index_name limit1 v_step dir v_end body))))

(defconst *eval_loop-*a-def*
  ;; Bind arbaddr for test/body calls, increment loop iter before reucrring
  '(b* ((arboffset (make-arbaddr-offset))
        (internal-arbaddr (cons (if is_while
                                    (arbaddr-component-while loop-idx loop-iter)
                                  (arbaddr-component-repeat loop-idx loop-iter))
                                arbaddr))
        ((evoo-*a (expr_result cres)) (eval_expr-*a env e_cond :arbaddr internal-arbaddr))
        (pos (expr->pos_start e_cond))
        ((evob-*a cbool) (v_to_bool cres.val))
        ((when (xor is_while cbool))
         (evo_normal-*a (continuing cres.env)))
        ((evob-*a limit1) (tick_loop_limit limit))
        ((evs-*a-special env2) (eval_block-*a cres.env body :arbaddr internal-arbaddr))
        ((when (zp clk))
         (evo_error-*a "DE_LE: Loop limit ran out" (stmt-fix body) (list (stmt->pos_start body))))
        (loop-iter (+ 1 (lnfix loop-iter))))
     (evtailcall-*a (eval_loop-*a env2 is_while limit1 e_cond body :clk (1- clk)))))

(define find_catcher-arbaddr-offset ((tenv static_env_global-p)
                                     (ty ty-p)
                                     (catchers catcherlist-p))
  :returns (offset arbaddr-offset-p)
  :guard (find_catcher tenv ty catchers)
  :prepwork ((local (in-theory (enable find_catcher))))
  (b* (((unless (mbt (consp catchers))) (make-arbaddr-offset))
       ((catcher c) (car catchers))
       ((when (same_named_type ty c.ty))
        (make-arbaddr-offset)))
    (arbaddr-offset-sum (catcher-arbaddr-offset c)
                        (find_catcher-arbaddr-offset tenv ty (cdr catchers)))))
  

(defconst *eval_catchers-*a-def*
  ;; Include offset for found catcher or full catcherlist in the case of otherwise,
  ;; to ensure nonoverlap of scopes
  '(b* (((throwdata throw))
        (catcher? (find_catcher (global-env->static (env->global env))
                                throw.ty catchers))
        ((unless catcher?)
         (b* (((unless otherwise)
               (evo_throwing-*a (throwdata-fix throw)
                                env backtrace))
              (arboffset (arbaddr-offset-sum (catcherlist-arbaddr-offset catchers)
                                             arboffset))
              ((evbind-*a blkres)
               (eval_block-*a env otherwise)))
           (evo-return-*a (rethrow_implicit throw blkres backtrace))))
        ((catcher c) catcher?)
        (arboffset (arbaddr-offset-sum (find_catcher-arbaddr-offset
                                        (global-env->static (env->global env))
                                        throw.ty catchers)
                                       arboffset))
        ((unless c.name)
         (b* (((evbind-*a blkres)
               (eval_block-*a env c.stmt)))
           (evo-return-*a (rethrow_implicit throw blkres backtrace))))
        (env2 (declare_local_identifier env c.name throw.val))
        ((evbind-nonrec-*a blkres)
         (b* (((evs-*a blkenv)
               (eval_block-*a env2 c.stmt))
              (env3 (remove_local_identifier blkenv c.name)))
           (evo_normal-*a (continuing env3)))))
     (evo-return-*a (rethrow_implicit throw blkres backtrace))))

(defconst *eval_subprogram-*a-arbaddr-argument*
  '(cons (arbaddr-component-call
          name
          (call-count-map-entry
           name (arbaddr-offset->callmap arboffset)))
         arbaddr))

(local (defun get-rid-of-aux-returns (x)
         (if (atom x)
             x
           (case-match x
             ((':returns ('mv first . &)) `(:returns ,first))
             (& (cons (get-rid-of-aux-returns (car x))
                      (get-rid-of-aux-returns (cdr x))))))))


;; Everyone has ((arbaddr arbaddr-p) 'arbaddr) ((arbmap arbmap-p) 'arbmap)
;; Almost everyone also has ((arboffset arbaddr-offset-p) 'arboffset) -- these
;; are the exceptions and what they have instead.
(local
 (defconst *nondefault-formals-*a*
   '((eval_for-*a        ((loop-iter natp) 'loop-iter) ((loop-idx natp) 'loop-idx))
     (eval_loop-*a       ((loop-iter natp) 'loop-iter) ((loop-idx natp) 'loop-idx))
     (eval_subprogram-*a))))
    


(local (defun replace-orac-with-new-formals (x)
         (if (atom x)
             x
           (case-match x
             (('define name formals . rest)
              `(define ,name
                 ,(let* ((formals (remove-equal '(orac 'orac) formals))
                         (nondefault-look (assoc name *nondefault-formals-*a*))
                         (new-formals (append (if nondefault-look
                                                  (cdr nondefault-look)
                                                '(((arboffset arbaddr-offset-p) 'arboffset)))
                                              '(((arbaddr arbaddr-p) 'arbaddr) ((arbmap arbmap-p) 'arbmap)))))
                    (append formals new-formals))
                 . ,rest))
             (& (cons (replace-orac-with-new-formals (car x))
                      (replace-orac-with-new-formals (cdr x))))))))

(local (defun replace-eval_subprogram-*a-call (x)
         (if (atom x)
             x
           (case-match x
             ((('eval_subprogram-*a . args) . rest)
              `((eval_subprogram-*a ,@args :arbaddr ,*eval_subprogram-*a-arbaddr-argument*)
                . ,rest))
             (& (cons (replace-eval_subprogram-*a-call (car x))
                      (replace-eval_subprogram-*a-call (cdr x))))))))



(local (defconst *asl-*a-xdoc*
         '(:parents (asl-interpreter-functions)
           :short "Modified version of @(see asl-interpreter-mutual-recursion) that uses a static alist rather than an oracle for a source of nondeterminism."
           :long "
<p>This is an automatically generated derived version of the ASL interpreter,
@(see asl-interpreter-mutual-recursion). Each function in the original mutual
recursion has an analogous function in this version, suffixed with
@('-*1') (the \"arbvals version\" of the function). </p>")))

(defconst *asl-interp-fns*
  (acl2::strip-cadrs
   (keep-define-forms-in-list
    (find-form-by-car
     'defines *asl-interpreter-mutual-recursion-command*))))


(defconst *eval-arbvals-substitution*
  (pair-suffixed (append *asl-interp-fns*
                         '(evo_normal pass-error evo_error evo_throwing evo-return
                                      evbind evbind-nonrec evoo evo evob evs evtailcall
                                      asl-interpreter-mutual-recursion))
                 '-*a))

(local
 (defun remove-orac-from-returns (x)
   (if (atom x)
       x
     (case-match x
       ((':returns ('mv x 'new-orac) . rest)
        `(:returns ,x . ,rest))
       (& (cons (remove-orac-from-returns (car x))
                (remove-orac-from-returns (cdr x))))))))


(local (defconst *asl-*a-function-replacements*
         `((eval_for-*a . ,*eval_for-*a-def*)
           (eval_loop-*a . ,*eval_loop-*a-def*)
           (eval_catchers-*a . ,*eval_catchers-*a-def*))))

(local (defconst *asl-*a-case-replacements*
         `((:e_arbitrary . ,*e_arbitrary-*a-def*)
           (:e_cond . ,*e_cond-*a-def*)
           (:s_cond . ,*s_cond-*a-def*)
           (:s_for . ,*s_for-*a-def*)
           (:s_while . ,*s_while-*a-def*)
           (:s_repeat . ,*s_repeat-*a-def*)
           (:t_named . ,*t_named-*a-def*))))




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
                             default-cdr)))

  ;; ---------------------------------------------------------------------------
  ;; Definition of the Arbvals ASL Interpreter (suffixed with *a)
  (with-output
    :off (event)
    (make-event
     (b* ((form *asl-interpreter-mutual-recursion-command*)
          ;; Strip out the events after the /// (theorem about resolved-p-of-resolve-ty)
          (form (strip-post-/// form))
          ;; Strip out xdoc
          (form (strip-xdoc form))
          ;; Add xdoc topic for mutual recursion
          (form (add-mutrec-xdoc *asl-*a-xdoc* form))
          ;; Add xdoc topic for each function
          (form (add-define-xdoc
                 "Arbvals version of @(see <NAME>); see @(see asl-interpreter-mutual-recursion-*a) for overview."
                 form))
          ;; Substitute function names with their -*a suffixed forms.
          (form (sublis *eval-arbvals-substitution* form))
          ;; Remove orac from returns
          (form (remove-orac-from-returns form))
          ;; Remove orac from formals and replace with new arbvals args
          (form (replace-orac-with-new-formals form))
          ;; Replace function bodies and cases with customized versions.
          ;; Eval_subprogram-*a keeps its body but needs a binding of arboffset around it.
          (form (replace-function-bodies form
                                         (cons `(eval_subprogram-*a . (let ((arboffset (make-arbaddr-offset)))
                                                                        ,(car (last (find-define 'eval_subprogram-*a form)))))
                                               *asl-*a-function-replacements*)))
          (form (replace-case-bodies form *asl-*a-case-replacements*))
          (form (replace-eval_subprogram-*a-call form))

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
       `(progn (defconst *asl-interpreter-mutual-recursion-*a-form* ',form)
               ,form)))))
