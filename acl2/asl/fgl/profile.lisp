;;****************************************************************************;;
;;                              ACL2 ASL Library                              ;;
;;****************************************************************************;;
;;
;; SPDX-FileCopyrightText: Copyright 2026 Arm Limited and/or its affiliates <open-source-office@arm.com>
;; SPDX-License-Identifier: BSD-3-Clause
;; 
;; ****************************************************************************;;

(in-package "ASL")

(include-book "defs")
(include-book "trace-interp" :dir :acl2asl)
(include-book "defsort/defsort" :dir :system)
(include-book "centaur/fgl/fancy-ev" :dir :system)
(include-book "centaur/fgl/annotation" :dir :system)
(local (in-theory (disable (tau-system))))

(deftypes callgraph
  (defprod callgraph-node
    ((fn stringp :rule-classes :type-prescription)
     (subcalls callgraph-nodelist)
     (stepcount natp :rule-classes :type-prescription))
    :measure (+ 1 (* 2 (acl2-count x)))
    :layout :list)
  (deflist callgraph-nodelist :elt-type callgraph-node :true-listp t
    :measure (* 2 (acl2-count x))))

(define callgraph-node-cheap (fn subcalls stepcount)
  (list fn subcalls stepcount))

(deflist callgraph-framelist :elt-type callgraph-nodelist :true-listp t)

(defines callgraph-call-count
  (define callgraph-count ((x callgraph-node-p))
    :measure (callgraph-node-count x)
    (+ 1 (callgraphlist-count (callgraph-node->subcalls x))))
  (define callgraphlist-count ((x callgraph-nodelist-p))
    :measure (callgraph-nodelist-count x)
    (if (atom x)
        0
      (+ (callgraph-count (car x))
         (callgraphlist-count (cdr x))))))

(defines callgraph-count-fn-calls
  (define callgraph-count-fn-calls ((fn stringp)
                                    (x callgraph-node-p))
    :measure (callgraph-node-count x)
    (b* (((callgraph-node x)))
      (+ (if (equal x.fn (mbe :logic (acl2::str-fix fn) :exec fn)) 1 0)
         (callgraphlist-count-fn-calls fn x.subcalls))))
  (define callgraphlist-count-fn-calls ((fn stringp)
                                        (x callgraph-nodelist-p))
    :measure (callgraph-nodelist-count x)
    (if (atom x)
        0
      (+ (callgraph-count-fn-calls fn (car x))
         (callgraphlist-count-fn-calls fn (cdr x))))))

(defines callgraph-count-fn-subcalls
  (define callgraph-count-fn-subcalls ((fn stringp)
                                    (x callgraph-node-p))
    :measure (callgraph-node-count x)
    (b* (((callgraph-node x)))
      (if (equal x.fn (mbe :logic (acl2::str-fix fn) :exec fn))
          (callgraph-count x)
        (callgraphlist-count-fn-subcalls fn x.subcalls))))
  (define callgraphlist-count-fn-subcalls ((fn stringp)
                                           (x callgraph-nodelist-p))
    :measure (callgraph-nodelist-count x)
    (if (atom x)
        0
      (+ (callgraph-count-fn-subcalls fn (car x))
         (callgraphlist-count-fn-subcalls fn (cdr x))))))

(defprod callgraphstats-entry
  ((calls natp :rule-classes :type-prescription
          :default 0)
   (subcalls natp :rule-classes :type-prescription
             :default 0)
   (stepcount natp :rule-classes :type-prescription
              :default 0)
   (recp booleanp))
  :layout :fulltree)

(make-event
 `(define empty-callgraphstats-entry ()
    ',(make-callgraphstats-entry)))

(fty::defmap callgraphstats :key-type string :val-type callgraphstats-entry :true-listp t)


(define callgraphstats-comparablep (x)
  (and (consp x)
       (stringp (car x))
       (callgraphstats-entry-p (cdr x))))

(encapsulate nil
  (define callgraphstats-subcalls-compare ((x callgraphstats-comparablep)
                                          (y callgraphstats-comparablep))
  :guard-hints (("goal" :in-theory (enable callgraphstats-comparablep)))
  (< (callgraphstats-entry->subcalls (cdr x))
     (callgraphstats-entry->subcalls (cdr y))))
  
  (local (in-theory (enable callgraphstats-subcalls-compare)))
  (acl2::defsort callgraphstats-subcalls-sort
    :prefix callgraphstats-subcalls
    :compare< callgraphstats-subcalls-compare
    :comparablep callgraphstats-comparablep
    ;; :comparable-listp callgraphstats-p
    :true-listp t)

  (defthm callgraphstats-subcalls-list-p-elim
    (equal (callgraphstats-subcalls-list-p x)
           (callgraphstats-p x))
    :hints(("Goal" :in-theory (enable callgraphstats-p
                                      callgraphstats-comparablep
                                      callgraphstats-subcalls-list-p)))))

(encapsulate nil
  (define callgraphstats-stepcount-compare ((x callgraphstats-comparablep)
                                          (y callgraphstats-comparablep))
  :guard-hints (("goal" :in-theory (enable callgraphstats-comparablep)))
  (< (callgraphstats-entry->stepcount (cdr x))
     (callgraphstats-entry->stepcount (cdr y))))
  
  (local (in-theory (enable callgraphstats-stepcount-compare)))
  (acl2::defsort callgraphstats-stepcount-sort
    :prefix callgraphstats-stepcount
    :compare< callgraphstats-stepcount-compare
    :comparablep callgraphstats-comparablep
    ;; :comparable-listp callgraphstats-p
    :true-listp t)

  (defthm callgraphstats-stepcount-list-p-elim
    (equal (callgraphstats-stepcount-list-p x)
           (callgraphstats-p x))
    :hints(("Goal" :in-theory (enable callgraphstats-p
                                      callgraphstats-comparablep
                                      callgraphstats-stepcount-list-p)))))

(defines callgraph-collect-stats
  (define callgraph-collect-stats ((x callgraph-node-p)
                                   (acc callgraphstats-p))
    :measure (callgraph-node-count x)
    :verify-guards nil
    :returns (mv (count natp :rule-classes :type-prescription)
                 (new-acc callgraphstats-p))
    (b* (((callgraph-node x))
         (look (hons-get x.fn acc))
         ((callgraphstats-entry entry1)
          (if look (cdr look) (empty-callgraphstats-entry)))
         (acc (hons-acons x.fn (change-callgraphstats-entry entry1 :recp t) acc))
         ((mv count acc) (callgraphlist-collect-stats x.subcalls acc))
         ((callgraphstats-entry entry2)
          (cdr (hons-get x.fn acc))))
      (mv (+ 1 (lnfix count))
          (hons-acons x.fn (make-callgraphstats-entry
                            :calls (+ 1 entry2.calls)
                            :subcalls (if entry1.recp
                                          entry2.subcalls
                                        (+ count entry2.subcalls))
                            :stepcount (if entry1.recp
                                           entry2.stepcount
                                         (+ x.stepcount entry2.stepcount))
                            :recp entry1.recp)
                      acc))))

  (define callgraphlist-collect-stats ((x callgraph-nodelist-p)
                                       (acc callgraphstats-p))
    :measure (callgraph-nodelist-count x)
    :returns (mv (count natp :rule-classes :type-prescription)
                 (new-acc callgraphstats-p))
    (b* (((when (atom x))
          (mv 0 (callgraphstats-fix acc)))
         ((mv count1 acc)
          (callgraph-collect-stats (car x) acc))
         ((mv count2 acc)
          (callgraphlist-collect-stats (cdr x) acc)))
      (mv (+ count1 count2) acc)))
  ///
  (std::defret-mutual callgraph-collect-stats-preserves-lookup
    (defret <fn>-preserves-lookup
      (implies (and (hons-assoc-equal fn acc)
                    (stringp fn))
               (hons-assoc-equal fn new-acc))
      :hints ('(:expand (<call>)))
      :fn callgraph-collect-stats)
    (defret <fn>-preserves-lookup
      (implies (and (hons-assoc-equal fn acc)
                    (stringp fn))
               (hons-assoc-equal fn new-acc))
      :hints ('(:expand (<call>)))
      :fn callgraphlist-collect-stats))
  (verify-guards callgraph-collect-stats))

(define callgraph-stats ((x callgraph-node-p))
  (b* (((mv count stats) (callgraph-collect-stats x nil)))
    (list count
          (set::mergesort (fast-alist-clean stats)))))


(define callgraphlist-stepcounts ((x callgraph-nodelist-p))
  (if (atom x)
      0
    (+ (callgraph-node->stepcount (car x))
       (callgraphlist-stepcounts (cdr x)))))

(defines callgraph-check-stepcounts
  (define callgraph-check-stepcounts ((x callgraph-node-p))
    :returns (bad-nodes callgraph-nodelist-p)
    :measure (callgraph-node-count x)
    :verify-guards nil
    (b* (((callgraph-node x)))
      (append (and (< x.stepcount (callgraphlist-stepcounts x.subcalls))
                   (list (callgraph-node-fix x)))
              (callgraphlist-check-stepcounts x.subcalls))))
  (define callgraphlist-check-stepcounts ((x callgraph-nodelist-p))
    :measure (callgraph-nodelist-count x)
    :returns (bad-nodes callgraph-nodelist-p)
    (if (atom x)
        nil
      (append (callgraph-check-stepcounts (car x))
              (callgraphlist-check-stepcounts (cdr x)))))
  ///
  (Verify-guards callgraphlist-check-stepcounts))
      
                      


(define set-callgraph (val state)
  :returns new-state
  (f-put-global ':callgraph val state)
  ///
  (defret w-of-<fn>
    (equal (w new-state) (w state))
    :hints(("Goal" :in-theory (enable w))))

  (fgl::fancy-ev-add-primitive set-callgraph t))

(define set-final-callgraph (val state)
  :returns new-state
  (f-put-global ':final-callgraph val state)
  ///
  (defret w-of-<fn>
    (equal (w new-state) (w state))
    :hints(("Goal" :in-theory (enable w))))

  (fgl::fancy-ev-add-primitive set-final-callgraph t))

(fgl::fancy-ev-add-primitive fgl::interp-st->steplimit$inline t)


;; Collect a tree of all eval_subprogram calls.
;;  - When entering a call, push an empty frame onto the stack.
;;  - When exiting a call, grab the top frame off the stack; this is a list of calls. Create a call by  it with the call's function name,
;;    and add it to the 
;; NOTE: This (and other functions that use annotations) should be placed LAST
;; in the file relative to other rules rewriting eval_subprogram-*t, so that they
;; will be tried first when rewriting an eval_subprogram-*t call.
(defthm eval_subprogram-*t-track-callgraph
  (implies (and (fgl::bind-fn-annotation annot 'eval_subprogram-*t-fn)
                ;; make sure this rule doesn't loop
                (not (member :callgraph annot)))
           (equal (eval_subprogram-*t env fn params args)
                  (fgl::fgl-prog2
                   (fgl::syntax-interp
                    (set-callgraph
                     (cons nil
                           (and (boundp-global :callgraph state)
                                (f-get-global :callgraph state)))
                     state))
                   (let* ((step0 (fgl::syntax-bind step0 (fgl::interp-st->steplimit 'fgl::interp-st)))
                          (res (fgl::annotate `(:callgraph t . ,annot)
                                              (eval_subprogram-*t env fn params args)))
                          )
                     (fgl::fgl-prog2
                      (fgl::syntax-interp
                       (b* ((step1 (fgl::interp-st->steplimit 'fgl::interp-st))
                            (graph (f-get-global :callgraph state))
                            (frame (car graph))
                            (stepcount (- step0 step1))
                            (node (callgraph-node-cheap fn frame stepcount)))
                         (if (consp (cdr graph))
                             (set-callgraph (cons (cons node
                                                        (cadr graph))
                                                  (cddr graph))
                                            state)
                           (pprogn (set-callgraph nil state)
                                   (set-final-callgraph node state)))))
                      res)))))
  :rule-classes nil)



;; (fgl::remove-fgl-rewrite eval_subprogram-*t-track-callgraph)
;; (fgl::add-fgl-rewrite eval_subprogram-*t-track-callgraph)
;; (fgl-reorder-eval_subprogram-*t-rules)
;; (fgl::remove-fgl-rewrite eval_subprogram-*t-print)


;; Before each run:

;; (assign :callgraph nil)
;; (assign :final-callgraph nil)

;; Run the target proof

;; (callgraphstats-stepcount-sort (cadr (callgraph-stats (@ :final-callgraph))))

;; or

;; (callgraphstats-subcalls-sort (cadr (callgraph-stats (@ :final-callgraph))))
