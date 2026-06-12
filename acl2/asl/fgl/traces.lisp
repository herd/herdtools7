;;****************************************************************************;;
;;                              ACL2 ASL Library                              ;;
;;****************************************************************************;;
;;
;; SPDX-FileCopyrightText: Copyright 2026 Arm Limited and/or its affiliates <open-source-office@arm.com>
;; SPDX-License-Identifier: BSD-3-Clause
;; 
;; ****************************************************************************;;



(in-package "ASL")
(include-book "trace-interp" :dir :acl2asl)
(include-book "defs")
(include-book "centaur/fgl/def-fgl-rewrite" :dir :system)
(include-book "centaur/fgl/fgl-object" :dir :system)
(include-book "centaur/fgl/fty" :dir :system)
(include-book "centaur/fgl/config" :dir :system)
(include-book "centaur/fgl/checks" :dir :system)
(local (include-book "std/lists/take" :dir :system))


(fty::add/remove-fgl-rules-for-fty-sum asl-trace)
;; In hopes that this will make IF merge rules easier, don't make concrete values for traces.
(fgl::disable-execution calltrace)
(fgl::disable-execution stmttrace)
(fgl::disable-execution call-trace-output)
(fgl::disable-execution stmt-trace-output)
(fgl::disable-execution call-trace-abort-before-output)
(fgl::disable-execution stmt-trace-abort-before-output)



;; (define maybe-cons (test first rest)
;;   (if test
;;       (cons first rest)
;;     rest))

(fty::defprod conditional-obj ((test) (obj)))
(fty::add/remove-fgl-rules-for-fty-sum conditional-obj)

(fty::deflist conditional-objlist :elt-type conditional-obj :true-listp t)

(define conditional-objs->list ((x conditional-objlist-p))
  (if (atom x)
      nil
    (let ((rest (conditional-objs->list (cdr x))))
      (if (conditional-obj->test (car x))
          (cons (conditional-obj->obj (car x)) rest)
        rest)))
  ///
  (fgl::remove-fgl-rewrite conditional-objs->list))



;; (defun maybe-obj (cond obj)
;;   (and cond obj))

;; (defun maybe-objs->list (lst)
;;   (if (atom lst)
;;       nil
;;     (let ((rest (maybe-objs->list (cdr lst))))
;;       (if (car lst)
;;           (cons (car lst) rest)
;;         rest))))

;; (fgl::def-fgl-branch-merge merge-maybe-obj-with-nil
;;   (equal (if cond1 (maybe-obj cond2 x) nil)
;;          (maybe-obj (and cond1 cond2) x))
;;   :hints(("Goal" :in-theory (enable maybe-obj))))

;; (fgl::remove-fgl-rewrite maybe-obj)
;; (fgl::remove-fgl-rewrite maybe-objs->list)

(define conditionalize-conditional-objs (cond (objs conditional-objlist-p))
  :returns (new-objs conditional-objlist-p)
  (if (atom objs)
      nil
    (cons (conditional-obj (and cond (conditional-obj->test (car objs)))
                           (conditional-obj->obj (car objs)))
          (conditionalize-conditional-objs cond (cdr objs))))
  ///
  (defret conditional-objs->list-of-<fn>
    (equal (conditional-objs->list new-objs)
           (and cond (conditional-objs->list objs)))
    :hints(("Goal" :in-theory (enable conditional-objs->list)))))


(define conditionalize-conditional-objs-binder (ans cond x)
  :verify-guards nil
  (let* ((spec (conditionalize-conditional-objs cond x)))
    (if (equal (conditional-objs->list spec) (conditional-objs->list ans))
        ans
      spec))
  ///
  (defthm conditionalize-conditional-objs-binder-correct
    (equal (conditional-objs->list (conditionalize-conditional-objs-binder ans cond x))
           (conditional-objs->list (conditionalize-conditional-objs cond x))))

  (fgl::def-fgl-rewrite conditionalize-objs->list-of-conditionalize-conditional-objs
    (equal (conditional-objs->list (conditionalize-conditional-objs cond x))
           (conditional-objs->list (conditionalize-conditional-objs-binder ans cond x))))

  (fgl::def-fgl-brewrite conditionalize-conditional-objs-binder-impl
    (implies (equal ans (if (atom x)
                            nil
                          (let* ((test (and cond (conditional-obj->test (car x))))
                                 (rest (conditionalize-conditional-objs-binder rest cond (cdr x))))
                            (if (fgl::check-true falsep (not test))
                                rest
                              (cons (conditional-obj test (conditional-obj->obj (car x)))
                                    rest)))))
             (equal (conditionalize-conditional-objs-binder ans cond x)
                    ans))
    :hints(("Goal" :in-theory (enable conditionalize-conditional-objs
                                      conditional-objs->list))))

  (fgl::remove-fgl-rewrite conditionalize-conditional-objs))
                                
    
         

(define conditionalize-objs (cond objs)
  :returns (condlst conditional-objlist-p)
  (if (atom objs)
      nil
    (cons (conditional-obj cond (car objs))
          (conditionalize-objs cond (cdr objs))))
  ///
  (defret conditional-objs->list-of-<fn>
    (equal (conditional-objs->list condlst)
           (and cond (true-list-fix objs)))
    :hints(("Goal" :in-theory (enable conditional-objs->list))))

  (fgl::def-fgl-rewrite conditionalize-objs-of-conditional-objs->list
    (equal (conditional-objs->list
            (conditionalize-objs cond (conditional-objs->list objs)))
           (conditional-objs->list (conditionalize-conditional-objs cond objs)))
    :hints(("Goal" :in-theory (enable conditionalize-conditional-objs
                                      conditional-objs->list))))

  (fgl::remove-fgl-rewrite conditionalize-objs)

  (fgl::def-fgl-rewrite conditionalize-objs-of-empty-list
    (equal (conditionalize-objs test nil) nil)))

(fgl::def-fgl-rewrite cons-of-calltrace-nil
  (equal (cons (calltrace name fn params args subtraces result pos) nil)
         (conditional-objs->list (list (conditional-obj t (calltrace name fn params args subtraces result pos)))))
  :hints(("Goal" :in-theory (enable conditional-objs->list))))

(fgl::def-fgl-rewrite cons-of-stmttrace-nil
  (equal (cons (stmttrace name stmt initial-vars subtraces result final-vars) nil)
         (conditional-objs->list (list (conditional-obj t (stmttrace name stmt initial-vars subtraces result final-vars)))))
  :hints(("Goal" :in-theory (enable conditional-objs->list))))

(fgl::def-fgl-rewrite cons-of-calltrace-conditional-objs->list
  (equal (cons (calltrace name fn params args subtraces result pos)
               (conditional-objs->list rest))
         (conditional-objs->list (cons (conditional-obj t (calltrace name fn params args subtraces result pos)) rest)))
  :hints(("Goal" :in-theory (enable conditional-objs->list))))

(fgl::def-fgl-rewrite cons-of-stmttrace-conditional-objs->list
  (equal (cons (stmttrace name stmt initial-vars subtraces result final-vars)
               (conditional-objs->list rest))
         (conditional-objs->list (cons (conditional-obj t (stmttrace name stmt initial-vars subtraces result final-vars)) rest)))
  :hints(("Goal" :in-theory (enable conditional-objs->list))))

(fgl::def-fgl-rewrite append-of-conditional-objs->list
  (equal (append (conditional-objs->list x) (conditional-objs->list y))
         (conditional-objs->list (append x y)))
  :hints(("Goal" :in-theory (enable conditional-objs->list))))

(fgl::def-fgl-rewrite append-of-conditional-objs->list-2
  (equal (append (conditional-objs->list x) (conditional-objs->list y) z)
         (append (conditional-objs->list (append x y)) z))
  :hints(("Goal" :in-theory (enable conditional-objs->list))))

(fgl::def-fgl-rewrite append-of-conditional-objs->list-nil
  (equal (append (conditional-objs->list x) nil)
         (conditional-objs->list x)))

(defthm conditional-objs->list-of-append
  (equal (conditional-objs->list (append x y))
         (append (conditional-objs->list x) (conditional-objs->list y)))
  :hints(("Goal" :in-theory (enable conditional-objs->list))))


;; (fgl::def-fgl-rewrite maybe-obj-of-maybe-obj
;;   (equal (maybe-obj cond1 (maybe-obj cond2 obj))
;;          (maybe-obj (and cond1 cond2) obj)))

(defthm conditional-objs->list-of-true-list-fix
  (equal (conditional-objs->list (true-list-fix x))
         (conditional-objs->list x))
  :hints(("Goal" :in-theory (enable conditional-objs->list))))

(local (defthmd append-take-suffix
         (implies (and (acl2::suffixp suff lst)
                       (equal len (- (len lst) (len suff))))
                  (equal (append (take len lst) suff) lst))
         :hints(("Goal" :in-theory (enable acl2::suffixp)))))

(fgl::def-fgl-branch-merge merge-conditional-objs->list
  (equal (if test (conditional-objs->list lst1) (conditional-objs->list lst2))
         (b* ((suff (common-suffix-binder suff lst1 lst2))
              (suff-len (len suff))
              (lst1-prefix (take (- (len lst1) suff-len) lst1))
              (lst2-prefix (take (- (len lst2) suff-len) lst2)))
           
           (append (conditional-objs->list
                    (conditionalize-conditional-objs test lst1-prefix))
                   (conditional-objs->list
                    (conditionalize-conditional-objs (not test) lst2-prefix))
                   (conditional-objs->list suff))))
  :hints (("goal" :do-not-induct t
           :in-theory (enable common-suffix-binder))
          (and stable-under-simplificationp
               '(:in-theory (e/d (append-of-conditional-objs->list
                                  append-take-suffix)
                                 (conditional-objs->list-of-append))))))

(fgl::def-fgl-branch-merge merge-conditional-objs->list-nil
  (equal (if test (conditional-objs->list lst1) nil)
         (conditional-objs->list (conditionalize-conditional-objs test lst1))))


;; (define maybe-cons (x y)
;;   (if x
;;       (cons x y)
;;     y)
;;   ///
;;   (fgl::remove-fgl-rewrite maybe-cons)
;;   (fgl::def-fgl-rewrite maybe-cons-onto-maybe-objs->list
;;     (equal (maybe-cons x (maybe-objs->list y))
;;            (maybe-objs->list (cons x y)))
;;     :hints(("Goal" :in-theory (enable maybe-objs->list))))

;;   (fgl::def-fgl-rewrite maybe-cons-onto-cons
;;     (implies y
;;              (equal (maybe-cons x (cons y z))
;;                     (maybe-cons x (maybe-cons y z))))))

;; (fgl::def-fgl-branch-merge merge-maybe-objs->list-cons
;;   (implies x
;;            (equal (if test (maybe-objs->list lst) (cons x y))
;;                   (if test (maybe-objs->list lst) (maybe-cons x y))))
;;   :hints(("Goal" :in-theory (enable maybe-cons))))

(fgl::def-fgl-branch-merge merge-conditional-objs->list-cons
  (implies (true-listp y)
           (equal (if test (conditional-objs->list lst) (cons x y))
                  (if test (conditional-objs->list lst)
                    (conditional-objs->list
                     (conditionalize-objs t (cons x y)))))))


         
(fgl::def-fgl-rewrite rev-of-conditional-objs->list
  (equal (acl2::rev (conditional-objs->list x))
         (conditional-objs->list (acl2::rev x)))
  :hints(("Goal" :in-theory (enable conditional-objs->list))))

;; (define maybe-obj-resolve (x)
;;   :enabled t
;;   x
;;   ///
;;   (fgl::remove-fgl-rewrite maybe-obj-resolve)
;;   (fgl::def-fgl-rewrite maybe-obj-resolve-of-maybe-obj
;;     (equal (maybe-obj-resolve (maybe-obj cond obj))
;;            (and cond obj)))

;;   (fgl::def-fgl-rewrite maybe-obj-resolve-of-non-maybe-obj
;;     (implies (syntaxp (fgl::fgl-object-case obj
;;                         :g-apply (not (equal obj.fn 'maybe-obj))
;;                         :otherwise t))
;;              (equal (maybe-obj-resolve obj) obj))))


(fgl::def-fgl-rewrite consp-of-conditional-objs->list
  (and (iff (consp (conditional-objs->list nil)) nil)
       (iff (consp (conditional-objs->list (cons a b)))
            (or (conditional-obj->test a)
                (consp (conditional-objs->list b)))))
  :hints(("Goal" :in-theory (enable conditional-objs->list))))

(fgl::def-fgl-rewrite conditional-objs->list-under-iff
  (and (iff (conditional-objs->list nil) nil)
       (iff (conditional-objs->list (cons a b))
            (or (conditional-obj->test a)
                (conditional-objs->list b))))
  :hints(("Goal" :in-theory (enable conditional-objs->list))))


(fgl::def-fgl-rewrite car-of-conditional-objs->list
  (and (equal (car (conditional-objs->list nil)) nil)
       (equal (car (conditional-objs->list (cons a b)))
              (b* (((conditional-obj a)))
                (if a.test
                    a.obj
                  (car (conditional-objs->list b))))))
  :hints(("Goal" :in-theory (enable conditional-objs->list))))

(fgl::def-fgl-rewrite cdr-of-conditional-objs->list
  (and (equal (cdr (conditional-objs->list nil)) nil)
       (equal (cdr (conditional-objs->list (cons a b)))
              (b* (((conditional-obj a))
                   (rest (conditional-objs->list b)))
                (if a.test
                    rest
                  (cdr rest)))))
  :hints(("Goal" :in-theory (enable conditional-objs->list))))

(fgl::def-fgl-rewrite asl-tracelist-find-calls-of-conditional-objs->list
  (and (equal (asl-tracelist-find-calls fn (conditional-objs->list nil)) nil)
       (equal (asl-tracelist-find-calls fn (conditional-objs->list (cons a b)))
              (append (conditional-objs->list
                       (conditionalize-objs
                        (conditional-obj->test a)
                        (asl-trace-find-calls fn (conditional-obj->obj a))))
                      (asl-tracelist-find-calls fn (conditional-objs->list b)))))
  :hints(("Goal" :in-theory (enable asl-tracelist-find-calls
                                    conditional-objs->list))))

;; (fgl::def-fgl-rewrite maybe-obj-under-iff
;;   (iff (maybe-obj cond obj)
;;        (and cond obj)))

;; (fgl::def-fgl-rewrite calltrace-accs-of-maybe-obj
;;   (and (equal (calltrace->name (maybe-obj cond obj))
;;               (and cond (calltrace->name obj)))
;;        (equal (calltrace->fn (maybe-obj cond obj))
;;               (if cond (calltrace->fn obj) (identifier-fix nil)))
;;        (equal (calltrace->params (maybe-obj cond obj))
;;               (and cond (calltrace->params obj)))
;;        (equal (calltrace->args (maybe-obj cond obj))
;;               (and cond (calltrace->args obj)))
;;        (equal (calltrace->subtraces (maybe-obj cond obj))
;;               (and cond (calltrace->subtraces obj)))
;;        (equal (calltrace->result (maybe-obj cond obj))
;;               (if cond (calltrace->result obj) (ev_normal nil)))
;;        (equal (calltrace->pos (maybe-obj cond obj))
;;               (if cond (calltrace->pos obj) (posn-fix nil))))
;;   :hints(("Goal" :in-theory (enable maybe-obj))))

;; (fgl::def-fgl-rewrite stmttrace-accs-of-maybe-obj
;;   (and (equal (stmttrace->name (maybe-obj cond obj))
;;               (and cond (stmttrace->name obj)))
;;        (equal (stmttrace->stmt (maybe-obj cond obj))
;;               (if cond (stmttrace->stmt obj) (stmt-fix nil)))
;;        (equal (stmttrace->initial-vars (maybe-obj cond obj))
;;               (and cond (stmttrace->initial-vars obj)))
;;        (equal (stmttrace->subtraces (maybe-obj cond obj))
;;               (and cond (stmttrace->subtraces obj)))
;;        (equal (stmttrace->result (maybe-obj cond obj))
;;               (if cond (stmttrace->result obj) (ev_normal :continuing)))
;;        (equal (stmttrace->final-vars (maybe-obj cond obj))
;;               (and cond (stmttrace->final-vars obj))))
;;   :hints(("Goal" :in-theory (enable maybe-obj))))

;; (fgl::def-fgl-rewrite asl-trace-accs-of-maybe-obj
;;   (and (equal (asl-trace-kind (maybe-obj cond obj))
;;               (if cond
;;                   (asl-trace-kind obj)
;;                 :calltrace))
;;        (equal (asl-trace-p (maybe-obj cond obj))
;;               (and cond (asl-trace-p obj))))
;;   :hints(("Goal" :in-theory (enable maybe-obj))))

;; (define len-of-maybe-objs (x)
;;   (if (atom x)
;;       0
;;     (+ (if (maybe-obj-resolve (car x)) 1 0) (len-of-maybe-objs (cdr x))))
;;   ///
;;   (fgl::def-fgl-rewrite len-of-maybe-objs->list
;;     (equal (len (maybe-objs->list x))
;;            (len-of-maybe-objs x))))

(fgl::def-fgl-rewrite len-of-conditional-objs->list
  (and (equal (len (conditional-objs->list nil)) 0)
       (equal (len (conditional-objs->list (cons a b)))
              (let ((rest (len (conditional-objs->list b))))
                (+ (if (conditional-obj->test a) 1 0) rest))))
  :hints(("Goal" :in-theory (enable conditional-objs->list))))
         

;; Replace append with something specialized?
(fgl::def-fgl-rewrite asl-tracelist-find-by-name-of-conditional-objs->list
  (and (equal (asl-tracelist-find-by-name name (conditional-objs->list nil)) nil)
       (equal (asl-tracelist-find-by-name name (conditional-objs->list (cons a b)))
              (append (conditional-objs->list
                       (conditionalize-objs
                        (conditional-obj->test a)
                        (asl-trace-find-by-name name (conditional-obj->obj a))))
                      (asl-tracelist-find-by-name name (conditional-objs->list b)))))
  :hints(("Goal" :in-theory (enable asl-tracelist-find-by-name
                                    conditional-objs->list))))

(fgl::remove-fgl-rewrite asl-tracelist-find-by-name)

;; (define asl-maybe-tracelist-p (x)
;;   (if (atom x)
;;       t
;;     (and (or (not (car x))
;;              (asl-trace-p (car x)))
;;          (asl-maybe-tracelist-p (cdr x))))
;;   ///
;;   (fgl::remove-fgl-rewrite asl-maybe-tracelist-p)
;;   (fgl::def-fgl-rewrite asl-maybe-tracelist-p-of-cons-maybe-obj
;;     (equal (asl-maybe-tracelist-p (cons (maybe-obj cond x) rest))
;;            (and (or (not cond) (asl-trace-p x) (not x))
;;                 (asl-maybe-tracelist-p rest)))))

;; (fgl::def-fgl-rewrite asl-tracelist-p-of-maybe-objs->list
;;   (implies (asl-maybe-tracelist-p x)
;;            (asl-tracelist-p (maybe-objs->list x)))
;;   :hints(("Goal" :in-theory (enable maybe-objs->list
;;                                     asl-maybe-tracelist-p))))

;; (fgl::add-fgl-rewrite asl-tracelist-fix-when-asl-tracelist-p)

;; (fgl::def-fgl-branch-merge merge-singleton-maybe-obj-with-nil
;;   (implies (and (implies test cond) obj)
;;            (equal (if test (list (maybe-obj cond obj)) nil)
;;                   (maybe-objs->list (list (maybe-obj (and test cond) obj)))))
;;   :hints(("Goal" :in-theory (enable maybe-objs->list))))

(fgl::disable-if-merge-args conditional-objs->list)
(fgl::disable-if-merge-args conditional-obj)

(fgl::enable-split-ifs calltrace->args$inline)
(fgl::enable-split-ifs calltrace->result$inline)
(fgl::enable-split-ifs asl-trace-kind$inline)


(define asl-conditional-tracelist-p ((x conditional-objlist-p))
  (if (atom x)
      t
    (and (asl-trace-p (conditional-obj->obj (car x)))
         (asl-conditional-tracelist-p (cdr x))))
  ///
  (fgl::def-fgl-rewrite asl-tracelist-p-of-conditional-objs->list
    (implies (asl-conditional-tracelist-p x)
             (asl-tracelist-p (conditional-objs->list x)))
    :hints(("Goal" :in-theory (enable conditional-objs->list)))))

(fgl::add-fgl-rewrite asl-tracelist-fix-when-asl-tracelist-p)


(fgl::def-fgl-rewrite calltrace-under-iff
  (iff (calltrace name fn params args subtraces result pos) t))


(fgl::def-fgl-rewrite v_record-under-iff
  (iff (v_record rec) t))
