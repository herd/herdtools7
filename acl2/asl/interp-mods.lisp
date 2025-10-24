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

;; Common functions for making derivative versions of the interpreter.

(include-book "tools/templates" :Dir :system)

(defun strip-post-/// (x)
  (declare (xargs :mode :program))
  (if (atom x)
      x
    (if (eq (car x) '///)
        '(///)
      (cons (strip-post-/// (car x))
            (strip-post-/// (cdr x))))))

(defun strip-xdoc (x)
  (if (atom x)
      x
    (if (member-eq (car x) '(:short :long :parents))
        (strip-xdoc (cddr x))
      (cons (strip-xdoc (car x))
            (strip-xdoc (cdr x))))))

(defun add-mutrec-xdoc (xdoc x)
  (if (atom x)
      x
    (case-match x
      (('defines name . rest)
       `(defines ,name ,@xdoc . ,rest))
      (& (cons (add-mutrec-xdoc xdoc (car x))
               (add-mutrec-xdoc xdoc (cdr x)))))))


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
               (add-define-xdoc short (cdr x)))))))


(defun find-def-and-rename (name suffix x)
  (if (atom x)
      x
    (case-match x
      (('define !name . rest)
       `(define ,(intern-in-package-of-symbol
                  (concatenate 'string (symbol-name name) "-" (symbol-name suffix) "1")
                  'asl-pkg) . ,rest))
      (& (cons (find-def-and-rename name suffix (car x))
               (find-def-and-rename name suffix (cdr x)))))))


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
            (wrap-define-bodies macro (cdr x))))))

(defun add-define-guard (guard x)
  (if (atom x)
      x
    (case-match x
      (('define name formals . rest)
       (let ((guard-tail (member :guard rest)))
         (if guard-tail
             (let* ((guard-head (take (- (len rest) (len guard-tail)) rest))
                    (old-guard (cadr guard-tail))
                    (new-guard (if (and (consp old-guard)
                                        (eq (car old-guard) 'and))
                                   `(and ,guard . ,(cdr old-guard))
                                 `(and ,guard ,old-guard))))
               `(define ,name ,formals ,@guard-head :guard ,new-guard . ,(cddr guard-tail)))
           `(define ,name ,formals :guard ,guard . ,rest))))
      (& (cons (add-define-guard guard (car x))
               (add-define-guard guard (cdr x)))))))

(defun add-define-formals (new-formals x)
  (if (atom x)
      x
    (case-match x
      (('define name formals . rest)
       `(define ,name ,(append formals new-formals) . ,rest))
      (& (cons (add-define-formals new-formals (car x))
               (add-define-formals new-formals (cdr x)))))))


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
               (add-define-to-defines def (cdr x)))))))


(defun insert-after-/// (forms x)
  (if (atom x)
      x
    (if (eq (car x) '///)
        (cons '/// forms)
      (cons (insert-after-/// forms (car x))
            (insert-after-/// forms (cdr x))))))


(defun pair-suffixed (syms suffix)
  (if (atom syms)
      nil
    (cons (cons (car syms)
                (intern-in-package-of-symbol
                 (concatenate 'string (symbol-name (car syms))
                              (symbol-name suffix))
                 (car syms)))
          (pair-suffixed (cdr syms) suffix))))


(defun add-keyval-to-defines (keyval x)
  (if (atom x)
      x
    (case-match x
      (('defines arg . rest)
       (if (and (symbolp arg)
                (not (keywordp arg)))
           `(defines ,arg ,@keyval . ,rest)
         `(defines ,@keyval ,arg . ,rest)))
      (& (cons (add-keyval-to-defines keyval (car x))
               (add-keyval-to-defines keyval (cdr x)))))))


(defun keep-define-forms-in-list (x)
  (if (atom x)
      nil
    (if (and (consp (car x))
             (eq (caar x) 'define))
        (cons (car x) (keep-define-forms-in-list (cdr x)))
      (keep-define-forms-in-list (cdr x)))))

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
          (find-form-by-car-list car (cdr x))))))



(defun find-define (name x)
  (if (atom x)
      nil
    (case-match x
      (('define !name . &) x)
      (& (or (find-define name (car x))
             (find-define name (cdr x)))))))
