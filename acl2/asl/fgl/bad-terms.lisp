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
(include-book "centaur/fgl/helper-utils" :dir :system)
(include-book "defs")





;; ----------------------------------------------------------------------------------
;; Rules that cause an error when certain undesirable terms are created

(fgl::def-fgl-rewrite hons-assoc-equal-of-storage
  (implies (syntaxp (equal storage '(:g-var . storage)))
           (equal (hons-assoc-equal key storage)
                  (fgl::fgl-prog2 (fgl::fgl-error! :msg "lookup in storage")
                                  (fgl::abort-rewrite (hons-assoc-equal key storage))))))

(fgl::def-fgl-branch-merge func_result-merge-with-nil
  (equal (if test (func_result vals env) nil)
         (fgl::fgl-prog2 (fgl::fgl-error! :msg "func_result merge with nil")
                         (fgl::abort-rewrite (if test (func_result vals env) nil)))))

(fgl::def-fgl-rewrite hons-assoc-equal-of-record
  (equal (hons-assoc-equal key (v_record->rec x))
         (fgl::fgl-prog2 (fgl::fgl-error! :msg "lookup in record")
                         (fgl::abort-rewrite (hons-assoc-equal key (v_record->rec x))))))

(fgl::def-fgl-rewrite put-pairs-of-cons
  (equal (val-imap-put-pairs lst (cons a b))
         (fgl::fgl-prog2 (fgl::fgl-error! :msg "put-pairs of cons")
                         (fgl::abort-rewrite (val-imap-put-pairs lst (cons a b))))))

(fgl::def-fgl-rewrite cons-of-val-imap-add-pairs
  (equal (cons x (val-imap-add-pairs y z))
         (fgl::fgl-prog2 (fgl::fgl-error! :msg "cons of add-pairs")
                         (fgl::abort-rewrite (cons x (val-imap-add-pairs y z))))))


(fgl::def-fgl-rewrite error-on-omap-update
  (equal (omap::update k v x)
         (fgl::fgl-prog2 (fgl::fgl-error! :msg "omap update")
                         (fgl::abort-rewrite (omap::update k v x)))))




                                          






