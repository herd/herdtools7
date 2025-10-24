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

;; TODO: Document and add the stdlib proofs.

(include-book "toplevel")
(include-book "trace-interp")
(include-book "proofs/stdlib/top")
(include-book "xdoc/save" :dir :system)
(include-book "oslib/date" :dir :system)
(include-book "centaur/fty/top" :dir :system)
(defttag :manual-info)

(value-triple (acl2::tshell-ensure))
(defconsts (*herdtools-git-hash* state)
  (b* (((mv ?ok lines state)
        (acl2::tshell-call "git rev-parse HEAD")))
    (mv (subseq (car lines) 0 8) state)))

(defconsts (*acl2-git-hash* state)
  (b* ((dir (acl2::include-book-dir :system state))
       ((mv ?ok lines state)
        (acl2::tshell-call (concatenate 'string
                                        "cd " dir "; git rev-parse HEAD"))))
    (mv (subseq (car lines) 0 8) state)))

(defconsts (*manual-date* state) (oslib::date))


(defxdoc acl2::top
  :short "ACL2 ASL interpreter manual"
  :long "<p>This manual was built on @(`(:raw *manual-date*)`) from Herdtools7 git
version @(`(:raw *herdtools-git-hash*)`) and ACL2 git version @(`(:raw
*acl2-git-hash*)`). See @(see asl) for a starting point.</p>")

(xdoc::save "./manual" :error t :redef-okp t)
