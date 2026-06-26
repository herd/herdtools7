;;****************************************************************************;;
;;                              ACL2 ASL Library                              ;;
;;****************************************************************************;;
;;
;; SPDX-FileCopyrightText: Copyright 2026 Arm Limited and/or its affiliates <open-source-office@arm.com>
;; SPDX-License-Identifier: BSD-3-Clause
;; 
;; ****************************************************************************;;



(in-package "ASL")
(include-book "centaur/fgl/top" :dir :System)
(include-book "enums")
(include-book "int-constraints")
(include-book "interp-redefs")
(include-book "arrays")
(include-book "imaps")
(include-book "oracle")
(include-book "traces")
(include-book "type-resolution")
(include-book "type-rules")
(include-book "bad-terms")
(include-book "ctrexes")
(include-book "fgl-debugging")
(include-book "storage-non-vars")
(include-book "traces")
(include-book "profile")

(local (in-theory (disable w (tau-system))))
(with-output :off (event)
  (fgl::def-fancy-ev-primitives asl-fancy-ev))

(make-event `(fgl::remove-fgl-rewrites
              eval_expr-*t-when-error-free-record-redef
              eval_lexpr-*t-when-error-free-setfield-redef
              eval_stmt-*t1-when-error-free-avoid-merging-s_cond-branches
              eval_expr-*t-when-error-free-arbitrary-redef
              eval_for-*t-count-when-error-free
              EVAL_EXPR-*T-WHEN-ERROR-FREE-OF-E_GETARRAY-IN-TERMS-OF-V_ARRAY-NTH
              EVAL_EXPR-*T-WHEN-ERROR-FREE-AVOID-MERGING-E_COND-BRANCHES
              EVAL_LEXPR-*T-WHEN-ERROR-FREE-OF-LE_SETARRAY-IN-TERMS-OF-V_ARRAY-UPDATE-NTH
              .
              ,*error-free-thmnames*))

(fgl-reorder-eval_subprogram-*t-rules)
