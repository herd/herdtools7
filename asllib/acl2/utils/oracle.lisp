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

(in-package "ACL2")

(include-book "xoshiro")
(include-book "centaur/fty/deftypes" :dir :system)
(include-book "centaur/fty/basetypes" :dir :system)
(local (include-book "centaur/bitops/ihsext-basics" :dir :System))
(local (in-theory (disable unsigned-byte-p)))
(local (std::add-default-post-define-hook :fix))

(encapsulate
  (((oracle-read * *) => (mv * *)
    :formals (limit st)
    :guard (posp limit))
   ((oracle-read-int *) => (mv * *)
    :formals (st)
    :guard t)
   ((oracle-read-rational *) => (mv * *)
    :formals (st)
    :guard t)
   ((oracle-read-string *) => (mv * *)
    :formals (st)
    :guard t))

  (local (defun oracle-read (limit st)
           (declare (xargs :guard (posp limit))
                    (ignore limit))
           (mv 0 st)))

  (local (defun oracle-read-int (st)
           (declare (xargs :guard t))
           (mv 0 st)))

  (local (defun oracle-read-rational (st)
           (declare (xargs :guard t))
           (mv 0 st)))

  (local (defun oracle-read-string (st)
           (declare (xargs :guard t))
           (mv "" st)))

  (defthm oracle-read-type
    (natp (mv-nth 0 (oracle-read limit st)))
    :rule-classes :type-prescription)

  (defthm oracle-read-bound
    (implies (posp limit)
             (< (mv-nth 0 (oracle-read limit st)) limit))
    :rule-classes :linear)

  (defthm oracle-read-int-type
    (integerp (mv-nth 0 (oracle-read-int st)))
    :rule-classes :type-prescription)

  (defthm oracle-read-rational-type
    (rationalp (mv-nth 0 (oracle-read-rational st)))
    :rule-classes :type-prescription)

  (defthm oracle-read-string-type
    (stringp (mv-nth 0 (oracle-read-string st)))
    :rule-classes :type-prescription))




(define char-range-p (x)
  (and (consp x)
       (natp (car x))
       (integerp (cdr x))
       (<= (car x) (cdr x))
       (<= (cdr x) 256)))

(define char-range-fix ((x char-range-p))
  :returns (new-x char-range-p)
  (mbe :logic (if (char-range-p x)
                  x
                '(0 . 0))
       :exec x)
  ///
  (defret char-range-fix-when-char-range-p
    (implies (char-range-p x)
             (equal (char-range-fix x) x)))

  (fty::deffixtype char-range
    :pred char-range-p
    :fix char-range-fix
    :equiv char-range-equiv
    :define t))

(define char-range-low ((x char-range-p))
  :returns (low natp :rule-classes :type-prescription)
  :prepwork ((local (in-theory (enable char-range-p char-range-fix))))
  (car (char-range-fix x)))

(define char-range-high ((x char-range-p))
  :returns (low natp :rule-classes :type-prescription)
  :prepwork ((local (in-theory (enable char-range-p char-range-fix))))
  (cdr (char-range-fix x))
  ///
  (defret char-range-low-high-rel
    (<= (char-range-low x) (char-range-high x))
    :hints(("Goal" :in-theory (enable char-range-low)))
    :rule-classes :linear)

  (defret char-range-high-bound
    (<= (char-range-high x) 256)
    :rule-classes :linear))
  
(fty::deflist char-ranges :elt-type char-range :true-listp t)

(define char-range-width ((x char-range-p))
  :returns (width natp :rule-classes :type-prescription)
  (- (char-range-high x)
     (char-range-low x))
  ///
  (defret <fn>-plus-low-upper-bound
    (<= (+ width (char-range-low x)) 256)
    :rule-classes :linear))

(define char-ranges-width ((x char-ranges-p))
  :returns (width natp :rule-classes :type-prescription)
  (if (atom x)
      0
    (+ (char-range-width (car x))
       (char-ranges-width (cdr x)))))

(fty::defprod char-spec
  ((width posp
          :reqfix (pos-fix (char-ranges-width ranges))
          :rule-classes :type-prescription)
   (ranges char-ranges-p :reqfix (if (equal (char-ranges-width ranges) 0)
                                     '((0 . 1))
                                   ranges)))
  :layout :fulltree
  :require (equal width (char-ranges-width ranges))
  ///
  (defthm char-ranges-width-of-char-spec->ranges
    (posp (char-ranges-width (char-spec->ranges x)))
    :hints(("Goal" :use posp-of-char-spec->width
            :in-theory (disable posp-of-char-spec->width)))
    :rule-classes :type-prescription))

(defstobj orac
  (oracle-mode :type (integer 0 2) :initially 0)
  (oracle-st :type t :initially nil)
  (oracle-xosh :type xoshiro)
  (oracle-xosh-int-low :type integer :initially #x-80000000)
  (oracle-xosh-int-width :type (integer 1 *) :initially #xffffffff)
  (oracle-xosh-string-len :type (integer 0 *) :initially 10)
  (oracle-xosh-char-spec :type (satisfies char-spec-p) :initially (96 (32 . 128))))



(define orac-read ((limit posp) orac)
  :returns (mv (val natp :rule-classes :type-prescription)
               (new-orac))
  (b* ((limit (mbe :logic (pos-fix limit) :exec limit)))
    (case (oracle-mode orac)
      (0 (stobj-let ((xoshiro (oracle-xosh orac)))
                    (val xoshiro)
                    (xoshiro-gen-range limit xoshiro)
                    (mv val orac)))
      (1 (mv 0 orac))
      (t (b* ((st (oracle-st orac))
              ((mv val new-st) (oracle-read limit st))
              (orac (update-oracle-st new-st orac)))
           (mv val orac)))))
  ///
  (defret orac-read-bound
    (implies (posp limit)
             (< val limit))
    :rule-classes :linear))

(define orac-read-u32 (orac)
    :returns (mv (val natp :rule-classes :type-prescription)
               (new-orac))
  (case (oracle-mode orac)
    (0 (stobj-let ((xoshiro (oracle-xosh orac)))
                  (val xoshiro)
                  (xoshiro-next xoshiro)
                  (mv val orac)))
    (1 (mv 0 orac))
    (t (b* ((st (oracle-st orac))
            ((mv val new-st) (oracle-read (ash 1 32) st))
            (orac (update-oracle-st new-st orac)))
         (mv val orac))))
  ///
  (defret orac-read-u32-type
    (unsigned-byte-p 32 val)
    :hints ((and stable-under-simplificationp
                 '(:in-theory (enable unsigned-byte-p))))))


(define orac-read-bits ((nbits natp) orac)
  :returns (mv (val natp :rule-classes :type-prescription)
               (new-orac))
  (case (oracle-mode orac)
    (0 (stobj-let ((xoshiro (oracle-xosh orac)))
                  (val xoshiro)
                  (xoshiro-gen-bits (lnfix nbits) xoshiro)
                  (mv val orac)))
    (1 (mv 0 orac))
    (t (b* ((st (oracle-st orac))
            ((mv val new-st) (oracle-read (ash 1 (lnfix nbits)) st))
            (orac (update-oracle-st new-st orac)))
         (mv val orac))))
  ///
  (local (defthm posp-of-expt
           (implies (natp n)
                    (posp (expt 2 n)))
           :hints(("Goal" :in-theory (enable expt)))))
  
  (defret orac-read-bits-type
    (implies (natp nbits)
             (unsigned-byte-p nbits val))
    :hints ((and stable-under-simplificationp
                 '(:in-theory (enable unsigned-byte-p
                                      bitops::ash-is-expt-*-x))))))




(define orac-read-int (orac)
  :returns (mv (val integerp :rule-classes :type-prescription)
               (new-orac))
  (case (oracle-mode orac)
    (0
     (b* ((low (lifix (oracle-xosh-int-low orac)))
          (width (oracle-xosh-int-width orac))
          (width (mbe :logic (pos-fix width) :exec width)))
       (stobj-let ((xoshiro (oracle-xosh orac)))
                  (val xoshiro)
                  (xoshiro-gen-range width xoshiro)
                  (mv (+ val low) orac))))
    (1 (mv 0 orac))
    (t (b* ((st (oracle-st orac))
            ((mv val new-st) (oracle-read-int st))
            (orac (update-oracle-st new-st orac)))
         (mv val orac)))))

(define orac-read-rational (orac)
  :returns (mv (val rationalp :rule-classes :type-prescription)
               (new-orac))
  (case (oracle-mode orac)
    (0
     (b* ((low (lifix (oracle-xosh-int-low orac)))
          (width (oracle-xosh-int-width orac))
          (width (mbe :logic (pos-fix width) :exec width)))
       (stobj-let ((xoshiro (oracle-xosh orac)))
                  (val xoshiro)
                  (b* (((mv num1 xoshiro) (xoshiro-gen-range width xoshiro))
                       (num (+ low num1))
                       ((mv den xoshiro) (xoshiro-gen-range width xoshiro)))
                    (mv (/ num (+ 1 den)) xoshiro))
                  (mv val orac))))
    (1 (mv 0 orac))
    (t (b* ((st (oracle-st orac))
            ((mv val new-st) (oracle-read-rational st))
            (orac (update-oracle-st new-st orac)))
         (mv val orac)))))

(define xoshiro-gen-char-aux ((val natp)
                              (ranges char-ranges-p))
  :guard (< val (char-ranges-width ranges))
  :guard-hints (("goal" :in-theory (enable char-ranges-width)))
  :measure (len ranges)
  :returns (char characterp :rule-classes :type-prescription)
  (b* ((range1 (car ranges))
       (width1 (char-range-width range1))
       (val (lnfix val))
       ((when (or (< val width1)
                  (not (mbt (consp ranges)))))
        (code-char (+ val (char-range-low range1)))))
    (xoshiro-gen-char-aux (- val width1) (cdr ranges))))
  
(define xoshiro-gen-char ((cs char-spec-p) xoshiro)
  :returns (mv (char characterp :rule-classes :type-prescription)
               new-xoshiro)
  :guard-debug t
  (b* (((char-spec cs))
       ((mv val xoshiro) (xoshiro-gen-range cs.width xoshiro)))
    (mv (xoshiro-gen-char-aux val cs.ranges) xoshiro)))

(define xoshiro-gen-string-aux ((len natp) (cs char-spec-p) xoshiro)
  :returns (mv (chars character-listp)
               new-xoshiro)
  (b* (((when (zp len)) (mv nil xoshiro))
       ((mv char1 xoshiro) (xoshiro-gen-char cs xoshiro))
       ((mv rest xoshiro) (xoshiro-gen-string-aux (1- len) cs xoshiro)))
    (mv (cons char1 rest) xoshiro)))

(define xoshiro-gen-string ((len natp) (cs char-spec-p) xoshiro)
  :returns (mv (str stringp :rule-classes :type-prescription)
               new-xoshiro)
  (b* (((mv chars xoshiro) (xoshiro-gen-string-aux len cs xoshiro)))
    (mv (coerce chars 'string) xoshiro)))
    
               

(define orac-read-string (orac)
  :returns (mv (val stringp :rule-classes :type-prescription)
               (new-orac))
  (case (oracle-mode orac)
    (0
     (b* ((len (oracle-xosh-string-len orac))
          (cs (oracle-xosh-char-spec orac)))
       (stobj-let ((xoshiro (oracle-xosh orac)))
                  (val xoshiro)
                  (xoshiro-gen-string len cs xoshiro)
                  (mv val orac))))
    (1 (mv "" orac))
    (t (b* ((st (oracle-st orac))
            ((mv val new-st) (oracle-read-string st))
            (orac (update-oracle-st new-st orac)))
         (mv val orac)))))


(define orac-st (lst orac)
  :returns (new-orac)
  (b* (((when (atom lst)) orac)
       (obj (car lst))
       (orac (orac-st (cdr lst) orac))
       ((mv & orac)
        (case-match obj
          ((':bits n) (orac-read-bits (nfix n) orac))
          (':u32 (orac-read-u32 orac))
          (':int (orac-read-int orac))
          (':rational (orac-read-rational orac))
          (':string   (orac-read-string orac))
          (& (orac-read (pos-fix obj) orac)))))
    orac)
  ///
  (defthm orac-st-consolidate
    (equal (orac-st lst1 (orac-st lst2 orac))
           (orac-st (append lst1 lst2) orac)))

  (defthm orac-read-to-orac-st
    (equal (mv-nth 1 (orac-read n orac))
           (orac-st (list (pos-fix n)) orac)))

  (defthm orac-read-bits-to-orac-st
    (equal (mv-nth 1 (orac-read-bits n orac))
           (orac-st `((:bits ,(nfix n))) orac)))

  (defthm orac-read-u32-to-orac-st
    (equal (mv-nth 1 (orac-read-u32 orac))
           (orac-st '(:u32) orac)))

  (defthm orac-read-int-to-orac-st
    (equal (mv-nth 1 (orac-read-int orac))
           (orac-st '(:int) orac)))

  (defthm orac-read-rational-to-orac-st
    (equal (mv-nth 1 (orac-read-rational orac))
           (orac-st '(:rational) orac)))

  (defthm orac-read-string-to-orac-st
    (equal (mv-nth 1 (orac-read-string orac))
           (orac-st '(:string) orac))))

(in-theory (disable acl2::oracp))
