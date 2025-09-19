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

(in-package "OMAP")

(include-book "std/omaps/core" :dir :system)
(local (include-book "std/lists/sets" :dir :system))






(local (defthm mapp-of-acons
         (implies (and (mapp rest)
                       (case-split
                         (or (emptyp rest)
                             (<< key (mv-nth 0 (head rest))))))
                  (mapp (cons (cons key val) rest)))
         :hints(("Goal" :in-theory (enable mapp emptyp head)))))

(local (defthm head-of-acons
         (implies (and (mapp rest)
                       (case-split
                         (or (emptyp rest)
                             (<< key (mv-nth 0 (head rest))))))
                  (equal (head (cons (cons key val) rest))
                         (mv key val)))
         :hints(("Goal" :in-theory (enable emptyp head)))))

(local (defthm emptyp-of-acons
         (implies (and (mapp rest)
                       (case-split
                         (or (emptyp rest)
                             (<< key (mv-nth 0 (head rest))))))
                  (not (emptyp (cons (cons key val) rest))))
         :hints(("Goal" :in-theory (enable emptyp)))))

(local (defthm tail-of-acons
         (implies (and (mapp rest)
                       (case-split
                         (or (emptyp rest)
                             (<< key (mv-nth 0 (head rest))))))
                  (equal (tail (cons (cons key val) rest))
                         rest))
         :hints(("Goal" :in-theory (enable tail)))))

(local (defthm head-ordered
         (implies (not (emptyp (tail x)))
                  (<< (mv-nth 0 (head x))
                      (mv-nth 0 (head (tail x)))))
         :hints(("Goal" :in-theory (enable head tail emptyp mfix mapp)))))

(defthm emptyp-of-keys
  (equal (set::emptyp (keys x))
         (emptyp x))
  :hints(("Goal" :in-theory (enable keys))))

(defthm keys-under-iff
  (iff (keys x) (not (emptyp x)))
  :hints(("Goal" :in-theory (enable keys))))


#!set
(local
 (encapsulate nil
   (defthm head-of-cons
     (implies (and (setp b)
                   (or (emptyp b)
                       (<< a (head b))))
              (equal (head (cons a b)) a))
     :hints(("Goal" :in-theory (enable setp head emptyp
                                       setp-of-cons))))

   (defthm head-of-insert
     (equal (set::head (set::insert x y))
            (if (or (set::emptyp y)
                    (<< x (set::head y)))
                x
              (set::head y)))
     :hints(("Goal" :in-theory (enable set::insert set::head set::tail
                                       set::emptyp set::sfix set::setp))))))
                                       

(defsection keys-redef
  (local (defthm insert-is-cons-when-ordered
           (implies (or (set::emptyp y)
                        (<< x (set::head y)))
                    (equal (set::insert x y)
                           (cons x (set::sfix y))))
           :hints(("Goal" :in-theory (enable set::insert)))))
  
  (local
   (defthm head-of-omap-keys
     (equal (set::head (keys x))
            (mv-nth 0 (head x)))
     :hints(("Goal" :in-theory (e/d (keys) (head-ordered))
             :induct t)
            '(:use head-ordered))))
  
  (defthmd keys-redef
    (equal (keys x)
           (if (emptyp x)
               nil
             (mv-let (key val)
               (head x)
               (declare (ignore val))
               (cons key (keys (tail x))))))
    :hints(("Goal" ;; :in-theory (enable emptyp
                   ;;                    mfix
                   ;;                    tail
                   ;;                    head)
            :expand ((mapp x)
                     (keys x))
            :do-not-induct t))
    :rule-classes ((:definition :controller-alist ((keys t))))))

;; (local (defthm mv-nth-of-if
;;          (equal (mv-nth n (if x y z))
;;                 (if x (mv-nth n y) (mv-nth n z)))))

(define key-ord-values ((x mapp))
  (if (emptyp x)
      nil
    (mv-let (key val)
      (head x)
      (declare (ignore key))
      (cons val (key-ord-values (tail x)))))
  ///
  (defthm from-lists-of-omap-keys-values
    (equal (from-lists (keys x)
                             (key-ord-values x))
           (mfix x))
    :hints(("Goal" :induct (key-ord-values x)
            :expand ((:with keys-redef (keys x))
                     (key-ord-values x)
                     (:free (a b c d) (from-lists (cons a b) (cons c d)))))))

  (defthm key-ord-values-of-update
    (subsetp-equal (key-ord-values (update key val x))
                   (cons val (key-ord-values x)))
    :hints(("Goal" :in-theory (enable update)
            :expand ((:free (X y) (key-ord-values (cons x y)))
                     (:free (x y) (tail (cons x y)))))))

  (defthm consp-of-key-ord-values
    (equal (consp (key-ord-values x))
           (not (emptyp x))))

  (defthm len-of-key-ord-values
    (equal (len (key-ord-values x))
           (len (keys x)))
    :hints(("Goal" :in-theory (enable keys-redef)))))



(defthm keys-of-from-lists
  (equal (keys (from-lists keys vals))
         (set::mergesort keys))
  :hints(("Goal" :in-theory (enable from-lists
                                    keys
                                    set::mergesort))))


(local (defthmd update-when-key-less
         (implies (or (emptyp x)
                      (<< key (car (keys x))))
                  (equal (update key val x)
                         (cons (cons key val)
                               (mfix x))))
         :hints(("Goal" 
                 :expand ((update key val x)
                          (update key val nil)
                          (keys x))))))

(local
 (defthm head-of-from-lists-when-keys-set
   (implies (set::setp keys)
            (equal (head (from-lists keys vals))
                   (mv (car keys) (and (consp keys) (car vals)))))
   :hints(("Goal" :in-theory (enable (:i from-lists)
                                     update-when-key-less)
           :induct (from-lists keys vals)
           :expand ((from-lists keys vals)
                    (:Free (vals) (from-lists nil vals))
                    (setp keys))))))

(defthm key-ord-values-of-from-lists-when-keys-set
  (implies (set::setp keys)
           (equal (key-ord-values (from-lists keys vals))
                  (take (len keys) vals)))
  :hints(("Goal" :in-theory (enable (:i from-lists)
                                    update-when-key-less)
          :induct (from-lists keys vals)
          :expand ((from-lists keys vals)
                   (:free (vals) (from-lists nil vals))
                   (:free (a b) (key-ord-values (cons a b)))
                   (set::setp keys)))))


(local (defthm assoc-when-key-<<-head-key
         (implies (<< key (head-key x))
                  (not (assoc key x)))
         :hints(("Goal" :in-theory (enable assoc)
                 :induct t)
                (and stable-under-simplificationp
                     '(:cases ((emptyp (tail x))))))))



(define diff-key ((x mapp) (y mapp))
  :returns (key)
  (b* (((when (emptyp x))
        (if (emptyp y)
            nil
          (head-key y)))
       ((when (emptyp y)) (head-key x))
       ((mv xkey xval) (head x))
       ((mv ykey yval) (head y))
       ((unless (equal xkey ykey))
        (if (<< xkey ykey) xkey ykey))
       ((unless (equal xval yval))
        xkey))
    (diff-key (tail x) (tail y)))
  ///
  (defret diff-key-lower-bound
    (b* ((xhead (head-key x))
         (yhead (head-key y)))
      (implies (not (equal (mfix x) (mfix y)))
               (if (emptyp x)
                   (if (emptyp y)
                       (equal key nil)
                     (not (<< key yhead)))
                 (if (emptyp y)
                     (not (<< key xhead))
                   (if (<< xhead yhead)
                       (not (<< key xhead))
                     (not (<< key yhead)))))))
    :hints (("goal" :induct t)
            (and stable-under-simplificationp
                 '(:use ((:instance head-ordered (x x))
                         (:instance head-ordered (x y)))
                   :in-theory (disable head-ordered)))))
  
  (std::defretd diff-key-when-unequal
    (implies (not (equal (mfix x) (mfix y)))
             (not (equal (assoc key x) (assoc key y))))
    :hints(("Goal" :in-theory (enable assoc)
            :induct t)
           (and stable-under-simplificationp
                '(:use ((:instance diff-key-lower-bound
                         (x (tail x)) (y (tail y)))
                        (:instance head-ordered)
                        (:instance head-ordered (x y)))
                  :in-theory (disable head-ordered))))))
        
        

(defthm cons-of-assoc
  (equal (cons k (cdr (assoc k x)))
         (or (assoc k x)
             (cons k nil)))
  :hints(("Goal" :in-theory (enable assoc))))


(defthmd restrict-of-insert-split
  (equal (restrict (set::insert k keys) x)
         (if (assoc k x)
             (update k (lookup k x)
                     (restrict keys x))
           (restrict keys x)))
  :hints (("goal" :use ((:instance diff-key-when-unequal
                         (x (restrict (set::insert k keys) x))
                         (y (if (assoc k x)
                                (update k (lookup k x)
                                        (restrict keys x))
                              (restrict keys x)))))
           :in-theory (enable lookup assoc-of-restrict)
           :do-not-induct t)))


(defthmd assoc-iff-in-keys
  (iff (assoc k x)
       (set::in k (keys x)))
  :hints(("Goal" :in-theory (enable keys assoc))))


(defthm restrict-of-keys
  (equal (restrict (keys x) x)
         (mfix x))
  :hints (("goal" :use ((:instance diff-key-when-unequal
                         (x (restrict (keys x) x))
                         (y (mfix x))))
           :in-theory (enable assoc-of-restrict
                              assoc-iff-in-keys))))


(local (defthm len-of-member
         (<= (len (member-equal k x)) (len x))
         :rule-classes :linear))

(defthmd assoc-of-from-lists
  (equal (assoc k (from-lists keys vals))
         (let ((mem (member-equal k keys)))
           (and mem
                (let ((idx (- (len keys) (len mem))))
                  (cons k (nth idx vals))))))
  :hints(("Goal" :in-theory (enable assoc from-lists))))
             


(define from-lists* ((keys true-listp)
                     (vals true-listp)
                     (base mapp))
  :guard (= (len keys) (len vals))
  :returns (map mapp)
  (cond ((endp keys) (mfix base))
        (t (update (car keys) (car vals)
                   (from-lists* (cdr keys) (cdr vals) base))))
  ///
  (defthmd assoc-of-from-lists*
    (equal (assoc k (from-lists* keys vals base))
           (let ((mem (member-equal k keys)))
             (if mem
                 (let ((idx (- (len keys) (len mem))))
                   (cons k (nth idx vals)))
               (assoc k base)))))

  (defthm from-lists*-in-terms-of-update*
    (equal (from-lists* keys vals base)
           (update* (from-lists keys vals) base))
    :hints (("goal" :use ((:instance diff-key-when-unequal
                           (x (from-lists* keys vals base))
                           (y (update* (from-lists keys vals) base))))
             :in-theory (enable assoc-of-from-lists*
                                assoc-of-from-lists)
             :do-not-induct t))))


(defthm assoc-of-delete
  (equal (assoc k1 (delete k2 x))
         (and (not (equal k1 k2))
              (assoc k1 x)))
  :hints(("Goal" :in-theory (enable assoc delete))))

(defthm delete-of-update-same
  (equal (delete k (update k v x))
         (delete k x))
  :hints (("goal" :use ((:instance diff-key-when-unequal
                         (x (delete k (update k v x)))
                         (y (delete k x)))))))

(defthm delete-of-update-diff
  (implies (not (equal k k2))
           (equal (delete k (update k2 v x))
                  (update k2 v (delete k x))))
  :hints (("goal" :use ((:instance diff-key-when-unequal
                         (x (delete k (update k2 v x)))
                         (y (update k2 v (delete k x))))))))
