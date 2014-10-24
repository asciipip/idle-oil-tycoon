(in-package :iot)

(defun ceo-efficiency (level)
  (/ level (1+ level)))

(defun investment-initial-ceo-cost (investment)
  (* 10 (investment-initial-cost investment)))

(defun investment-ceo-cost-at (investment level &optional (count 1))
  (if (= 1 count)
      (* (investment-initial-ceo-cost investment)
         (expt 2 (expt level 2)))
      (+ (investment-ceo-cost-at investment level 1)
         (investment-ceo-cost-at investment (1+ level) (1- count)))))
