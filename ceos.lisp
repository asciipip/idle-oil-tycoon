(in-package :iot)

(defun ceo-efficiency (level)
  (/ level (1+ level)))

(defun prop-initial-ceo-cost (property-or-key)
  (* 10 (prop-initial-cost property-or-key)))

(defun prop-ceo-cost-at (property-or-key level &optional (count 1))
  (if (= 1 count)
      (* (prop-initial-ceo-cost property-or-key)
         (expt 2 (expt level 2)))
      (+ (prop-ceo-cost-at property-or-key level 1)
         (prop-ceo-cost-at property-or-key (1+ level) (1- count)))))
