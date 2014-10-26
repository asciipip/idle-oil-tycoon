(in-package :iot)

(defun prop-purchase-for (property-or-key amount)
  "How many properties of the given type can you purchase for AMOUNT dollars?"
  (or (iterate (for l from 0)
               (summing (prop-cost-at property-or-key l) into total)
               (until (< amount total))
               (maximizing (1+ l)))
      0))

(defun costs-per-pps (property-or-key &optional max)
  "Generates a list of (COST PPS) pairs suitable for graphing the property's
   cost-per-pps line."
  (let ((prop (property property-or-key)))
    (append (cons (list (prop-cost-at prop 0 1) (prop-pps-at prop 1 1 0 0))
                  (iterate (for a in *achievements*)
                           (when (equal (achievement-property a) prop)
                             (collecting (list (prop-cost-at prop 0 (1- (achievement-level a)))
                                               (prop-pps-at prop (1- (achievement-level a)) 1 0 0)))
                             (collecting (list (prop-cost-at prop 0 (achievement-level a))
                                               (prop-pps-at prop (achievement-level a) 1 0 0))))))
            (when max
              (iterate (for l from 0)
                       (for cost = (prop-cost-at prop l))
                       (summing cost into total-cost)
                       (while (<= cost max))
                       (finally (return (list (list (prop-cost-at prop 0 l)
                                                    (prop-pps-at prop l 1 0 0))))))))))
