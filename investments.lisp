(in-package :iot)

(defstruct investment
  name
  initial-profit
  initial-time
  initial-cost
  cost-increase
  achievements)

(defvar *investment-keys*
  '(:gas-royalties
    :oil-royalties
    :gas-well
    :oil-well
    :oil-sands
    :shale-play
    :omani-field
    :saudi-field))

(defvar *investments*
  (let ((invs (make-hash-table)))
    (prog1
        invs
      (setf (gethash :gas-royalties invs)
            (make-investment
             :name "Gas Royalties"
             :initial-profit 2
             :initial-time 2
             :initial-cost 5
             :cost-increase 8/100))
      (setf (gethash :oil-royalties invs)
            (make-investment
             :name "Oil Royalties"
             :initial-profit 15
             :initial-time 4
             :initial-cost 70
             :cost-increase 8/100))
      (setf (gethash :gas-well invs)
            (make-investment
             :name "Gas Well"
             :initial-profit 130
             :initial-time 10
             :initial-cost 450
             :cost-increase 11/100))
      (setf (gethash :oil-well invs)
            (make-investment
             :name "Oil Well"
             :initial-profit 1100
             :initial-time 20
             :initial-cost 21000
             :cost-increase 9/100))
      (setf (gethash :oil-sands invs)
            (make-investment
             :name "Oil Sands"
             :initial-profit 10000
             :initial-time 50
             :initial-cost 160000
             :cost-increase 10/100))
      (setf (gethash :shale-play invs)
            (make-investment
             :name "Shale Play"
             :initial-profit 90000
             :initial-time 150
             :initial-cost 2200000
             :cost-increase 8/100))
      (setf (gethash :omani-field invs)
            (make-investment
             :name "Omani Field"
             :initial-profit 850000
             :initial-time 600
             :initial-cost 19400000
             :cost-increase 8/100))
      (setf (gethash :saudi-field invs)
            (make-investment
             :name "Saudi Field"
             :initial-profit 7000000
             :initial-time 2400
             :initial-cost 620000000
             :cost-increase 7/100)))))

(defun investment (ikey)
  (gethash ikey *investments*))

(defun full-investment-name (key)
  (if (eql key :all)
      "All Properties"
      (investment-name (gethash key *investments*))))

(defun investment-cost-at (investment level &optional (count 1))
  (* (investment-initial-cost investment)
     (expt (1+ (investment-cost-increase investment)) level)
     (/ (1- (expt (1+ (investment-cost-increase investment)) count))
        (investment-cost-increase investment))))

(defun investment-profit-at (investment level tech-mult all-min-level experience)
  (let ((ikey (iterate (for (key value) in-hashtable *investments*)
                       (finding key such-that (eql (investment-name value)
                                                   (investment-name investment))))))
    (* (investment-initial-profit investment)
       level
       tech-mult
       (achievement-mult ikey level all-min-level)
       (experience-mult experience))))

(defun investment-pps-at (investment level tech-mult all-min-level experience)
  (let ((ikey (iterate (for (key value) in-hashtable *investments*)
                       (finding key such-that (eql (investment-name value)
                                                   (investment-name investment))))))
    (* (/ (investment-profit-at investment level tech-mult all-min-level experience)
          (investment-initial-time investment))
       (achievement-mult ikey level all-min-level :achievement-key #'achievement-speed-mult))))

(defun investment-purchase-for (investment amount)
  "How many investments of the given type can you purchase for AMOUNT dollars?"
  (or (iterate (for l from 0)
               (summing (investment-cost-at investment l) into total)
               (until (< amount total))
               (maximizing (1+ l)))
      0))

(defun cost-per-pps (ikey &optional max)
  (let ((inv (gethash ikey *investments*)))
    (append (cons (list (investment-cost-at inv 0 1) (investment-pps-at inv 1 1 0 0))
                  (iterate (for a in *achievements*)
                           (when (eql (achievement-investment a) ikey)
                             (collecting (list (investment-cost-at inv 0 (1- (achievement-level a)))
                                               (investment-pps-at inv (1- (achievement-level a)) 1 0 0)))
                             (collecting (list (investment-cost-at inv 0 (achievement-level a))
                                               (investment-pps-at inv (achievement-level a) 1 0 0))))))
            (when max
              (iterate (for l from 0)
                       (for cost = (investment-cost-at inv l))
                       (summing cost into total-cost)
                       (while (<= cost max))
                       (finally (return (list (list (investment-cost-at inv 0 l)
                                                    (investment-pps-at inv l 1 0 0))))))))))
