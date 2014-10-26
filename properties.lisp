(in-package :iot)

(defstruct (property (:conc-name prop-)
                     (:constructor make-prop))
  name
  initial-profit
  initial-time
  initial-cost
  cost-increase)

(defvar *property-keys*
  '(:gas-royalties
    :oil-royalties
    :gas-well
    :oil-well
    :oil-sands
    :shale-play
    :omani-field
    :saudi-field)
  "Keys to the *properties* hash, in preferred order.")

(defvar *properties*
  (let ((props (make-hash-table)))
    (prog1
        props
      (setf (gethash :gas-royalties props)
            (make-prop
             :name "Gas Royalties"
             :initial-profit 2
             :initial-time 2
             :initial-cost 5
             :cost-increase 8/100))
      (setf (gethash :oil-royalties props)
            (make-prop
             :name "Oil Royalties"
             :initial-profit 15
             :initial-time 4
             :initial-cost 70
             :cost-increase 8/100))
      (setf (gethash :gas-well props)
            (make-prop
             :name "Gas Well"
             :initial-profit 130
             :initial-time 10
             :initial-cost 450
             :cost-increase 11/100))
      (setf (gethash :oil-well props)
            (make-prop
             :name "Oil Well"
             :initial-profit 1100
             :initial-time 20
             :initial-cost 21000
             :cost-increase 9/100))
      (setf (gethash :oil-sands props)
            (make-prop
             :name "Oil Sands"
             :initial-profit 10000
             :initial-time 50
             :initial-cost 160000
             :cost-increase 10/100))
      (setf (gethash :shale-play props)
            (make-prop
             :name "Shale Play"
             :initial-profit 90000
             :initial-time 150
             :initial-cost 2200000
             :cost-increase 8/100))
      (setf (gethash :omani-field props)
            (make-prop
             :name "Omani Field"
             :initial-profit 850000
             :initial-time 600
             :initial-cost 19400000
             :cost-increase 8/100))
      (setf (gethash :saudi-field props)
            (make-prop
             :name "Saudi Field"
             :initial-profit 7000000
             :initial-time 2400
             :initial-cost 620000000
             :cost-increase 7/100)))))

;; This just simplifies iteration over all of the properties.
(defmacro-clause (FOR-PROPERTY prop &optional WITH-KEY key)
  (let ((pkey (or key (gensym))))
    `(progn
       (for ,pkey in *property-keys*)
       (for ,prop = (gethash ,pkey *properties*)))))

(defun property (property-or-key)
  (if (symbolp property-or-key)
      (gethash property-or-key *properties*)
      property-or-key))

(defun prop-key (property-or-key)
  (if (symbolp property-or-key)
      property-or-key
      (iterate (for-property prop with-key key)
               (finding key such-that (equal property-or-key prop)))))

(defun full-prop-name (property-or-key)
  "Works like PROP-NAME, but a) takes keys, too; and b) understands :all."
  (if (eql property-or-key :all)
      "All Properties"
      (prop-name (property property-or-key))))

;; Each property has a fixed percentage by which the purchase price increases.
;; If the percentage is P, then C_{n}, the cost at level N, is given by:
;;
;;   C_{n} = C_{n-1} * (1 + P)
;;
;; This can be calculated non-recursively by:
;;
;;   C_{n} = C_0 * (1 + P)^{N}
;;
(defun prop-cost-at (property-or-key level &optional (count 1))
  (let ((prop (property property-or-key)))
    (* (prop-initial-cost prop)
       (expt (1+ (prop-cost-increase prop)) level)
       (/ (1- (expt (1+ (prop-cost-increase prop)) count))
          (prop-cost-increase prop)))))

;; Property profits are a little more involved.
;; Basically, the profit at level N is equal to the profit at level 1 times N.
;; You also have to factor in all the multipliers from technologies,
;; achievements, and experience.
(defun prop-profit-at (property-or-key level tech-mult all-min-level experience)
  (let ((prop (property property-or-key)))
    (* (prop-initial-profit prop)
       level
       tech-mult
       (achievement-mult prop level all-min-level)
       (experience-mult experience))))

;; Propfit per second just has to take the property speed and any speed
;; multipliers from achievements into account.
(defun prop-pps-at (property-or-key level tech-mult all-min-level experience)
  (let ((prop (property property-or-key)))
    (* (/ (prop-profit-at prop level tech-mult all-min-level experience)
          (prop-initial-time prop))
       (achievement-mult prop level all-min-level :achievement-key #'achievement-speed-mult))))
