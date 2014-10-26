(in-package :iot)

;; A property's "Perfect Clicker $/sec" assumes that the property is always
;; started as soon as its previous run finishes.  The CEOs are always slower
;; than that.  To get the CEO, $/sec, you have to multiply the Perfect Clicker
;; $/sec by a CEO efficiency factor, E.  For a CEO at level N, that's defined
;; as:
;;
;;   E = N / (1 + N)
;;
(defun ceo-efficiency (level)
  (/ level (1+ level)))

;; The first CEO level costs ten times what the first property level cost.
(defun prop-initial-ceo-cost (property-or-key)
  (* 10 (prop-initial-cost property-or-key)))

;; CEO costs rise really fast.  C_{N}, the cost to improve a CEO at level N, is:
;;
;;   C_{N} = C_0 * 2^{N^{2}}
;;
(defun prop-ceo-cost-at (property-or-key level &optional (count 1))
  (if (= 1 count)
      (* (prop-initial-ceo-cost property-or-key)
         (expt 2 (expt level 2)))
      (+ (prop-ceo-cost-at property-or-key level 1)
         (prop-ceo-cost-at property-or-key (1+ level) (1- count)))))
