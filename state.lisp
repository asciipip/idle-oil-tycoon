(in-package :iot)

(defstruct (state
             (:print-object print-state))
  (levels (make-array (length *investment-keys*) :initial-element 0) :read-only t)
  (ceos   (make-array (length *investment-keys*) :initial-element 0) :read-only t)
  (techs  nil :read-only t)
  (experience 0 :read-only t))

(defun print-state (state stream)
  (format stream "#S(STATE~%   :LEVELS ~A~%   :CEOS ~A~%   :TECHS #<last: ~A>~%   :EXPERIENCE ~A)"
          (state-levels state)
          (state-ceos state)
          (and (car (state-techs state)) (tech-name (car (last (state-techs state)))))
          (f (state-experience state))))

(defun print-state-short (state stream)
  (iterate (for i in *investment-keys*)
           (format stream "~A/~A " (state-investment-level state i)
                   (state-ceo-level state i)))
  (format stream " ~A/~A~%" (length (state-techs state))
          (and (car (state-techs state)) (tech-name (car (last (state-techs state)))))))

(defun fresh-state (experience)
  (make-state :levels #(10 0 0 0 0 0 0 0)
              :ceos #(1 0 0 0 0 0 0 0)
              :experience experience))

(defun describe-state (state stream)
  (let ((name-len (iterate (for (ikey investment) in-hashtable *investments*)
                           (maximizing (length (investment-name investment)))))
        (min-level (iterate (for level in-vector (state-levels state))
                            (minimizing level))))
    (iterate (for ikey in *investment-keys*)
             (for investment = (gethash ikey *investments*))
             (for i from 0)
             (for profit = (investment-profit-at
                              investment
                              (elt (state-levels state) i)
                              (tech-mult (state-techs state) ikey)
                              min-level
                              (state-experience state)))
             (for pps = (investment-pps-at
                              investment
                              (elt (state-levels state) i)
                              (tech-mult (state-techs state) ikey)
                              min-level
                              (state-experience state)))
             (sum pps into all-pps)
             (format stream "~vA   level: ~4@A   profit: ~11@A   $/sec: ~11@A~%"
                     name-len (investment-name investment)
                     (elt (state-levels state) i)
                     (f profit)
                     (f (* pps (ceo-efficiency (elt (state-ceos state) i)))))
             (finally (format stream "~vA                                       $/sec: ~11@A~%" name-len "Total" (f all-pps))))))

(defun state-pps (state)
  (let ((min-level (iterate (for level in-vector (state-levels state))
                            (minimizing level))))
    (iterate (for (ikey investment) in-hashtable *investments*)
             (for i from 0)
             (summing (* (ceo-efficiency (elt (state-ceos state) i))
                         (investment-pps-at investment
                                            (elt (state-levels state) i)
                                            (tech-mult (state-techs state) ikey)
                                            min-level
                                            (state-experience state)))))))

(defun state-investment-level (state ikey)
  (elt (state-levels state) (position ikey *investment-keys*)))

(defun state-min-level (state)
  (iterate (for l in-vector (state-levels state))
           (for i in *investment-keys*)
           (minimizing l into ml)
           (finding i minimizing l into mi)
           (finally (return (values ml mi)))))

(defun state-ceo-level (state ikey)
  (elt (state-ceos state) (position ikey *investment-keys*)))

(defun state-add-investment (state ikey inc)
  (let ((new-levels (copy-array (state-levels state)))
        (i (position ikey *investment-keys*)))
    (setf (elt new-levels i) (+ inc (elt new-levels i)))
    (make-state :levels new-levels :ceos (state-ceos state)
                :techs (state-techs state) :experience (state-experience state))))

(defun state-add-ceo (state ikey &optional (inc 1))
  (let ((new-ceos (copy-array (state-ceos state)))
        (i (position ikey *investment-keys*)))
    (setf (elt new-ceos i) (+ inc (elt new-ceos i)))
    (make-state :levels (state-levels state) :ceos new-ceos
                :techs (state-techs state) :experience (state-experience state))))

(defun state-add-tech (state tech)
  (let ((new-techs (sort (cons tech (copy-list (state-techs state)))
                         (lambda (t1 t2) (< (tech-cost t1) (tech-cost t2))))))
    (make-state :levels (state-levels state) :ceos (state-ceos state)
                :techs new-techs :experience (state-experience state))))

(defun prior-state-p (reference test)
  (and (iterate (for rl in-vector (state-levels reference))
                (for tl in-vector (state-levels test))
                (always (<= tl rl))
                (for rc in-vector (state-ceos reference))
                (for tc in-vector (state-ceos reference))
                (always (<= tc rc)))
       (endp (set-difference (state-techs test) (state-techs reference)))))
