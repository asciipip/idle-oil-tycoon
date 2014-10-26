(in-package :iot)

(defstruct (state
             (:print-object print-state))
  (props (make-array (length *property-keys*) :initial-element 0) :read-only t)
  (ceos  (make-array (length *property-keys*) :initial-element 0) :read-only t)
  (techs nil :read-only t)
  (experience 0 :read-only t))

(defun print-state (state stream)
  (format stream "#S(STATE~%   :PROPS ~A~%   :CEOS ~A~%   :TECHS #<last: ~A>~%   :EXPERIENCE ~A)"
          (state-props state)
          (state-ceos state)
          (and (car (state-techs state)) (tech-name (car (last (state-techs state)))))
          (f (state-experience state))))

(defun print-state-short (state stream)
  (iterate (for-property prop)
           (format stream "~A/~A " (state-prop-level state prop)
                   (state-ceo-level state prop)))
  (format stream " ~A/~A~%" (length (state-techs state))
          (and (car (state-techs state)) (tech-name (car (last (state-techs state)))))))

(defun fresh-state (experience)
  (make-state :props #(10 0 0 0 0 0 0 0)
              :ceos #(1 0 0 0 0 0 0 0)
              :experience experience))

(defun describe-state (state stream)
  (let ((name-len (iterate (for-property prop)
                           (maximizing (length (prop-name prop)))))
        (min-level (iterate (for level in-vector (state-props state))
                            (minimizing level))))
    (iterate (for-property prop)
             (for i from 0)
             (for profit = (prop-profit-at
                              prop
                              (state-prop-level state prop)
                              (tech-mult (state-techs state) prop)
                              min-level
                              (state-experience state)))
             (for pps = (prop-pps-at
                           prop
                           (state-prop-level state prop)
                           (tech-mult (state-techs state) prop)
                           min-level
                           (state-experience state)))
             (sum pps into all-pps)
             (format stream "~vA   level: ~4@A   profit: ~11@A   $/sec: ~11@A~%"
                     name-len (prop-name prop)
                     (state-prop-level state prop)
                     (f profit)
                     (f (* pps (ceo-efficiency (state-ceo-level state prop)))))
             (finally (format stream "~vA                                       $/sec: ~11@A~%" name-len "Total" (f all-pps))))))

(defun state-pps (state)
  (let ((min-level (iterate (for level in-vector (state-props state))
                            (minimizing level))))
    (iterate (for-property prop)
             (for i from 0)
             (summing (* (ceo-efficiency (state-ceo-level state prop))
                         (prop-pps-at prop
                                      (state-prop-level state prop)
                                      (tech-mult (state-techs state) prop)
                                      min-level
                                      (state-experience state)))))))

(defun state-prop-level (state property-or-key)
  (elt (state-props state) (position (prop-key property-or-key) *property-keys*)))

(defun state-min-level (state)
  (iterate (for l in-vector (state-props state))
           (for-property p)
           (minimizing l into ml)
           (finding p minimizing l into mp)
           (finally (return (values ml mp)))))

(defun state-ceo-level (state property-or-key)
  (elt (state-ceos state) (position (prop-key property-or-key) *property-keys*)))

(defun state-add-prop (state property-or-key inc)
  (let ((new-props (copy-array (state-props state)))
        (i (position (prop-key property-or-key) *property-keys*)))
    (setf (elt new-props i) (+ inc (elt new-props i)))
    (make-state :props new-props :ceos (state-ceos state)
                :techs (state-techs state) :experience (state-experience state))))

(defun state-add-ceo (state property-or-key &optional (inc 1))
  (let ((new-ceos (copy-array (state-ceos state)))
        (i (position (prop-key property-or-key) *property-keys*)))
    (setf (elt new-ceos i) (+ inc (elt new-ceos i)))
    (make-state :props (state-props state) :ceos new-ceos
                :techs (state-techs state) :experience (state-experience state))))

(defun state-add-tech (state tech)
  (let ((new-techs (sort (cons tech (copy-list (state-techs state)))
                         (lambda (t1 t2) (< (tech-cost t1) (tech-cost t2))))))
    (make-state :props (state-props state) :ceos (state-ceos state)
                :techs new-techs :experience (state-experience state))))

(defun prior-state-p (reference test)
  (and (iterate (for rp in-vector (state-props reference))
                (for tp in-vector (state-props test))
                (always (<= tp rp))
                (for rc in-vector (state-ceos reference))
                (for tc in-vector (state-ceos reference))
                (always (<= tc rc)))
       (endp (set-difference (state-techs test) (state-techs reference)))))
