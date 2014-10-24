(in-package :iot)

(defclass action () ())

(defun print-actions (actions &optional initial-state)
  (if (car actions)
      (if initial-state
          (progn
            (format t "~A~%" (action-desc (car actions) initial-state))
            (print-actions (cdr actions) (action-apply (car actions) initial-state)))
          (progn
            (format t "~A~%" (action-desc (car actions)))
            (print-actions (cdr actions))))
      (progn
        (format t "====================~%")
        (print-state initial-state t))))

(defclass action-investment (action)
  ((investment :accessor action-investment :initarg :investment)
   (qty :accessor action-qty :initarg :qty)))

(defmethod action-cost ((action action-investment) state)
  (let* ((ikey (action-investment action))
         (investment (gethash ikey *investments*)))
    (investment-cost-at investment (state-investment-level state ikey) (action-qty action))))

(defclass action-ceo (action)
  ((investment :accessor action-investment :initarg :investment)
   (qty :accessor action-qty :initarg :qty :initform 1)))

(defmethod action-cost ((action action-ceo) state)
  (let* ((ikey (action-investment action))
         (investment (gethash ikey *investments*)))
    (investment-ceo-cost-at investment (state-ceo-level state ikey) (action-qty action))))

(defclass action-tech (action)
  ((technology :accessor action-tech :initarg :tech)))

(defmethod action-cost ((action action-tech) state)
  (tech-cost (action-tech action)))

(defclass action-bootstrap (action)
  ((investment :accessor action-investment :initarg :investment)))

(defmethod action-cost ((action action-bootstrap) state)
  (let ((investment (gethash (action-investment action) *investments*)))
    (+ (investment-cost-at investment 0 10)
       (investment-ceo-cost-at investment 0))))

(defun action-time (action state)
  (/ (action-cost action state) (state-pps state)))

(defmethod action-desc :around ((action action) &optional (state nil))
  (if state
      (let ((base (call-next-method))
            (time (/ (action-cost action state) (state-pps state))))
        (format nil "~A - in ~A" base (fs time)))
      (call-next-method)))

(defmethod action-desc ((action action-investment) &optional (state nil))
  (let ((base (format nil "Invest: +~A ~A"
                      (action-qty action)
                      (investment-name (gethash (action-investment action) *investments*)))))
    (if state
        (format nil "~A for $~A (~A)" base
                (f (action-cost action state))
                (+ (action-qty action) (state-investment-level state (action-investment action))))
        base)))

(defmethod action-desc ((action action-ceo) &optional (state nil))
  (let ((base (format nil "CEO: +~A ~A"
                      (action-qty action)
                      (investment-name (gethash (action-investment action) *investments*)))))
    (if state
        (format nil "~A for $~A (~A)" base
                (f (action-cost action state))
                (+ (action-qty action) (state-ceo-level state (action-investment action))))
        base)))

(defmethod action-desc ((action action-tech) &optional state)
  (format nil "Tech: ~A for $~A"
          (tech-name (action-tech action))
          (f (action-cost action state))))

(defmethod action-desc ((action action-bootstrap) &optional state)
  (format nil "Bootstrap: ~A for $~A"
          (investment-name (gethash (action-investment action) *investments*))
          (f (action-cost action state))))

(defmethod action-apply ((action action-investment) state)
  (state-add-investment state (action-investment action) (action-qty action)))

(defmethod action-apply ((action action-ceo) state)
  (state-add-ceo state (action-investment action) (action-qty action)))

(defmethod action-apply ((action action-tech) state)
  (state-add-tech state (action-tech action)))

(defmethod action-apply ((action action-bootstrap) state)
  (let ((ikey (action-investment action)))
    (state-add-ceo (state-add-investment state ikey 10) ikey)))

(defmethod action-group-sort (a1 a2)
  (string< (symbol-name (class-name (class-of a1)))
           (symbol-name (class-name (class-of a2)))))

(defmethod action-group-sort ((a1 action-investment) (a2 action-investment))
  (< (position (action-investment a1) *investment-keys*)
     (position (action-investment a2) *investment-keys*)))

(defmethod action-group-sort ((a1 action-ceo) (a2 action-ceo))
  (< (position (action-investment a1) *investment-keys*)
     (position (action-investment a2) *investment-keys*)))

(defmethod action-group-sort ((a1 action-tech) (a2 action-tech))
  (< (action-cost a1 nil)
     (action-cost a2 nil)))

(defun next-actions (state &optional (invest-inc 10) one-shot)
  (let ((buy-investments (iterate (for (ikey investment) in-hashtable *investments*)
                                  (for qty = invest-inc)
                                  (collecting (make-instance 'action-investment :qty qty :investment ikey))))
        (improve-ceos (iterate (for (ikey investment) in-hashtable *investments*)
                               (for cur-level = (state-ceo-level state ikey))
                               (collecting (make-instance 'action-ceo :investment ikey))))
        (buy-techs (iterate (for tech in (set-difference *technologies* (state-techs state)))
                            (collecting (make-instance 'action-tech :tech tech))))
        (buy-max-investments (iterate (for (ikey investment) in-hashtable *investments*)
                                      (for next-achievement-level = (next-achievement-level (state-investment-level state ikey)))
                                      (when next-achievement-level
                                        (for qty = (- next-achievement-level (state-investment-level state ikey)))
                                        (when (/= qty invest-inc)
                                          (collecting (make-instance 'action-investment :qty qty :investment ikey))))))
        (bootstrap (iterate (for (ikey investment) in-hashtable *investments*)
                            (when (zerop (state-investment-level state ikey))
                              (collecting (make-instance 'action-bootstrap :investment ikey))))))
    (append buy-investments improve-ceos buy-techs
            (when one-shot (append bootstrap buy-max-investments)))))

(defun action-less-than (state a b)
  (let ((pps (state-pps state)))
    (if (plusp pps)
        (let ((afirst (+ (/ (action-cost a state) pps)
                         (/ (action-cost b state) (state-pps (action-apply a state)))))
              (bfirst (+ (/ (action-cost b state) pps)
                         (/ (action-cost a state) (state-pps (action-apply b state))))))
          (< afirst bfirst))
        (< (/ (action-cost a state) (state-pps (action-apply a state)))))))

(defun best-action (state)
  (labels ((choose (choices)
             (if (cdr choices)
                 (if (action-less-than state (second choices) (first choices))
                     (choose (cdr choices))
                     (choose (cons (car choices) (cddr choices))))
                 (car choices))))
    (choose (next-actions state 10 t))))

(defun next-action-sequence (state length)
  (iterate (repeat length)
           (for cur-state initially state then (action-apply action cur-state))
           (for action = (best-action cur-state))
           (collecting action)))

(defmethod action-combine (action1 action2)
  nil)

(defmethod action-combine ((action1 action-investment) (action2 action-investment))
  (when (eql (action-investment action1) (action-investment action2))
    (make-instance 'action-investment
                   :investment (action-investment action1)
                   :qty (+ (action-qty action1) (action-qty action2)))))

(defmethod action-combine ((a1 action-ceo) (a2 action-ceo))
  (when (eql (action-investment a1) (action-investment a2))
    (make-instance 'action-ceo
                   :investment (action-investment a1)
                   :qty (+ (action-qty a1) (action-qty a2)))))

(defun consolidate-actions (actions)
  (if (cdr actions)
      (let ((combo (action-combine (first actions) (second actions))))
        (if combo
            (consolidate-actions (cons combo (cddr actions)))
            (cons (car actions) (consolidate-actions (cdr actions)))))
      actions))

(defun reorder-quick-actions (actions state &optional (threshold 1))
  (let ((pps (state-pps state)))
    (multiple-value-bind (fast slow)
        (span (lambda (a) (< (/ (action-cost a state) pps) threshold))
              actions)
      (append (consolidate-actions (sort fast #'action-group-sort))
              slow))))

(defun next-quick-actions (state &optional (threshold 1))
  (consolidate-actions
   (sort (iterate (for s initially state then (action-apply a s))
                  (for a = (best-action s))
                  (while (< (/ (action-cost a s) (state-pps state)) threshold))
                  (collecting a))
         #'action-group-sort)))

(defun run-actions (state actions)
  (iterate (for s initially state then (action-apply a s))
           (for a in actions)
           (format t "~A~%" (action-desc a s))
           (finally (return s))))
