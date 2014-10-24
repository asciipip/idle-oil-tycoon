(in-package :iot)

(defvar *suffixes* '("" "K" "M" "G" "T" "P" "E" "Z" "Y"))

(defun f (n)
  (cond
    ((zerop n) "0")
    ((< n 1) (format nil "~,3E" n))
    (t
     (let ((order (truncate (log n 1000))))
       (if (>= order (length *suffixes*))
           (concatenate 'string
                        (f (/ n (expt 1000 (1- (length *suffixes*)))))
                        (car (last *suffixes*)))
           (format nil "~3$ ~A" (/ n (expt 1000 order)) (nth order *suffixes*)))))))

;; Inverse of F.
;; Note: (parse-num (f ...)) is lossy.
(defun parse-num (pretty-number)
  (multiple-value-bind (match groups)
      (scan-to-strings "^(\\d+)\\.(\\d+) ?([A-Z]+)$" pretty-number)
    (when match
      (* (+ (* (parse-integer (svref groups 0))
               (expt 10 (length (svref groups 1))))
            (parse-integer (svref groups 1)))
         (/ (expt 1000 (iterate (for c in-string (svref groups 2))
                                (summing (position (make-string 1 :initial-element c) *suffixes* :test #'equal))))
            (expt 10 (length (svref groups 1))))))))

(defun fs (seconds)
  (format nil "~{~2,'0D~^:~}"
          (iterate (for divisor in '(60 60 24))
                   (initially (setq s seconds))
                   (for (values s n) = (truncate s divisor))
                   (when (or (plusp s) (plusp n))
                     (collecting (truncate n) at beginning)))))
