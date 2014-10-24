(in-package :iot)

(defun experience-mult (experience)
  (if (zerop experience)
      1
      (truncate (expt 2 (log experience 5d0)))))
