(in-package :iot)

;; I like Common Lisp, but some surprising things are not built into the language...
(defun copy-array (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

;; Ditto.  Like Haskell's span, but returns multiple values.
(defun span (predicate list)
  (labels ((span-r (rest result)
             (if (endp rest)
                 (values (reverse result) nil)
                 (if (funcall predicate (car rest))
                     (span-r (cdr rest) (cons (car rest) result))
                     (values (reverse result) rest)))))
    (span-r list nil)))
