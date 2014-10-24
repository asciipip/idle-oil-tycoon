(in-package :iot)

(defstruct achievement
  name
  investment
  level
  (profit-mult 1)
  (speed-mult 1))

(defvar *achievements*
  (labels ((list-achievements (name level &key (profit 1) (profits nil) (all-speed 1) (all-profit 1))
             (let ((real-profits (or profits
                                     (make-list (length *investment-keys*) :initial-element profit))))
               (append
                (iterate (for ikey in *investment-keys*)
                         (for investment = (gethash ikey *investments*))
                         (for p in real-profits)
                         (when (< 1 p)
                           (collecting (make-achievement :name (format nil "~A ~A" (investment-name investment) name)
                                                         :investment ikey
                                                         :level level
                                                         :profit-mult p))))
                (when (or (< 1 all-speed)
                          (< 1 all-profit))
                  (list (make-achievement :name (format nil "Oil Business ~A" name)
                                          :investment :all
                                          :level level
                                          :profit-mult all-profit
                                          :speed-mult all-speed)))))))
    (append
     (list-achievements "Investor"       10 :profit 3 :all-speed 2)
     (list-achievements "Up and Comer"   50 :profit 3 :all-speed 2)
     (list-achievements "Schmoozer"     100 :profit 3 :all-speed 2)
     (list-achievements "Leader"        200 :profit 3 :all-speed 2)
     (list-achievements "Tycoon"        300 :profits '(15 13 11 9 6 3 3 3) :all-speed 2)
     (list-achievements "Baron"         400 :profit 3 :all-speed 2)
     (list-achievements "Oligarch"      500 :profit 5 :all-speed 2)
     (list-achievements "Magnate"       600 :profits '(15 13 11 9 6 3 3 3) :all-profit 5/2)
     (list-achievements "Prince"        700 :profit 3 :all-profit 5/2)
     (list-achievements "King"          800 :profit 3 :all-profit 5/2)
     (list-achievements "Icon"          900 :profit 3 :all-profit 5/2)
     (list-achievements "Monopolist"   1000 :profit 8 :all-profit 25)
     (list-achievements "Emperor"      1100 :profit 3 :all-profit 5/2)
     (list-achievements "Czar"         1200 :profit 3 :all-profit 5/2)
     (list-achievements "Sultan"       1300 :profit 5 :all-profit 5)
     (list-achievements "Kaiser"       1400 :profit 3 :all-profit 5/2)
     (list-achievements "Dynast"       1500 :profit 6 :all-profit 4)
     (list-achievements "Potentate"    1600 :profit 3 :all-profit 5/2)
     (list-achievements "Commandant"   1700 :profit 3 :all-profit 5/2)
     (list-achievements "Mastermind"   1800 :profits '(3 3 5 5 5 3 3 3) :all-profit 5/2)
     (list-achievements "Kingpin"      1900 :profits '(3 3 1 1 1 3 3 3))
     (list-achievements "Overlord"     2000 :profits '(5 5 1 1 1 5 5 5))
     (list-achievements "Big Kahuna"   2100 :profits '(3 3 1 1 1 3 3 3))
     (list (make-achievement :name "The Gas Royalty Man"
                             :investment :gas-royalties
                             :level 2200
                             :profit-mult 3)
           (make-achievement :name "The Oil Royalty Man"
                             :investment :oil-royalties
                             :level 2200
                             :profit-mult 3)
           (make-achievement :name "The Saudi Field Man"
                             :investment :saudi-field
                             :level 2200
                             :profit-mult 3)))))

(let ((max-level (iterate (for a in *achievements*)
                          (maximizing (achievement-level a)))))
  (labels ((fill-hash (hash mult-func)
             (iterate (for a in *achievements*)
                      (for ikey = (achievement-investment a))
                      (for m = (funcall mult-func a))
                      (for (values v found) = (gethash ikey hash (make-array (1+ max-level) :initial-element 1)))
                      (when (not found) (setf (gethash ikey hash) v))
                      (iterate (for x in-vector v with-index i from (achievement-level a))
                               (setf (svref v i) (* x m))))))
    (let ((profit-mults (make-hash-table))
          (speed-mults (make-hash-table)))
      (fill-hash profit-mults #'achievement-profit-mult)
      (fill-hash speed-mults #'achievement-speed-mult)
      (defun achievement-mult (investment-key level all-level &key (achievement-key #'achievement-profit-mult))
        (let ((hash (if (eq achievement-key #'achievement-profit-mult)
                        profit-mults
                        speed-mults)))
          (labels ((get-mult (key level)
                     (svref (gethash key hash)
                            (if (<= level max-level)
                                level
                                max-level))))
            (* (get-mult investment-key level)
               (get-mult :all all-level))))))))

(defun next-achievement-level (level)
  (iterate (for a in *achievements*)
           (when (< level (achievement-level a))
             (minimizing (achievement-level a)))))
