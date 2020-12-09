(in-package :day09)

(defun read-input (inpath)
  (with-open-file (s inpath)
    (let ((ary (make-array 50 :element-type 'integer
                              :adjustable t
                              :fill-pointer 0)))
      (loop for line = (read-line s nil) while line
            do (vector-push-extend (parse-integer line) ary))
      ary)))


(defun find-summand-alt (num num-ary idx winsz)
  (let ((start (1- idx)))
    (dotimes (j winsz)
      (let ((curr (- start j)))
        (alexandria:if-let (res (find (- num (aref num-ary curr)) num-ary
                                      :from-end t
                                      :start (- idx winsz)
                                      :end curr))
          (return-from find-summand-alt (values res curr)))))))

(defun find-summand (num num-ary idx winsz)
  (loop for j from (1- idx) downto (- idx winsz)
        for cand1 = (aref num-ary j)
        for diff = (- num cand1)
        for sumd2 = (find diff num-ary :from-end t :start (- idx winsz) :end j)
        if sumd2 do
          (return (values diff cand1 j sumd2))
        finally (return nil)))

(defun first-non-sum (num-ary &key (window-size 25))
  (when (< (length num-ary) window-size)
    (return-from first-non-sum nil))
  (loop for i from window-size
        for elt = (aref num-ary i)
        if (not (find-summand elt num-ary i window-size)) do
          (return (values elt i))))

(defun sol1 (&key (inpath "./input/day09") (wsz 25))
  (-<> (read-input inpath)
       (first-non-sum <> :window-size wsz)))

(defun prune (num-ary threshold)
  (remove-if (lambda (x) (> x threshold)) num-ary))

(defun sliding-window (start length num-ary)
  (make-array length :displaced-to num-ary :displaced-index-offset start))

(defun brute-force-window (num-ary &key (wsz 25))
  (let* ((non-sum (first-non-sum num-ary :window-size wsz))
         (pruned (prune num-ary non-sum))
         (tgt-pos (position non-sum pruned)))
    (loop for winsz from tgt-pos downto 2 do
      (loop for start from 0 while (< (+ start winsz) tgt-pos)
            for win = (sliding-window start winsz num-ary)
            if (= (reduce #'+ win) non-sum) do
              (return-from brute-force-window win)))))

(defun min-max (ary)
  (loop for x across ary maximize x into ma minimize x into mi
        finally (return (values mi ma))))

(defun sol2 (&key (inpath "./input/day09") (wsz 25))
  (let* ((inp (read-input inpath))
         (res (brute-force-window inp :wsz wsz)))
    (multiple-value-bind (mi ma)
        (min-max res)
      (+ mi ma))))

;; took 1,534,334 microseconds (1.534334 seconds) to run.
;;          1,842 microseconds (0.001842 seconds, 0.12%) of which was spent in GC.
;; During that period, and with 8 available CPU cores,
;;      1,535,773 microseconds (1.535773 seconds) were spent in user mode
;;            743 microseconds (0.000743 seconds) were spent in system mode
;;  8,415,216 bytes of memory allocated.
;; 76096372 (27 bits, #x4892374)
