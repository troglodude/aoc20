(in-package :day01)

(defun read-input (in-path)
  (with-open-file (s in-path)
    (loop for line = (read-line s nil) while line
          collect (parse-integer line))))

(defmacro with-brute-force (((&rest inds) list) &body body)
  (labels ((rec (inds)
             (if inds
                 `(loop for ,(car inds) in ,list do ,(rec (cdr inds)))
                 `(progn ,@body))))
    (rec inds)))

(defun sol1 ()
  (let ((nums (read-input "./input/day01")))
    (with-brute-force ((i j) nums)
      (when (= (+ i j) 2020)
        (* i j)))))

(defun sol2 ()
  (let ((nums (read-input "./input/day01")))
    (with-brute-force ((i j k) nums)
      (when (= (+ i j k) 2020)
        (* i j k)))))
