(in-package :day01)

(defun read-input (in-path)
  (with-open-file (s in-path)
    (loop for line = (read-line s nil) while line
          collect (parse-integer line))))

(defmacro with-brute-force (((&rest inds) list) &body body)
  (labels ((rec (inds)
             (if (= 1 (length inds))
                 `(loop for ,(car inds) in ,list do ,@body)
                 `(loop for ,(car inds) in ,list do ,(rec (cdr inds))))))
    (rec inds)))

(defun find-sum-2 (nums)
  (with-brute-force ((i j) nums)
    (when (= (+ i j) 2020)
      (return-from find-sum-2 (values i j)))))

(defun sol1 ()
  (let ((nums (read-input "./input/day01")))
    (multiple-value-bind (i j)
        (find-sum-2 nums)
      (* i j))))

(defun find-sum-3 (nums)
  (with-brute-force ((i j k) nums)
    (when (= (+ i j k) 2020)
      (return-from find-sum-3 (values i j k)))))

(defun sol2 ()
  (let ((nums (read-input "./input/day01")))
    (multiple-value-bind (i j k)
        (find-sum-3 nums)
      (* i j k))))
