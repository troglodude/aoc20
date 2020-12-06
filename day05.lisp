(in-package :day05)

(defun read-input (inpath)
  (with-open-file (s inpath)
    (loop for line = (read-line s nil) while line collect line)))

(defun id-from-partition (p)
  (let ((row (cons 0 127))
        (col (cons 0 7)))
    (loop for i below 7
          for c = (aref p i)
          if (char= c #\F) do
            (setf (cdr row) (- (cdr row) (1+ (floor (- (cdr row) (car row)) 2))))
          else if (char= c #\B) do
            (setf (car row) (+ (car row) (1+ (floor (- (cdr row) (car row)) 2)))))
    (loop for i below 3
          for c = (aref p (+ i 7))
          if (char= c #\L) do
            (setf (cdr col) (- (cdr col) (1+ (floor (- (cdr col) (car col)) 2))))
          else if (char= c #\R) do
            (setf (car col) (+ (car col) (1+ (floor (- (cdr col) (car col)) 2)))))
    (+ (* (car row) 8) (car col))))

(defun sol1 ()
  (let ((inp (read-input "./input/day05")))
    (reduce #'max inp :key #'id-from-partition)))

(defun missing (ids)
  (let ((len (length ids))
        (sorted (sort ids #'<)))
    (loop for i from 1 below (1- len)
          for j = (1+ i)
          if (/= (- (nth j sorted) (nth i sorted)) 1) do
            (return-from missing (1+ (nth i sorted))))))

(defun sol2 ()
  (-<> (read-input "./input/day05")
       (loop for bp in <> collect (id-from-partition bp))
       (missing <>)))
