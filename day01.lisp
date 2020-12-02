(in-package :day01)

(defun read-input (in-path)
  (with-open-file (s in-path)
    (loop for i below 10 collect i)))
