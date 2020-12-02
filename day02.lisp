(in-package :day02)

(defun extract-policy (line)
  (ppcre:register-groups-bind
      ((#'parse-integer min max) ((lambda (c) (char c 0)) letter) password)
      ("(\\d+)-(\\d+)\\s+([a-z]):\\s(\\w+)" line)
    (list min max letter password)))

(defun read-input (in-path)
  (with-open-file (s in-path)
    (loop for line = (read-line s nil) while line
          collect (extract-policy line))))

(defun process (extracted)
  (loop for (mi ma l pw) in extracted
        for l-count = (count l pw)
        if (<= mi l-count ma) count 1))

(defun sol1 ()
  (-<> (read-input "./input/day02")
       (process <>)))

(defun process-2 (extracted)
  (loop for (mi ma l pw) in extracted
        for mi1 = (1- mi)
        for ma1 = (1- ma)
        if (or (and (char= (char pw mi1) l)
                    (char/= (char pw ma1) l))
               (and (char/= (char pw mi1) l)
                    (char= (char pw ma1) l)))
          count 1))

(defun sol2 ()
  (-<> (read-input "./input/day02")
       (process-2 <>)))
