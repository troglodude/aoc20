(in-package :day13)

(defun read-input (inpath)
  (with-open-file (s inpath)
    (let ((timestamp (parse-integer (read-line s)))
          (bus-ids nil))
      (ppcre:do-register-groups (id)
          ("(\\d+|x),?" (read-line s))
        (push id bus-ids))
      (list timestamp (nreverse bus-ids)))))

(defun num-ids (str-ids)
  (loop for sid in str-ids
        if (digit-char-p (char sid 0)) collect
          (parse-integer sid)))

(defun least-id-multiple-greater-than (target ids)
  (loop for id in ids
        collect (loop for i from 0
                      for multiple = (* i id)
                      while (< multiple target)
                      finally (return (list id multiple)))))

(defun earliest (ids-n-mults)
  (reduce (lambda (x y)
            (if (< (second x) (second y)) x y))
          ids-n-mults))

(defun sol1 (&optional (inpath "./input/day13"))
  (destructuring-bind (target ids)
      (read-input inpath)
    (let ((nids (num-ids ids)))
      (destructuring-bind (id ts)
          (earliest (least-id-multiple-greater-than target nids))
        (* (- ts target) id)))))

;; from AoC16
(defun eea (a b)
  "Extended Eudlidean Algorithm (from Cormen et al.).

Returns values (d x y) such that d = x*a + y*b."
  (if (zerop b)
      (values a 1 0)
      (multiple-value-bind (d xx yy)
          (eea b (mod a b))
        (values d yy (- xx (* (floor a b) yy))))))

(defun modular-inverse (number modulus)
  "Calculate the modular inverse of `NUMBER' modulo `MODULUS'."
  ;; not sure what happens if number is not invertible...
  (multiple-value-bind (d x y)
      (eea number modulus)
    (declare (ignore y d))
    (mod x modulus)))

(defun collect-congurences (input)
  "Helper function.

Converts a list of strings `INPUT' with elements being either number
strings or 'X'to a list with elements being lists of the form `(Y MODULUS)'"
  (loop for inp in input
	for i from 0
        if (digit-char-p (char inp 0))
          collect (let* ((modulus (parse-integer inp))
                         (res (mod (+ (- i) modulus) modulus)))
                    (list res modulus))))

(defun crt (sim-congruences)
  "Solves simultaneous congruences.

Congruences are given in the form of a list the elements of which are lists
of the form `(Y MODULUS)'.

That is, the list represents congruences
                X = Y1 mod MODULUS
                X = Y2 mod MODULUS
                ...
                X = Yn mod MODULUS
with `X' being the simultaneous solution for all congruences."
  (let ((m (reduce #'* sim-congruences :key #'second)))
    (loop for cgr in sim-congruences
	  for mi = (cadr cgr)
	  for ai = (/ m mi)
	  for bi = (car cgr)
	  for xi = (mod (* (modular-inverse ai mi) bi) mi)
	  sum (* ai xi) into sol
	  finally (return (mod sol m)))))

(defun sol2 (&optional (inpath "./input/day13"))
  (destructuring-bind (target ids)
      (read-input inpath)
    (declare (ignorable target))
    (-<> (collect-congurences ids)
         (crt <>))))
