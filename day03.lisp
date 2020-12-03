(in-package :day03)

(defun read-input (in-path)
  (with-open-file (s in-path)
    (loop for line = (read-line s nil) while line
            collect line)))

(defun trees-with-slope (&key right down list)
  (let ((xpos 0)
        (len (length (car list)))
        (num-trees 0))
     (loop for l in list by (if right right #'cdr) do
       (when (char= #\# (char l xpos))
         (incf num-trees))
       (setf xpos (mod (+ xpos (if down down 1)) len)))
     num-trees))

(defun sol1()
  (-<> (read-input "./input/day03")
       (trees-with-slope :down 3 :list <>)))

(defun sol2 ()
  (let ((inp (read-input "./input/day03")))
    (* (trees-with-slope :down 3 :list inp)
       (trees-with-slope :down 1 :list inp)
       (trees-with-slope :down 5 :list inp)
       (trees-with-slope :down 7 :list inp)
       (trees-with-slope :right #'cddr :list inp))))
