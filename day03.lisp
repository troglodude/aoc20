(in-package :day03)

(defun read-input (in-path)
  (with-open-file (s in-path)
    (let ((w 0)
          (h 0))
      (values
       (loop for line = (read-line s nil) while line
            do (incf h)
               (setf w (length line))
             collect line)
       w h))))

(defun count-trees-3-1 (lol)
  (let ((xpos 0)
        (ll (length (car lol)))
        (num-trees 0))
    (loop for l in lol do
      (when (char= #\# (char l xpos))
        (incf num-trees))
      (setf xpos (mod (+ xpos 3) ll)))
    num-trees))

(defun sol1()
  (-<> (read-input "./input/day03")
       (count-trees-3-1 <>)))

(defun count-trees-1-1 (lol)
  (let ((xpos 0)
        (ll (length (car lol)))
        (num-trees 0))
    (loop for l in lol do
      (when (char= #\# (char l xpos))
        (incf num-trees))
      (setf xpos (mod (+ xpos 1) ll)))
    num-trees))

(defun count-trees-5-1 (lol)
  (let ((xpos 0)
        (ll (length (car lol)))
        (num-trees 0))
    (loop for l in lol do
      (when (char= #\# (char l xpos))
        (incf num-trees))
      (setf xpos (mod (+ xpos 5) ll)))
    num-trees))

(defun count-trees-7-1 (lol)
  (let ((xpos 0)
        (ll (length (car lol)))
        (num-trees 0))
    (loop for l in lol do
      (when (char= #\# (char l xpos))
        (incf num-trees))
      (setf xpos (mod (+ xpos 7) ll)))
    num-trees))

(defun count-trees-1-2 (lol)
  (let ((xpos 0)
        (ll (length (car lol)))
        (num-trees 0))
    (loop for l in lol by #'cddr do
      (when (char= #\# (char l xpos))
        (incf num-trees))
      (setf xpos (mod (+ xpos 1) ll)))
    num-trees))

(defun sol2 ()
  (let* ((inp (read-input "./input/day03"))
         (1-1 (count-trees-1-1 inp))
         (3-1 (count-trees-3-1 inp))
         (5-1 (count-trees-5-1 inp))
         (7-1 (count-trees-7-1 inp))
         (1-2 (count-trees-1-2 inp)))
    (* 1-1 3-1 5-1 7-1 1-2)))
