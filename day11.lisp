(in-package :day11)

(defstruct board
  (str nil :type string)
  (width 0 :type integer)
  (height 0 :type integer))

(defun read-input (inpath)
  (with-open-file (s inpath)
    (let* ((w 0)
           (res (with-output-to-string (str)
                  ;; initial run to get width
                  (loop for c = (read-char s nil)
                        while (char/= c #\Newline) do
                          (write-char c str)
                          (incf w))
                  ;; rest of the chars
                  (loop for c = (read-char s nil) while c
                        if (not (char= c #\Newline)) do
                          (write-char c str)))))
      (make-board :str res :width w :height (/ (length res) w)))))

(defun at (x y board)
  (aref (board-str board) (+ (* y (board-width board)) x)))

(defun (setf at) (n x y board)
  (setf (aref (board-str board) (+ (* y (board-width board)) x))
        n))

(defmethod print-object ((board board) stream)
  (print-unreadable-object (board stream :type t :identity t)
    (format stream "~%")
    (loop for j below (board-height board) do
      (loop for i below (board-width board) do
        (format stream "~a " (at i j board)))
      (terpri stream))))

;; (defmethod print-object ((board board) stream)
;;   (print-unreadable-object (board stream :type t :identity t)))

(defparameter *nbr-posns* '((-1 -1) (0 -1) (1 -1)
                            (-1 0)         (1 0)
                            (-1 1)  (0 1)  (1 1)))

(defun neighbours (x y board)
  (loop for (i j) in *nbr-posns*
        for nxpos = (+ x i)
        for nypos = (+ y j)
        if (and (< -1 nxpos (board-width board))
                (< -1 nypos (board-height board)))
          collect (at nxpos nypos board)))

(defun all-neighbours-empty-p (neighbours)
  (every (lambda (nb) (or (char= nb #\.) (char= nb #\L)))
         neighbours))

(defun n-or-more-occupied-p (n neighbours)
  (>= (count-if (lambda (nb) (char= nb #\#)) neighbours) n))

(defstruct env
  (nbr (constantly nil) :type function)
  (empty-pred (constantly nil) :type function)
  (occd-pred (constantly nil) :type function))

(defparameter *sol1-env* (make-env :nbr #'neighbours
                                   :empty-pred #'all-neighbours-empty-p
                                   :occd-pred (lambda (nbrs)
                                                (n-or-more-occupied-p 4 nbrs))))

(defun exec1 (board changes env
              &aux (bw (board-width board))
                (bh (board-height board)))
  (let ((did-change? nil))
    (loop for j below bh do
      (loop for i below bw do
        (let ((nbs (funcall (env-nbr env) i j board)))
          (case (at i j board)
            (#\L (when (funcall (env-empty-pred env) nbs)
                   (setf (aref changes (+ (* j bw) i)) 1
                         did-change? t)))
            (#\# (when (funcall (env-occd-pred env) nbs)
                   (setf (aref changes (+ (* j bw) i)) 1
                         did-change? t)))))))
    (loop for j below bh do
      (loop for i below bw do
        (when (plusp (aref changes (+ (* j bw) i)))
          (setf (at i j board) (if (char= (at i j board) #\#) #\L #\#)))))
    (values board did-change?)))

(defun exec (board env)
  (let ((board board)
        (changes (make-array (length (board-str board)) :element-type 'bit)))
    (loop for ctr from 0 do
      (multiple-value-bind (board change?)
          (exec1 board changes env)
        (when (null change?)
          (return (values board ctr)))
        ;; that does not seem to have any effect on consing
        ;; perhaps the memory comes from `neighbours'?
        (setf changes (bit-xor changes changes))))))

(defun sol1 (&optional (inpath "./input/day11"))
  (let* ((b (read-input inpath)))
    (count-if (lambda (c) (char= c #\#))
              (board-str (exec b *sol1-env*)))))

(defun neighbours-in-view (x y board)
  (labels ((in-bounds-p (i j)
             (and (< -1 i (board-width board))
                  (< -1 j (board-height board))))
           (cast (i j dir)
             (labels ((in-bounds-p (i j)
                        (and (< -1 i (board-width board))
                             (< -1 j (board-height board)))))
               (destructuring-bind (dx dy) dir
                 (loop for ni = (+ i dx) then (+ ni dx)
                       for nj = (+ j dy) then (+ nj dy) do
                         (if (not (in-bounds-p ni nj))
                             (return nil)
                             (let ((c (at ni nj board)))
                               (when (char/= c #\.)
                                 (return c))))
                       finally
                          (return (at (- ni dx) (- nj dy) board)))))))
    (loop for dir in *nbr-posns*
          for cast = (cast x y dir) if cast collect it)))

(defparameter *sol2-env* (make-env :nbr #'neighbours-in-view
                                   :empty-pred #'all-neighbours-empty-p
                                   :occd-pred (lambda (nbrs)
                                                (n-or-more-occupied-p 5 nbrs))))

(defun sol2 (&optional (inpath "./input/day11"))
  (let* ((b (read-input inpath)))
    (count-if (lambda (c) (char= c #\#)) (board-str (exec b *sol2-env*)))))
