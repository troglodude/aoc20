(in-package :day07)

(defun parse-line (line)
  (let ((init-pos (search "bags" line))
        (rst nil))
    (ppcre:do-register-groups ((#'parse-integer num) col)
        ("(\\d+) (.*?)bag" line)
      (push (list num (string-trim " " col)) rst))
    (list (subseq line 0 (1- init-pos))
          (nreverse rst))))

(defun read-input (inpath)
  (with-open-file (s inpath)
    (loop for line = (read-line s nil) while line
          collect (parse-line line))))

(defun count-bags (rules)
  (let ((cols nil)
        (q nil))
    (push "shiny gold" q)
    (loop while q
          for goal = (pop q) do
            (loop for (head body) in rules do
              (loop for (nil next) in body
                    if (string= next goal) do
                      (setf cols (adjoin head cols :test #'string=)
                            q    (append q (list head))))))
    cols))

(defun sol1 ()
  (-<> (read-input "./input/day07")
       (count-bags <>)
       (length <>)))

(defun map-from-alist (rules)
  (let ((map (make-hash-table :test #'equal)))
    (loop for (head body) in rules do
      (setf (gethash head map) body))
    map))

(defun count-bags-2 (head rules)
  (loop for (num subgoal) in (gethash head rules)
        sum (+ num (* num (count-bags-2 subgoal rules)))))

(defun sol2 ()
  (-<> (read-input "./input/day07")
       (map-from-alist <>)
       (count-bags-2 "shiny gold" <>)))
