(in-package :day06)

;; todo extract 
(defun read-input (inpath)
  (with-open-file (s inpath)
    (labels ((sjoin (s)
               (format nil "~{~a~^ ~}" s)))
      (let ((pps nil))
        (loop for line = (read-line s nil) while line
              with pp = nil
              if (string/= line "") do
                (push line pp)
              else do
                (push pp pps)
                (setf pp nil)
              finally
                 (push pp pps))
        (nreverse pps)))))

(defun mk-set (list-of-strings)
  (sort (remove-duplicates (loop for str in list-of-strings
                                 nconc (ppcre:split "" str))
                           :test #'string=)
        #'string<=))

(defun unique-questions (list-of-answers)
  (loop for answer in list-of-answers
        collect (length (mk-set answer))))

(defun sol1 ()
  (-<> (read-input "./input/day06")
       (unique-questions <>)
       (reduce #'+ <>)))

(defun explode-answers (answers)
  (loop for answer in answers
        collect (loop for indiv-answer in answer
                      collect (coerce indiv-answer 'list))))

(defun combine-answers (answers)
  (loop for group-answers in answers
        collect (length (reduce #'intersection group-answers))))

(defun sol2 ()
  (-<> (read-input "./input/day06")
       (explode-answers <>)
       (combine-answers <>)
       (reduce #'+ <>)))
