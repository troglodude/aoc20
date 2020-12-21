(in-package :day15)

(defparameter *input* "0,13,1,16,6,17")
(defparameter *test1* "0,3,6")
(defparameter *test2* "1,3,2")
(defparameter *test3* "2,1,3")
(defparameter *test4* "3,1,2")

(defun parse-input (input)
  (loop for n in (ppcre:split "," input)
        collect (parse-integer n)))

;; number => (times-spoken last-turn snd-to-last-turn)
(defun play (starting-numbers &optional (fin 2020))
  (let ((last nil)
        (map (make-hash-table)))
    ;; enter starting numbers
    (loop for sn in starting-numbers
          for turn from 1 do
            (setf (gethash sn map) (list 1 turn turn)
                  last sn))
    ;; now, take turns
    (loop for turn from (1+ (length starting-numbers))
          while (<= turn fin) do
            (ap:slet (gethash last map)
              (setf last (if (= 1 (first ap:it)) 0 (- (second ap:it) (third ap:it))))
              (cond (ap:it
                     (incf (first ap:it))
                     (setf (third ap:it) (second ap:it)
                           (second ap:it) turn))
                    (t
                     (setf ap:it (list 1 turn turn)))))
          finally (return last))))

(defun sol1 ()
  (-<> *input*
       (parse-input <>)
       (play <>)))

;; took 27,600,650 microseconds (27.600650 seconds) to run.
;;       4,564,620 microseconds ( 4.564620 seconds, 16.54%) of which was spent in GC.
;; During that period, and with 8 available CPU cores,
;;      27,496,538 microseconds (27.496538 seconds) were spent in user mode
;;         164,721 microseconds ( 0.164721 seconds) were spent in system mode
;;  463,996,784 bytes of memory allocated.
;;  76,427 minor page faults, 0 major page faults, 0 swaps.
(defun sol2 (&optional (inp *input*))
  (-<> (parse-input inp)
       (play <> 30000000)))
