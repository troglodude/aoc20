(in-package :day16)

(defun read-input (inpath)
  (with-open-file (s inpath)
    (let ((ranges (loop for line = (read-line s nil) while (string/= line "")
                        collect line))
          (my-ticket (loop for line = (read-line s nil) while (string/= line "")
                           collect line))
          (nearby-tickets (loop for line = (read-line s nil) while line
                                collect line)))
      (list ranges my-ticket nearby-tickets))))

(defun parse-ranges (ranges)
  (loop for range in ranges
        collect (ppcre:register-groups-bind (what (#'parse-integer r11 r12 r21 r22))
                    ("([a-z ]+):\\s*(\\d+)-(\\d+)\\s*or\\s*(\\d+)-(\\d+)" range)
                  (list what (list r11 r12) (list r21 r22)))))

(defun parse-my-ticket (my-ticket)
  (loop for n in (ppcre:split "," (second my-ticket))
        collect (parse-integer n)))

(defun parse-nearby-tickets (nearby-tickets)
  (mapcar #'(lambda (tkt)
              (mapcar #'parse-integer
                      (ppcre:split "," tkt)))
          (cdr nearby-tickets)))

(defun parse-notes (notes)
  (destructuring-bind (ranges my-ticket nearby-tickets) notes
    (list (parse-ranges ranges)
          (parse-my-ticket my-ticket)
          (parse-nearby-tickets nearby-tickets))))

(defun ticket-valid-p (ticket ranges)
  (labels ((chk-rng (num rng)
             (destructuring-bind (name (r11 r12) (r21 r22)) rng
               (declare (ignorable name))
               (or (<= r11 num r12)
                   (<= r21 num r22)))))
  (loop for num in ticket
        if (not (some (lambda (rng) (chk-rng num rng)) ranges)) do
          (return (values nil num))
        finally (return (values t)))))

(defun invalid-tickets (nearby-tickets ranges)
  (loop for nb-tkt in nearby-tickets
        if (multiple-value-bind (valid? offender)
               (ticket-valid-p nb-tkt ranges)
             (unless valid? offender))
          collect it))

(defun sol1 (&optional (inpath "./input/day16"))
  (let ((inp (read-input inpath)))
    (destructuring-bind (ranges my-ticket nearby-tickets)
        (parse-notes inp)
      (declare (ignorable my-ticket))
      (reduce #'+ (invalid-tickets nearby-tickets ranges)))))

(defun valid-tickets (nearby-tickets ranges)
  (remove-if-not (lambda (tkt)
                   (ticket-valid-p tkt ranges))
                 nearby-tickets))

(defun transpose (lol)
  (apply #'mapcar (lambda (&rest args) args) lol))

(defun match-col-to-ranges (col ranges)
  (loop with temp = ranges
        for num in col do
          (setf temp (remove-if-not (lambda (ran)
                                      (destructuring-bind (name (r11 r12) (r21 r22))
                                          ran
                                        (declare (ignorable name))
                                        (or (<= r11 num r12)
                                            (<= r21 num r22))))
                                    temp))
        finally (return temp)))
          
(defun initial-match-up (cols ranges)
  (loop with temp = ranges
        for col in cols
        for match = (match-col-to-ranges col temp)
        if (and match (= 1 (length match))) do
          (setf temp (remove-if (lambda (ran)
                                  (string= (first ran) (caar match)))
                                temp))
        collect match))

(defun only-names (list-o-ranges)
  (mapcar (lambda (r) (mapcar #'first r)) list-o-ranges))

(defun all-singleton (matchups)
  (every (lambda (x) (= 1 (length x))) matchups))

(defun prune (matchups)
  (loop with seen = (make-hash-table :test #'equal)
        while (not (all-singleton matchups)) do
          (let ((next-singleton (find-if (lambda (x)
                                           (and (not (gethash x seen))
                                                (= 1 (length x))))
                                         matchups)))
            (setf (gethash next-singleton seen) t)
            (loop for m on matchups
                  if (> (length (car m)) 1) do
                    (setf (car m)
                          (remove (first next-singleton)
                                  (car m)
                                  :test #'string=)))))
  matchups)

(defun flatten (matchups)
  (mapcar #'first matchups))

(defun starts-with (what str)
  (let ((res (search what str)))
    (when res
      (zerop res))))

(defun select (my-ticket matchup)
  (loop for m in matchup
        for i from 0
        if (starts-with "departure" m)
          collect (nth i my-ticket)))

(defun sol2 (&optional (inpath "./input/day16"))
  (destructuring-bind (ranges my-ticket nearby-tickets)
      (parse-notes (read-input inpath))
    (declare (ignorable my-ticket))
    (-<> (valid-tickets nearby-tickets ranges)
         (transpose <>)
         (initial-match-up <> ranges)
         (only-names <>)
         (prune <>)
         (flatten <>)
         (select my-ticket <>)
         (reduce #'* <>))))
