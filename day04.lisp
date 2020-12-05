(in-package :day04)

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
                (push (sjoin pp) pps)
                (setf pp nil)
              finally
                 (push (sjoin pp) pps))
        pps))))

(defparameter *mandatory-fields* '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"))

(defun valid-1-p (pp)
  (loop for m in *mandatory-fields* do
    (unless (search m pp)
      (unless (string= m "cid")
        (return-from valid-1-p nil))))
  t)

(defun sol1 ()
  (-<> (read-input "./input/day04")
       (valid-1-p <>)))

(defun valid-2-p (pp)
  (and (multiple-value-bind (m kv)
           (ppcre:scan-to-strings "(byr):([0-9]{4})\\b" pp)
         (when m
           (<= 1920 (parse-integer (aref kv 1)) 2002)))
       (multiple-value-bind (m kv)
           (ppcre:scan-to-strings "(iyr):([0-9]{4})\\b" pp)
         (when m
           (<= 2010 (parse-integer (aref kv 1)) 2020)))
       (multiple-value-bind (m kv)
           (ppcre:scan-to-strings "(eyr):([0-9]{4})\\b" pp)
         (when m
           (<= 2020 (parse-integer (aref kv 1)) 2030)))
       (ppcre:scan-to-strings "(hcl):(#[0-9a-f]{6})\\b" pp)
       (ppcre:scan-to-strings "(ecl):(amb|blu|brn|gry|grn|hzl|oth)\\b" pp)
       (ppcre:scan-to-strings "(pid):([0-9]{9})\\b" pp)
       (multiple-value-bind (m kv)
           (ppcre:scan-to-strings "(hgt):([0-9]+)(cm|in)\\b" pp)
         (when m
           (let ((dim (aref kv 2))
                 (val (parse-integer (aref kv 1))))
             (if (string= dim "cm")
                 (<= 150 val 193)
                 (<= 59 val 76)))))))

(defun sol2 ()
  (-<> (read-input "./input/day04")
       (loop for pp in <> if (valid-2-p pp) count 1)))
