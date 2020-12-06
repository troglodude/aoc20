(in-package :day04)

(defun read-paragraphs (inpath)
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

(defun valid-1-p (passport-data)
  (loop for mandatory-key in '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid") do
    (unless (search mandatory-key passport-data)
      (unless (string= mandatory-key "cid")
        (return-from valid-1-p nil))))
  t)

(defun sol1 ()
  (-<> (read-paragraphs "./input/day04")
       (loop for passport-data in <>
             if (valid-1-p passport-data) count 1)))

(defun valid-2-p (passport-data)
  (and (multiple-value-bind (match key-val)
           (ppcre:scan-to-strings "byr:([0-9]{4})\\b" passport-data)
         (when match
           (<= 1920 (parse-integer (aref key-val 0)) 2002)))
       (multiple-value-bind (match key-val)
           (ppcre:scan-to-strings "iyr:([0-9]{4})\\b" passport-data)
         (when match
           (<= 2010 (parse-integer (aref key-val 0)) 2020)))
       (multiple-value-bind (match key-val)
           (ppcre:scan-to-strings "eyr:([0-9]{4})\\b" passport-data)
         (when match
           (<= 2020 (parse-integer (aref key-val 0)) 2030)))
       (ppcre:scan "hcl:(#[0-9a-f]{6})\\b" passport-data)
       (ppcre:scan "ecl:(amb|blu|brn|gry|grn|hzl|oth)\\b" passport-data)
       (ppcre:scan "pid:([0-9]{9})\\b" passport-data)
       (multiple-value-bind (match key-val)
           (ppcre:scan-to-strings "hgt:([0-9]+)(cm|in)\\b" passport-data)
         (when match
           (let ((dim (aref key-val 1))
                 (val (parse-integer (aref key-val 0))))
             (if (string= dim "cm")
                 (<= 150 val 193)
                 (<= 59 val 76)))))))

(defun sol2 ()
  (-<> (read-paragraphs "./input/day04")
       (loop for passport-data in <>
             if (valid-2-p passport-data) count 1)))
