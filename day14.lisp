(in-package :day14)

(defun read-input (inpath)
  (with-open-file (s inpath)
    (let (paras)
      (loop with para = nil
            for line = (read-line s nil) while line
            if (serapeum:string^= "mask" line) do
              (when (not (null para))
                (push (nreverse para) paras))
              (setf para nil)
              (push line para)
            else do
              (push line para)
            finally (push (nreverse para) paras))
      (nreverse paras))))

(defun parse-mask (line)
  (let ((split (ppcre:split "=" line)))
    (string-trim " " (second split))))

(defun parse-mem-accesses (inputs)
  (loop for input in inputs
        collect (ppcre:register-groups-bind ((#'parse-integer addr val))
                    ("mem\\[(\\d+)\\] = (\\d+)" input)
                  (list addr val))))

(defun convert (inputs)
  (loop for input in inputs
        collect (list (parse-mask (car input))
                      (parse-mem-accesses (cdr input)))))
                                    

(defun set-bits (value mask &aux (mlen (length mask)))
  (loop for c across mask
        for i from (1- mlen) downto 0
        if (char= c #\1) do
          (setf value (dpb 1 (byte 1 i) value))
        else if (char= c #\0) do
          (setf value (dpb 0 (byte 1 i) value)))
  value)

(defun exec (mask-n-accesses)
  (let ((memory (make-hash-table)))
    (loop for (mask accs) in mask-n-accesses do
      (loop for (addr val) in accs do
        (setf (gethash addr memory) (set-bits val mask))))
    memory))

(defun sum-vals (memory)
  (loop for v being the hash-values of memory sum v))

(defun sol1 (&optional (inpath "./input/day14"))
  (-<> (read-input inpath)
       (convert <>)
       (exec <>)
       (sum-vals <>)))

(defun fix-addrs (addr mask &aux (mask-len (length mask)))
  ;; first set all the 1s.
  (let ((base-addr (loop for c across mask
                         for i from (1- mask-len) downto 0
                         if (char= c #\1) do
                           (setf addr (dpb 1 (byte 1 i) addr))
                         finally (return addr))))
    (labels ((helper (i addr)
               (cond ((= i (1- mask-len))
                      (if (char= #\X (aref mask i))
                          (let ((bit-pos (- (1- mask-len) i)))
                            (list (dpb 1 (byte 1 bit-pos) addr)
                                  (dpb 0 (byte 1 bit-pos) addr)))
                          (list addr)))
                     (t (if (char= #\X (aref mask i))
                            (let ((bit-pos (- (1- mask-len) i)))
                              (nconc (helper (1+ i) (dpb 1 (byte 1 bit-pos) addr))
                                     (helper (1+ i) (dpb 0 (byte 1 bit-pos) addr))))
                            (helper (1+ i) addr))))))
      (helper 0 base-addr))))

(defun exec-v2 (mask-n-accesses)
  (let ((memory (make-hash-table)))
    (loop for (mask accs) in mask-n-accesses do
      (loop for (addr val) in accs do
        (loop for fixed-addr in (fix-addrs addr mask) do
          (setf (gethash fixed-addr memory) val))))
    memory))

(defun sol2 (&optional (inpath "./input/day14"))
  (-<> (read-input inpath)
       (convert <>)
       (exec-v2 <>)
       (sum-vals <>)))
