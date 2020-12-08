(in-package :day08)

(defstruct (instruction (:conc-name insn-))
  (operation "" :type string)
  (argument 0 :type integer)
  (repeat 0 :type integer)
  (toggled nil :type boolean))

(defstruct env
  (acc 0 :type integer)
  (pc 0 :type integer)
  (plen 0 :type integer))

(defun parse-instruction (line)
  (destructuring-bind (op arg)
      (ppcre:split " " line)
    (make-instruction :operation op :argument (parse-integer arg))))

(defun read-input (inpath)
  (with-open-file (s inpath)
    (let ((program (make-array 10 :element-type 'instruction
                                  :adjustable t
                                  :fill-pointer 0)))
      (loop for line = (read-line s nil) while line do
        (vector-push-extend (parse-instruction line) program))
      program)))

(defun dispatch (insn)
  (alexandria:switch ((insn-operation insn) :test #'string=)
    ("nop" (lambda (insn env)
             (when (= (insn-repeat insn) 1)
               (throw :repeated (env-acc env)))
             (incf (insn-repeat insn))
             (incf (env-pc env))))
    ("acc" (lambda (insn env)
             (when (= (insn-repeat insn) 1)
               (throw :repeated (env-acc env)))
             (incf (insn-repeat insn))
             (incf (env-acc env) (insn-argument insn))
             (incf (env-pc env))))
    ("jmp" (lambda (insn env)
             (when (= (insn-repeat insn) 1)
               (throw :repeated (env-acc env)))
             (incf (insn-repeat insn))
             (incf (env-pc env) (insn-argument insn))))))

(defun exec (program &aux (prog-len (length program)))
  (let ((env (make-env :plen prog-len)))
    (catch :repeated
      (loop while (< (env-pc env) prog-len)
            for insn = (aref program (env-pc env)) do
              (funcall (dispatch insn) insn env)))))

(defun sol1 ()
  (-<> (read-input "./input/day08")
       (exec <>)))

(defun toggle-name (name)
  (cond ((string= name "nop") "jmp")
        ((string= name "jmp") "nop")
        (t name)))

(defun toggle-next (program prev)
  (when (>= prev 0)
    (anaphora:slet (insn-operation (aref program prev))
      (setf anaphora:it (toggle-name anaphora:it))))
  ;; reset the program instructions' "repeat" counter
  (loop for insn across program do
    (setf (insn-repeat insn) 0))
  (let ((next (position-if (lambda (x)
                             (or (string= x "nop")
                                 (string= x "jmp")))
                           program
                           :key #'insn-operation
                           :start (1+ prev))))
    (when next
      (anaphora:slet (insn-operation (aref program next))
        (setf anaphora:it (toggle-name anaphora:it)))
      (values program next))))

(defun dispatch-2 (insn)
  (alexandria:switch ((insn-operation insn) :test #'string=)
    ("nop" (lambda (insn env)
             (when (= (insn-repeat insn) 1)
               (throw :repeated :asdf))
             (incf (insn-repeat insn))
             (incf (env-pc env))))
    ("acc" (lambda (insn env)
             (when (= (insn-repeat insn) 1)
               (throw :repeated :asdf))
             (incf (insn-repeat insn))
             (incf (env-acc env) (insn-argument insn))
             (incf (env-pc env))))
    ("jmp" (lambda (insn env)
             (when (= (insn-repeat insn) 1)
               (throw :repeated :asdf))
             (incf (insn-repeat insn))
             (incf (env-pc env) (insn-argument insn))))))

(defun exec-fini (program &aux (plen (length program)))
  (let ((env (make-env :plen plen))
        (cp (copy-seq program))
        (idx -1))
    (loop do
      (let ((exec-res (catch :repeated
                        (loop while (< (env-pc env) plen)
                              for insn = (aref cp (env-pc env)) do
                                (funcall (dispatch-2 insn) insn env)
                              finally (return (env-acc env))))))
        (if (equal exec-res :asdf)
            ;; toggling did not do anything, prepare for next round
            (multiple-value-bind (new-p next-idx)
                (toggle-next cp idx)
              (setf cp            new-p
                    idx           next-idx
                    (env-acc env) 0
                    (env-pc env)  0))
            ;; we got a value different from `:asdf'
            ;; which means we finished properly.
            (return-from exec-fini exec-res))))))

(defun sol2 ()
  (-<> (read-input "./input/day08")
       (exec-fini <>)))
