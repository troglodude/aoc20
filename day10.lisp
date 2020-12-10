(in-package :day10)

(defun read-input (inpath)
  (with-open-file (s inpath)
    (loop for line = (read-line s nil) while line
          collect (parse-integer line))))

(defun set-from-list (list)
  (let ((set (make-hash-table)))
    (values set (loop for n in list
                      do (setf (gethash n set) t)
                      maximize n)
            (length list))))

(defun check-outlet (c set)
  (loop for i from 1 to 3
        for tgt = (+ c i)
        if (gethash tgt set) do
          (return tgt)))

(defun trace-adapters (adaplist)
  (multiple-value-bind (set max-adapter len)
      (set-from-list adaplist)
    (loop repeat len
          with 1-diffs = 0
          with 3-diffs = 0
          for prev = 0 then next
          for next = (check-outlet prev set) do
            (case (- next prev)
              (1 (incf 1-diffs))
              (3 (incf 3-diffs)))
          finally
             (case (- (+ max-adapter 3) next)
               (1 (incf 1-diffs))
               (3 (incf 3-diffs)))
             (return (values 1-diffs 3-diffs)))))

(defun sol1 (&key (inpath "./input/day10"))
  (-<> (read-input inpath)
       (trace-adapters <>)
       (multiple-value-bind (1s 3s)
           <>
         (* 1s 3s))))


;; Some notes (for later reference)
;; Firstly, consider the sorted list of adapters and calculate diffs
;; between adjacent elements:
;;                        X-a1-a2-a3-a4-a5-X, where a(i+1)=ai
;; Runs of 1s indicate a chain of elements that contribute to the final
;; number.
;; A singleton run of 1 can be disregarded since the corresponding elements
;; cannot be omitted, since omitting one leads to a difference of 4:
;;                 diffs:  3  1  3
;;                        / \/ \/ \
;;                 chain: X--a--b--Y
;; Thus, a singleton run contributes zero to the final product.
;;
;; A run `(1 1)' corresponds to a single element that can be omitted:
;;                 diffs:  3  1  1  3
;;                        / \/ \/ \/ \
;;                 chain: X--a--b--c--Y
;; Here, `b' can be in a path, or it can be omitted. A run `(1 1)' contributes
;; 2 to the final product.
;;
;; In general, for runs (1 .. 1) of length greater than 2 the
;; contribution is essentially the the sum of the number of subsets of
;; size less than 3, i.e.
;;             d * /n\ + /n\ + /n\
;;                 \0/   \1/   \2/
;; where `d' is a factor that eliminates the contribution of the empty set.
;; This is necessary for diff-chains longer than 3 because omitting
;; all "inner" nodes would lead to a difference greater than 4.
;;
;; For the actual calculation, one less the number of 1 runs is used (A)
;; because the "outer" elements of a chain remain fixed:
;;                 diffs:  3  1  1  1  1  3
;;                        / \/ \/ \/ \/ \/ \
;;                 chain: X--a--b--c--d--e--Y
;;                           ^           ^
;;      can't be removed}---´-----------´
;;      ===> {b,c,d} subsets need to be considered.

(defun diffs (adaplist)
  (let ((sorted (sort adaplist #'<)))
    (append (cons (- (car sorted) 0)
                  (loop for (x y) on sorted
                        while y collect (- y x)))
            (list 3))))

(defun rle (list &aux (len (length list)))
  (let (res)
    (if (= len 1)
        (return-from rle (list (cons 1 (car list))))
        (loop with curr = (car list)
              with ctr = 1
              for e in (cdr list)
              if (= curr e) do
                (incf ctr)
              else do
                (push (cons ctr curr) res)
                (setf ctr 1
                      curr e)
              finally
                 (push (cons ctr curr) res)))
    (nreverse res)))


(defun 1-vals (rle)
  (let ((one-counts (remove-if-not (lambda (x) (= x 1)) rle :key #'cdr)))
    (remove-if #'zerop                           ;; remove singleton runs (no contribution)
               (mapcar #'1-                      ;; see (A) above
                       (mapcar #'car             ;; want the number of occurrences
                               one-counts)))))

(defun calc (1-vals)
  (mapcar (lambda (v)
            (+ (if (> v 3) 0 1)         ;; don't count singleton set for runs >= 4 ("n over 0")
               v                        ;; singleton sets                          ("n over 1")
               (floor (* v (1- v)) 2))) ;; two-element subsets                     ("n over 2")
          1-vals))

(defun sol2 (&optional (inpath "./input/day10"))
  (-<> (read-input inpath)
       (diffs <>)
       (rle <>)
       (1-vals <>)
       (calc <>)
       (reduce #'* <>)))
