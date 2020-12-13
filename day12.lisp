(in-package :day12)

(defun parse-line (line)
  (ppcre:register-groups-bind (((lambda (s) (char s 0)) dir)
                               (#'parse-integer length))
      ("(.)(\\d+)" line)
    (list dir length)))

(defun read-input (inpath)
  (with-open-file (s inpath)
    (loop for line = (read-line s nil)
          while line collect (parse-line line))))

(defconstant +north+ #c(0 1))
(defconstant +south+ #c(0 -1))
(defconstant +east+ #c(1 0))
(defconstant +west+ #c(-1 0))

(defstruct ship
  (orientation +east+)
  (pos #c(0 0))
  (waypoint #c(10 1)))

(defmethod %move ((ship ship) (dir (eql #\N)) value)
  (setf (ship-pos ship) (+ (ship-pos ship) (* value +north+))))

(defmethod %move ((ship ship) (dir (eql #\S)) value)
  (setf (ship-pos ship) (+ (ship-pos ship) (* value +south+))))

(defmethod %move ((ship ship) (dir (eql #\E)) value)
  (setf (ship-pos ship) (+ (ship-pos ship) (* value +east+))))

(defmethod %move ((ship ship) (dir (eql #\W)) value)
  (setf (ship-pos ship) (+ (ship-pos ship) (* value +west+))))

(defmethod %move ((ship ship) (dir (eql #\L)) value)
  (declare (ignorable value))
  (let ((num-rot (floor value 90)))
    (setf (ship-orientation ship) (* (ship-orientation ship) (expt #c(0 1) num-rot)))))

(defmethod %move ((ship ship) (dir (eql #\R)) value)
  (declare (ignorable value))
  (let ((num-rot (floor value 90)))
    (setf (ship-orientation ship) (* (ship-orientation ship) (expt #c(0 -1) num-rot)))))

(defmethod %move ((ship ship) (dir (eql #\F)) value)
  (setf (ship-pos ship) (+ (ship-pos ship) (* value (ship-orientation ship)))))

(defun travel (ship route &key (mover #'%move))
  (loop for (d v) in route do
    (funcall mover ship d v))
  ship)

(defun manh (pos)
  (+ (abs (realpart pos))
     (abs (imagpart pos))))

(defun sol1 (&optional (inpath "./input/day12"))
  (let ((ship (make-ship))
        (route (read-input inpath)))
    (manh (ship-pos (travel ship route)))))


(defmethod %move2 ((ship ship) (dir (eql #\N)) value)
  (setf (ship-waypoint ship) (+ (ship-waypoint ship) (* value +north+))))

(defmethod %move2 ((ship ship) (dir (eql #\S)) value)
  (setf (ship-waypoint ship) (+ (ship-waypoint ship) (* value +south+))))

(defmethod %move2 ((ship ship) (dir (eql #\E)) value)
  (setf (ship-waypoint ship) (+ (ship-waypoint ship) (* value +east+))))

(defmethod %move2 ((ship ship) (dir (eql #\W)) value)
  (setf (ship-waypoint ship) (+ (ship-waypoint ship) (* value +west+))))

(defmethod %move2 ((ship ship) (dir (eql #\L)) value)
  (declare (ignorable value))
  (let ((num-rot (floor value 90)))
    (setf (ship-waypoint ship) (* (ship-waypoint ship) (expt #c(0 1) num-rot)))))

(defmethod %move2 ((ship ship) (dir (eql #\R)) value)
  (declare (ignorable value))
  (let ((num-rot (floor value 90)))
    (setf (ship-waypoint ship) (* (ship-waypoint ship) (expt #c(0 -1) num-rot)))))

(defmethod %move2 ((ship ship) (dir (eql #\F)) value)
  (setf (ship-pos ship) (+ (ship-pos ship) (* value (ship-waypoint ship)))))

(defun sol2 (&optional (inpath "./input/day12"))
  (let ((ship (make-ship))
        (route (read-input inpath)))
    (manh (ship-pos (travel ship route :mover #'%move2)))))
