(in-package #:cl-user)

(defpackage #:aoc-22
  (:use #:cl))

(in-package #:aoc-22)

;;; I define "matrix" here as an effectively unbounded two-dimensional array.
;;; Its backing array is always square and of odd length, and is resized as soon
;;; as a point outside is referenced.  The centre of the backing array is at the
;;; coordinates (0 0).

(defstruct matrix
  array)

(defun create-matrix (&key (initial-contents '((nil))))
  (let* ((raw-size (max (length initial-contents)
                        (reduce #'max (mapcar #'length initial-contents))))
         (size (if (oddp raw-size)
                   raw-size
                   (1+ raw-size))))
    (make-matrix :array (make-array (list size size)
                                    :initial-contents initial-contents))))

(defun resize-matrix (matrix)
  "Resizes the matrix' backing square array of odd edge length to one more than
double edge length (e. g. 3 to 7, 7 to 15, 15 to 31)."
  (let* ((old-array (matrix-array matrix))
         (old-size (array-dimension old-array 0))
         (new-size (1+ (* old-size 2)))
         (new-array (make-array (list new-size new-size)
                                :initial-element :clean))
         (start-copy-index (/ (- new-size old-size) 2)))
    (loop :for xn :from start-copy-index :repeat old-size
          :for xo :upfrom 0
          :do (loop :for yn :from start-copy-index :repeat old-size
                    :for yo :upfrom 0
                    :do (setf (aref new-array xn yn)
                              (aref old-array xo yo))))
    (setf (matrix-array matrix) new-array)
    matrix))

(defun mref (matrix i j)
  (let* ((backing-array (matrix-array matrix))
         (x (+ (/ (1- (array-dimension backing-array 0))
                  2)
               i))
         (y (+ (/ (1- (array-dimension backing-array 1))
                  2)
               j)))
    (if (array-in-bounds-p backing-array x y)
        (aref backing-array x y)
        (mref (resize-matrix matrix) i j))))

(defun (setf mref) (value matrix i j)
  (let* ((backing-array (matrix-array matrix))
         (x (+ (/ (1- (array-dimension backing-array 0))
                  2)
               i))
         (y (+ (/ (1- (array-dimension backing-array 1))
                  2)
               j)))
    (if (array-in-bounds-p backing-array x y)
        (setf (aref backing-array x y) value)
        (setf (mref (resize-matrix matrix) i j) value))))

(defun read-input (filename)
  (with-open-file (in filename)
    (create-matrix :initial-contents
                   (loop :for line := (read-line in nil)
                         :while line
                         :collect (map 'list
                                       (lambda (c)
                                         (if (char= c #\#)
                                             :infected
                                             :clean))
                                       line)))))

(defun turn0 (direction status)
  (ecase status
    (:clean (list (- (second direction))
                  (first direction)))
    (:infected (list (second direction)
                     (- (first direction))))))

(defun update0 (status)
  (ecase status
    (:clean :infected)
    (:infected :clean)))

(defun run-infection (n turn-fun update-fun)
  (let ((matrix (read-input "input.txt")))
    (loop :for position := '(0 0) :then (mapcar #'+ position direction)
          :for status := (apply #'mref matrix position)
          :for direction := (funcall turn-fun '(-1 0) status)
            :then (funcall turn-fun direction status)
          :for new-status := (funcall update-fun status)
          :repeat n
          :do (setf (apply #'mref matrix position) new-status)
          :count (eq new-status :infected))))

(defun work0 ()
  (run-infection 10000 #'turn0 #'update0))

(defun turn1 (direction status)
  (ecase status
    (:clean (list (- (second direction))
                  (first direction)))
    (:weakened direction)
    (:infected (list (second direction)
                     (- (first direction))))
    (:flagged (list (- (first direction))
                    (- (second direction))))))

(defun update1 (status)
  (ecase status
    (:clean :weakened)
    (:weakened :infected)
    (:infected :flagged)
    (:flagged :clean)))

(defun work1 ()
  (run-infection 10000000 #'turn1 #'update1))
