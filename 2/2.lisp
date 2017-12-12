(in-package #:cl-user)

(defpackage #:aoc-2
  (:use #:cl))

(in-package #:aoc-2)

(defun parse-line (line)
  (loop :for (n pos)
          := (multiple-value-list (parse-integer line :junk-allowed t))
          :then (multiple-value-list (parse-integer line
                                                    :start pos
                                                    :junk-allowed t))
        :while n
        :collect n))

(defun read-sheet (filename)
  (with-open-file (in filename)
    (loop :for line := (read-line in nil)
          :while line
          :collect (parse-line line))))

(defun max-min (line)
  (loop :for n :in line
        :maximize n :into max
        :minimize n :into min
        :finally (return (- max min))))

(defun work1 ()
  (let ((sheet (read-sheet "sheet")))
    (loop :for line :in sheet
          :sum (max-min line))))

(defun evenly-divisible-p (a b)
  (multiple-value-bind (d r) (floor a b)
    (when (zerop r) d)))

(defun even-division (line)
  (loop :for (n . r) :on line
        :thereis (loop :for m :in r
                       :thereis (or (evenly-divisible-p n m)
                                    (evenly-divisible-p m n)))))

(defun work2 ()
  (let ((sheet (read-sheet "sheet")))
    (reduce #'+ (mapcar #'even-division sheet))))
