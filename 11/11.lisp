(in-package #:cl-user)

(defpackage #:aoc-11
  (:use #:cl
        #:alexandria
        #:split-sequence))

(in-package #:aoc-11)

(defun read-input (filename)
  (with-open-file (in filename)
    (let ((line (read-line in)))
      (split-sequence #\, line))))

(defun mba-step (instruction)
  "Translates an instruction in the form of n, ne, se, s etc. into
Miller-Bravais-analogue hexagonal coordinates."
  (switch (instruction :test #'string=)
    ("n" #(1 1 -2))
    ("ne" #(2 -1 -1))
    ("se" #(1 -2 1))
    ("s" #(-1 -1 2))
    ("sw" #(-2 1 1))
    ("nw" #(-1 2 -1))))

(defun mba-step+ (a b)
  "Add two steps."
  (map 'vector #'+ a b))

(defun hexagonal-manhattan (mba-coords)
  "Hexagonal Manhattan length of the given hexagonal coordinates."
  (let* ((abs-axes (map 'vector #'abs mba-coords))
         (largest (sort abs-axes #'>)))
    (/ (+ (aref largest 0)
          (aref largest 1))
       3)))

(defun work0 ()
  (let* ((instructions (read-input "input.txt"))
         (steps (mapcar #'mba-step instructions))
         (final-position (reduce #'mba-step+ steps)))
    (hexagonal-manhattan final-position)))

(defun work1 ()
  (let* ((instructions (read-input "input.txt"))
         (steps (mapcar #'mba-step instructions)))
    (loop :for step :in steps
          :for position := #(0 0 0) :then (mba-step+ position step)
          :maximize (hexagonal-manhattan position))))
