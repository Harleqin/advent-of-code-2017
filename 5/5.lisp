(in-package #:cl-user)

(defpackage #:aoc-5
  (:use #:cl))

(in-package #:aoc-5)

(defun work (update-rule)
  (let ((memory
          (coerce (with-open-file (in "input.txt")
                    (loop :for line := (read-line in nil)
                          :while line
                          :collect (parse-integer line
                                                  :junk-allowed t)))
                  'vector)))
    (loop :with i := 0
          :while (array-in-bounds-p memory i)
          :do (incf i (prog1 (aref memory i)
                        (incf (aref memory i)
                              (funcall update-rule (aref memory i)))))
          :counting i)))

(defun work0 ()
  (work (lambda (n)
          (declare (ignore n))
          1)))

(defun work1 ()
  (work (lambda (n)
          (if (> n 2)
              -1
              1))))
