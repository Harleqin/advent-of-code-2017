(in-package #:cl-user)

(defpackage #:aoc-6
  (:use #:cl))

(in-package #:aoc-6)

(defun read-memory ()
  (coerce (with-open-file (in "input.txt")
            (let ((line (read-line in)))
              (loop :for (n pos)
                      := (multiple-value-list (parse-integer line
                                                             :junk-allowed t))
                        :then (multiple-value-list (parse-integer line
                                                                  :junk-allowed t
                                                                  :start pos))
                    :while n
                    :collect n)))
          'vector))

(defun redistribute (memory &aux
                              (length (length memory))
                              (result (copy-seq memory)))
  (destructuring-bind (source-bank n)
      (loop :for i :below length
            :for (best-index best-value)
              := (list i (aref memory i))
                :then (if (> (aref memory i) best-value)
                          (list i (aref memory i))
                          (list best-index best-value))
            :finally (return (list best-index best-value)))
    (setf (aref result source-bank) 0)
    (loop :repeat n
          :for i :upfrom (1+ source-bank)
          :for index := (mod i length)
          :do (incf (aref result index)))
    result))

(defun work (memory)
  (let ((seen (make-hash-table :test #'equalp)))
    (loop :for state := memory :then (redistribute state)
          :for i :upfrom 0
          :for seen-i := (gethash state seen)
          :until seen-i
          :do (setf (gethash state seen) i)
          :finally (return (values i (- i seen-i))))))
