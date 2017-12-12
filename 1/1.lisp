(in-package #:cl-user)

(defpackage #:aoc-1
  (:use #:cl))

(in-package #:aoc-1)

(defun maybe-read-digit (stream)
  (let ((c (read-char stream nil)))
    (when c
      (digit-char-p c))))

(defun read-digits (filename)
  (with-open-file (in filename)
    (loop :for d := (maybe-read-digit in)
          :while d
          :collect d)))

(defun work1 ()
  (let* ((l (read-digits "input")))
    (loop :for (a b) :on (append (last l) l)
          :while b
          :when (= a b)
            :sum a)))

(defun rotate-list (list &aux (length (length list)))
  (append (last list (floor length 2))
          (butlast list (ceiling length 2))))

(defun work2 ()
  (let ((l (read-digits "input")))
    (loop :for a :in l
          :for b :in (rotate-list l)
          :when (= a b)
            :sum a)))
