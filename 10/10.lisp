(in-package #:cl-user)

(defpackage #:aoc-10
  (:use #:cl
        #:alexandria
        #:split-sequence)
  (:export #:knot-hash))

(in-package #:aoc-10)

(defun read-input-0 (filename)
  (with-open-file (in filename)
    (mapcar #'parse-integer
            (split-sequence #\, (read-line in)))))

(defun knot-hash-round (circle knots
                        &optional (pos 0) (skip-in 0)
                        &aux (length (length circle)))
  (loop :for knot :in knots
        :and skip :upfrom skip-in
        :do (loop :for i := pos
                    :then (mod (1+ i) length)
                  :for j := (mod (+ pos knot -1) length)
                    :then (mod (1- j) length)
                  :repeat (floor knot 2)
                  :do (rotatef (aref circle i)
                               (aref circle j)))
            (setf pos (mod (+ pos knot skip) length))
        :finally (return (values circle pos (1+ skip)))))

(defun work0 ()
  (let* ((circle0 (coerce (iota 256) 'vector))
         (circle1 (knot-hash-round circle0 (read-input-0 "input.txt"))))
    (* (aref circle1 0)
       (aref circle1 1))))

(defun read-input-1 (filename)
  (with-open-file (in filename)
    (map 'list #'char-code (read-line in))))

(defun knot-hash (input)
  (let* ((knots (append input (list 17 31 73 47 23)))
         (circle0 (coerce (iota 256) 'vector))
         (sparse-hash
           (loop :repeat 64
                 :for (circle pos skip)
                   := (multiple-value-list (knot-hash-round circle0
                                                            knots))
                     :then (multiple-value-list (knot-hash-round circle
                                                                 knots
                                                                 pos
                                                                 skip))
                 :finally (return circle)))
         (dense-hash
           (loop :for i :from 0 :below 255 :by 16
                 :collect (reduce #'logxor
                                  (subseq sparse-hash i (+ 16 i))))))
    (string-downcase (format nil "~{~2,'0x~}" dense-hash))))

(defun work1 ()
  (knot-hash (read-input-1 "input.txt")))
