(in-package #:cl-user)

(defpackage #:aoc-21
  (:use #:cl
        #:alexandria
        #:cl-arrows
        #:cl-ppcre
        #:split-sequence))

(in-package #:aoc-21)

(defun rotate-array (array &aux
                             (a0 (array-dimension array 0))
                             (a1 (array-dimension array 1)))
  "Rotates the given 2D-array counter-clockwise, where the first array dimension
goes down, the second right."
  (loop :with rotated := (make-array (list a1 a0)
                                     :element-type (array-element-type array))
        :for i :from 0 :below a0
        :do (loop :for j :from 0 :below a1
                  :do (setf (aref rotated (- a1 1 j) i)
                            (aref array i j)))
        :finally (return rotated)))

(defun transpose-array (array &aux
                                (a0 (array-dimension array 0))
                                (a1 (array-dimension array 1)))
  (loop :with transposed := (make-array (list a1 a0)
                                        :element-type (array-element-type array))
        :for i :from 0 :below a0
        :do (loop :for j :from 0 :below a1
                  :do (setf (aref transposed j i)
                            (aref array i j)))
        :finally (return transposed)))

(defun parse-pattern (string)
  (let* ((lines (split-sequence #\/ string))
         (pattern (make-array (list (length lines)
                                    (length (first lines)))
                              :element-type 'bit)))
    (loop :for line :in lines
          :for i :upfrom 0
          :do (loop :for c :across line
                    :for j :upfrom 0
                    :do (setf (aref pattern i j)
                              (ecase c
                                (#\# 1)
                                (#\. 0)))))
    pattern))

(defun rotated-images (pattern)
  (loop :repeat 4
        :for image := pattern :then (rotate-array image)
        :collect image))

(defun images (pattern)
  (let ((transposed (transpose-array pattern)))
    (remove-duplicates (append (rotated-images pattern)
                               (unless (equalp pattern transposed)
                                 (rotated-images transposed)))
                       :test #'equalp)))

(defun parse-rules (string)
  (register-groups-bind ((#'parse-pattern from to))
      ("([#./]+)\\s+=>\\s+([#./]+)" string)
    (mapcar (lambda (image)
              (cons image to))
            (images from))))

(defun read-input (filename)
  (with-open-file (in filename)
    (-> (loop :for line := (read-line in nil)
              :while line
              :nconc (parse-rules line))
        (alist-hash-table :test #'equalp))))

(defun sub-array (array i j a b)
  (let ((sub (make-array (list a b)
                         :element-type (array-element-type array))))
    (loop :for x :from i :repeat a
          :do (loop :for y :from j :repeat b
                    :do (setf (aref sub (- x i) (- y j))
                              (aref array x y))))
    sub))

(defun (setf sub-array) (sub array i j a b)
  (loop :for x :from i :repeat a
        :do (loop :for y :from j :repeat b
                  :do (setf (aref array x y)
                            (aref sub (- x i) (- y j))))))

(defun apply-rule (pattern rules)
  (or (gethash pattern rules)
      (error "No rule found for pattern ~s." pattern)))

(defun enhance (array rules)
  (let* ((size (array-dimension array 0))
         (step (if (evenp size) 2 3))
         (new-step (1+ step))
         (new-size (* size (/ new-step step)))
         (new (make-array (list new-size new-size)
                          :element-type (array-element-type array))))
    (loop :for i :from 0 :below (/ size step)
          :do (loop :for j :from 0 :below (/ size step)
                    :do (setf (sub-array new
                                         (* i new-step)
                                         (* j new-step)
                                         new-step
                                         new-step)
                              (apply-rule (sub-array array
                                                     (* i step)
                                                     (* j step)
                                                     step
                                                     step)
                                          rules))))
    new))

(defun count-on (bit-array)
  (loop :for i :below (array-total-size bit-array)
        :count (= (row-major-aref bit-array i) 1)))

(defun work (n)
  (let ((rules (read-input "input.txt"))
        (initial-pattern (parse-pattern ".#./..#/###")))
    (loop :for pattern := initial-pattern :then (enhance pattern rules)
          :repeat n
          :finally (return (count-on pattern)))))

(defun work0 ()
  (work 5))

(defun work1 ()
  (work 18))
