(in-package #:cl-user)

(defpackage #:aoc-14
  (:use #:cl
        #:alexandria
        #:aoc-10
        #:cl-arrows))

(in-package #:aoc-14)

(defparameter *input* "hxtvlmkl")

(defun row-bitvectors (input)
  (loop :for row :from 0 :below 128
        :for row-key := (format nil "~a-~a" input row)
        :for hash := (knot-hash (map 'list #'char-code row-key))
        :collect (parse-integer hash :radix 16)))

(defun work0 ()
  (->> (row-bitvectors *input*)
       (mapcar #'logcount)
       (reduce #'+)))

(defun neighbours (row-major-coord)
  (remove nil
          (list (when (> row-major-coord 127)
                  (- row-major-coord 128))
                (when (< row-major-coord (* 128 127))
                  (+ row-major-coord 128))
                (when (plusp (mod row-major-coord 128))
                  (1- row-major-coord))
                (when (/= (mod row-major-coord 128) 127)
                  (1+ row-major-coord)))))

(defun unmarked-neighbours (grid coord)
  (remove-if-not (lambda (neighbour)
                   (eql :unmarked (row-major-aref grid neighbour)))
                 (neighbours coord)))

(defun make-grid (row-bitvectors)
  (let ((grid (make-array (list 128 128)
                          :initial-element :empty)))
    (loop :for bv :in row-bitvectors
          :for i :upfrom 0
          :do (loop :for j :from 0 :below 128
                    :when (logbitp j bv)
                      :do (setf (aref grid i j) :unmarked)))
    grid))

(defun advance (row-major-coord)
  (unless (>= row-major-coord (* 128 128))
    (1+ row-major-coord)))

(defun find-unmarked (grid &optional (start-coord 0))
  (loop :for i :upfrom start-coord :below (array-total-size grid)
        :thereis (when (eql :unmarked (row-major-aref grid i))
                   i)))

(defun n-mark-group (grid start-coord)
  (do ((current-heads (list start-coord)
                      (mapcan (curry #'unmarked-neighbours grid)
                              current-heads)))
      ((endp current-heads))
    (dolist (c current-heads)
      (setf (row-major-aref grid c) :marked))))

(defun work1 ()
  (let ((grid (make-grid (row-bitvectors *input*))))
    (loop :for start-coord := (find-unmarked grid)
            :then (find-unmarked grid (advance start-coord))
          :while start-coord
          :do (n-mark-group grid start-coord)
          :count t)))
