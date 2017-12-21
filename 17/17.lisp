(in-package #:cl-user)

(defpackage #:aoc-17
  (:use #:cl
        #:alexandria))

(in-package #:aoc-17)

(defstruct node
  (count 0)
  values)

(defun init-tree ()
  (make-node :count 1
             :values (make-array 5
                                 :initial-element 0
                                 :adjustable t
                                 :fill-pointer 1)))

(defun subcopy-extend (vector index value)
  (let ((new (copy-array (subseq vector 0 index)
                         :adjustable t
                         :fill-pointer index)))
    (vector-push-extend value new)
    new))

(defun insert-at (tree index value)
  (unless (<= 0 index (node-count tree))
    (error "index ~s out of bounds for insert into tree ~s" index tree))
  (let ((values (node-values tree)))
    (etypecase values
      (vector
       (if (= index (length values))
           (vector-push-extend value values)
           (setf (node-values tree)
                 (cons (make-node :count (1+ index)
                                  :values (subcopy-extend values index value))
                       (make-node :count (- (length values) index)
                                  :values (copy-array (subseq values index)
                                                      :adjustable t
                                                      :fill-pointer
                                                      (- (length values) index)))))))
      (cons
       (if (> index (node-count (car values)))
           (insert-at (cdr values) (- index (node-count (car values))) value)
           (insert-at (car values) index value)))))
  (incf (node-count tree))
  tree)

(defun lookup (tree index)
  (let ((values (node-values tree)))
    (etypecase values
      (vector (aref values index))
      (cons (if (>= index (node-count (car values)))
                (lookup (cdr values) (- index (node-count (car values))))
                (lookup (car values) index))))))

(defun spinlock (stepsize n)
  (let ((tree (init-tree)))
    (loop :repeat n
          :for i :upfrom 1
          :for position := 1 :then (1+ (mod (+ position stepsize) i))
          :do (insert-at tree position i)
          :finally (return (values tree position)))))

(defun work0 ()
  (multiple-value-bind (tree position) (spinlock 312 2017)
    (lookup tree (mod (1+ position) (node-count tree)))))

(defun spinlock1 (stepsize n)
  (loop :repeat n
        :for i :upfrom 1
        :for position := 1 :then (1+ (mod (+ position stepsize) i))
        :when (= position 1)
          :maximize i))

(defun work1 ()
  (spinlock1 312 50000000))
