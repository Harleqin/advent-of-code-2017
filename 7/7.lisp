(in-package #:cl-user)

(defpackage #:aoc-7
  (:use #:cl #:cl-ppcre))

(in-package #:aoc-7)

(defstruct node
  name
  weight
  weight-sum
  children-names)

(defun split-children (string)
  (split "[\\s,]+" string))

(defun parse-node (string)
  (or (register-groups-bind (name
                             (#'parse-integer weight)
                             (#'split-children children))
          ("(\\w+)\\s+\\((\\d+)\\)(?:\\s+->\\s+(.*))?" string)
        (list :name name
              :weight weight
              :children children))
      (error "Line is no valid node: ~s" string)))

(defun parse-graph (filename)
  (let ((graph (make-hash-table :test #'equal)))
    (with-open-file (in filename)
      (loop :for line := (read-line in nil)
            :while line
            :do (let ((node (parse-node line)))
                  (setf (gethash (getf node :name) graph) node)))
      (maphash (lambda (name node)
                 (loop :for child-name :in (getf node :children)
                       :do (setf (getf (gethash child-name graph) :parent)
                                 name)))
               graph)
      graph)))

(defun some-value (hash-table)
  (loop :for node :being :the :hash-values :of hash-table
        :do (return node)))

(defun root-node (graph)
  (loop :for node := (some-value graph)
          :then (gethash parent graph)
        :for parent := (getf node :parent)
        :while parent
        :finally (return node)))

(defun work0 ()
  (root-node (parse-graph "input.txt")))

(defun signal-adjustment (graph childnames weights)
  (let* ((diffs (mapcar #'- weights (rest weights)))
         (i (position-if (complement #'zerop) diffs))
         (node-to-adjust (nth (1+ i) childnames)))
    (signal "Adjust ~s by ~s to ~s.~%"
            node-to-adjust
            (nth i diffs)
            (+ (getf (gethash node-to-adjust graph) :weight)
               (nth i diffs)))))

(defun weight-sum (node graph)
  (let ((children-weights (mapcar (lambda (name)
                                    (weight-sum (gethash name graph) graph))
                                  (getf node :children))))
    (unless (every #'= children-weights (rest children-weights))
      (restart-case
          (signal-adjustment graph
                             (getf node :children)
                             children-weights)
        (continue ())))
    (+ (getf node :weight)
       (reduce #'+ children-weights))))

(defun balance (graph)
  (weight-sum (root-node graph) graph))

(defun work1 ()
  ;; Since we go depth first, the culprit is the first one to be signalled, and
  ;; we can stop.
  (handler-case (balance (parse-graph "input.txt"))
    (condition (adjustment)
      (apply #'format
             t
             (simple-condition-format-control adjustment)
             (simple-condition-format-arguments adjustment)))))
