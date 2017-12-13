(in-package #:cl-user)

(defpackage #:aoc-12
  (:use #:cl
        #:alexandria
        #:cl-ppcre))

(in-package #:aoc-12)

(defstruct node
  index
  connections
  markp)

(defun parse-node (string)
  (register-groups-bind ((#'parse-integer index)
                         connections)
      ("(\\d+) <-> (.*)" string)
    (make-node :index index
               :connections (mapcar #'parse-integer
                                    (split ",\\s*" connections))
               :markp nil)))

(defun read-graph (filename)
  (with-open-file (in filename)
    (let* ((lines (loop :for line := (read-line in nil)
                        :while line
                        :collect line))
           (graph (make-array (length lines))))
      ;; Defending against holes in the process numbering, so not just (map
      ;; 'vector #'parse-node lines)
      (loop :for line :in lines
            :for node := (parse-node line)
            :do (setf (aref graph (node-index node)) node))
      graph)))

(defun unmarked-successors (nodes graph)
  (let ((successors (remove-duplicates (mapcan #'node-connections nodes))))
    (remove-if #'node-markp
               (mapcar (curry #'aref graph)
                       successors))))

(defun nfind-and-mark-group (node graph)
  (loop :for nodes := (list node)
          :then (unmarked-successors nodes graph)
        :while nodes
        :do (dolist (node nodes)
              (setf (node-markp node) t))
        :append nodes))

(defun work0 ()
  (let ((graph (read-graph "input.txt")))
    (length (nfind-and-mark-group (aref graph 0) graph))))

(defun work1 ()
  (let ((graph (read-graph "input.txt")))
    (loop :for start-node := (find-if-not #'node-markp graph)
          :while start-node
          :do (nfind-and-mark-group start-node graph)
          :count t)))
