(in-package #:cl-user)

(defpackage #:aoc-25
  (:use #:cl
        #:alexandria
        #:split-sequence))

(in-package #:aoc-25)

(defstruct state
  name
  actions)

(defun parse-move (string)
  (if (string= string "right")
      1
      -1))

(defun parse-state-index (string)
  (- (char-code (char string 0))
     (char-code #\A)))

(defun last-word (string)
  (remove-if (lambda (c)
               (member c (coerce ".:" 'list)))
             (lastcar (split-sequence #\space string))))

(defun read-action (lines)
  (mapcar #'funcall
          (list #'parse-integer #'parse-move #'parse-state-index)
          (mapcar #'last-word lines)))

(defun read-state (lines)
  (make-state :name (last-word (first lines))
              :actions
              (vector (read-action (subseq lines 2 5))
                      (read-action (subseq lines 6 9)))))

(defun read-input (filename)
  (let* ((lines (with-open-file (in filename)
                  (loop :for line := (read-line in nil)
                        :while line
                        :collect line)))
         (begin (parse-state-index (last-word (first lines))))
         (steps (parse-integer (nth 1
                                    (reverse (split-sequence #\space
                                                             (second lines))))))
         (states (map 'vector
                      #'read-state
                      (split-sequence ""
                                      (nthcdr 3 lines)
                                      :test #'equal))))
    (values begin steps states)))

(defun work0 ()
  (multiple-value-bind (begin steps states) (read-input "input.txt")
    (let ((tape (make-array (* 2 steps) ; just throw more memory at it
                            :element-type 'bit
                            :initial-element 0)))
      (loop :repeat steps
            :for position := steps :then (+ position move)
            :for state := (svref states begin) :then (svref states cont)
            :for (out move cont) := (svref (state-actions state)
                                           (bit tape position))
            :do (setf (bit tape position) out))
      (count 1 tape))))
