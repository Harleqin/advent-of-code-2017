(in-package #:cl-user)

(defpackage #:aoc-9
  (:use #:cl))

(in-package #:aoc-9)

(defun work ()
  (with-open-file (in "input.txt")
    (loop :with level := 0
          :and score := 0
          :and state := (list :group)
          :and count := 0
          :for c := (read-char in nil)
          :while c
          :do (case (first state)
                (:group (ecase c
                          (#\}
                           (incf score level)
                           (decf level)
                           (pop state))
                          (#\{
                           (incf level)
                           (push :group state))
                          ((#\, #\newline) nil)
                          (#\<
                           (push :garbage state))))
                (:garbage (case c
                            (#\>
                             (pop state))
                            (#\!
                             (read-char in nil))
                            (t
                             (incf count)))))
          :finally (return (values score count)))))
