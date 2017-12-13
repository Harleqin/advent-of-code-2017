(in-package #:cl-user)

(defpackage #:aoc-13
  (:use #:cl
        #:cl-ppcre))

(in-package #:aoc-13)

(defun read-input (filename)
  (with-open-file (in filename)
    (loop :for line := (read-line in nil)
          :while (and line (plusp (length line)))
          :collect (mapcar #'parse-integer
                           (split ": " line)))))

(defun scanner-period (range)
  (max 1
       (+ range range -2)))

(defun work0 ()
  (let ((scanners (read-input "input.txt")))
    (loop :for (depth range) :in scanners
          :when (zerop (mod depth (scanner-period range)))
            :sum (* depth range))))

(defun work1 ()
  (let ((scanners (read-input "input.txt")))
    (loop :for delay :upfrom 0
          :until (loop :for (depth range) :in scanners
                       :never (zerop (mod (+ depth delay)
                                          (scanner-period range))))
          :finally (return delay))))
