(in-package #:cl-user)

(defpackage #:aoc-4
  (:use #:cl
        #:split-sequence))

(in-package #:aoc-4)

(defun phrase-valid-p (phrase)
  (let ((words  (split-sequence #\space phrase)))
    (= (length words)
       (length (remove-duplicates words :test #'string=)))))

(defun phrase-anagram-valid-p (phrase)
  (let ((words (mapcar (lambda (word)
                         (sort word #'> :key #'char-code))
                       (split-sequence #\space phrase))))
    (= (length words)
       (length (remove-duplicates words :test #'string=)))))

(defun work (validation-fun)
  (with-open-file (in "input.txt")
    (loop :for phrase := (read-line in nil)
          :while phrase
          :count (funcall validation-fun phrase))))

(defun work0 ()
  (work #'phrase-valid-p))

(defun work1 ()
  (work #'phrase-anagram-valid-p))
