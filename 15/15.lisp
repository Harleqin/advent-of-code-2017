(in-package #:cl-user)

(defpackage #:aoc-15
  (:use #:cl
        #:cl-ppcre))

(in-package #:aoc-15)

(defun read-input (filename)
  "Returns a plist of name to startvalue."
  (with-open-file (in filename)
    (loop :repeat 2
          :append (register-groups-bind (((lambda (s)
                                            (intern s '#:keyword))
                                          generator)
                                         (#'parse-integer start))
                       ("Generator (.*) starts with (\\d+)" (read-line in))
                     (list generator start)))))

(defun simple-generator (factor start)
  (let ((n start))
    (lambda ()
      (shiftf n (mod (* n factor)
                     2147483647)))))

(defun judge (gen-a gen-b count)
  (loop :repeat count
        :for a := (funcall gen-a)
        :and b := (funcall gen-b)
        :count (= (mask-field (byte 16 0) a)
                  (mask-field (byte 16 0) b))))

(defun work0 ()
  (let* ((start-values (read-input "input.txt"))
         (gen-a (simple-generator 16807 (getf start-values :a)))
         (gen-b (simple-generator 48271 (getf start-values :b))))
    (judge gen-a gen-b 40000000)))

(defun picky-generator (factor start zero-bit-count)
  (let ((gen (simple-generator factor start)))
    (lambda ()
      (loop :for r := (funcall gen)
            :when (zerop (mask-field (byte zero-bit-count 0) r))
              :do (return r)))))

(defun work1 ()
  (let* ((start-values (read-input "input.txt"))
         (gen-a (picky-generator 16807 (getf start-values :a) 2))
         (gen-b (picky-generator 48271 (getf start-values :b) 3)))
    (judge gen-a gen-b 5000000)))
