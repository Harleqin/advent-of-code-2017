(in-package #:cl-user)

(defpackage #:aoc-8
  (:use #:cl
        #:alexandria
        #:cl-ppcre))

(in-package #:aoc-8)

(defstruct instruction
  register
  operation
  argument
  cond-register
  cond-comparator
  cond-argument)

(defun parse-operation (string)
  (eswitch (string :test #'string=)
    ("inc" #'+)
    ("dec" #'-)))

(defun parse-comparator (string)
  (eswitch (string :test #'string=)
    (">" #'>)
    ("<" #'<)
    (">=" #'>=)
    ("<=" #'<=)
    ("==" #'=)
    ("!=" #'/=)))

(defun parse-instruction (string)
  (or (register-groups-bind (register
                             (#'parse-operation operation)
                             (#'parse-integer argument)
                             cond-register
                             (#'parse-comparator cond-comparator)
                             (#'parse-integer cond-argument))
          ("(\\w+)\\s+(\\w+)\\s+(-?\\d+)\\s+if\\s+(\\w+)\\s+(\\S+)\\s+(-?\\d+)"
           string)
        (make-instruction :register register
                          :operation operation
                          :argument argument
                          :cond-register cond-register
                          :cond-comparator cond-comparator
                          :cond-argument cond-argument))
      (error "Instruction not recognized: ~s." string)))

(defun work ()
  (let ((instructions (with-open-file (in "input.txt")
                        (loop :for line := (read-line in nil)
                              :while line
                              :collect (parse-instruction line)))))
    (loop :with registers := (make-hash-table :test #'equal)
          :and max := 0
          :for instruction :in instructions
          :do
             (when (funcall (instruction-cond-comparator instruction)
                            (gethash (instruction-cond-register instruction)
                                     registers
                                     0)
                            (instruction-cond-argument instruction))
               (setf max
                     (max max
                          (incf (gethash (instruction-register instruction)
                                         registers
                                         0)
                                (funcall (instruction-operation instruction)
                                         (instruction-argument instruction))))))
          :finally
             (return (values (loop :for v :being :the :hash-values :of registers
                                   :maximize v)
                             max)))))
