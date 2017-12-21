(in-package #:asdf-user)

(defsystem #:advent-of-code-2017
  :depends-on (#:alexandria
               #:cl-arrows
               #:cl-ppcre
               #:split-sequence)
  :components ((:file "1/1")
               (:file "2/2")
               (:file "3/3")
               (:file "4/4")
               (:file "5/5")
               (:file "6/6")
               (:file "7/7")
               (:file "8/8")
               (:file "9/9")
               (:file "10/10")
               (:file "11/11")
               (:file "12/12")
               (:file "13/13")
               (:file "14/14")
               (:file "15/15")
               (:file "16/16")
               (:file "17/17")))
