(in-package #:cl-user)

(defpackage #:aoc-3
  (:use #:cl))

(in-package #:aoc-3)

(defun partway-coordinate (n i l)
  (+ 1
     (floor l 2)
     (- i)
     n))

(defun work0 (n)
  (loop :for leg :in '#1=(:d :r :u :l . #1#)
        :and l := 0 :then (ccase leg
                            ((:r :l) l)
                            ((:u :d) (1+ l)))
        :for i := 1 :then (+ i l)
        :when (<= n i)
          :do (let ((x (case leg
                         (:r (abs (partway-coordinate n i l)))
                         (:u (1+ (floor l 2)))
                         (:l (abs (1- (partway-coordinate n i l))))
                         (:d (floor l 2))))
                    (y (case leg
                         ((:r :l) (floor l 2))
                         (:u (abs (partway-coordinate n i l)))
                         (:d (abs (1- (partway-coordinate n i l))))))) 
                (return (+ x y)))))

(defun sum-surrounding (array coordinates)
  (loop :for y :from (1- (second coordinates)) :to (1+ (second coordinates))
        :sum (loop :for x :from (1- (first coordinates)) :to (1+ (first coordinates))
                   :sum (aref array x y))))

(defun work1 (n)
  (loop :with array := (let ((a (make-array '(50 50)
                                            :initial-element 0)))
                         (setf (aref a 25 25) 1)
                         a)
        :and coordinates := (list 25 25)
        :for leg :in '#1=(:r :u :l :d . #1#)
        :for length := 1 :then (if (member leg '(:u :d))
                                   length
                                   (1+ length))
        :do (loop :repeat length
                  :do (ccase leg
                        (:r (incf (first coordinates)))
                        (:u (decf (second coordinates)))
                        (:l (decf (first coordinates)))
                        (:d (incf (second coordinates))))
                      (let ((x (sum-surrounding array coordinates)))
                        (if (> x n)
                            (return-from work1 (values x coordinates))
                            (setf (apply #'aref array coordinates) x))))))
