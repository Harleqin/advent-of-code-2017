(in-package #:cl-user)

(defpackage #:aoc-20
  (:use #:cl
        #:alexandria
        #:cl-ppcre))

(in-package #:aoc-20)

(defstruct particle
  name
  pos
  vec
  acc)

(defparameter *particle-scanner*
  (let ((vector-notation "<(-?\\d+),(-?\\d+),(-?\\d+)>"))
    (create-scanner (concatenate 'string
                                 "p="
                                 vector-notation
                                 ", v="
                                 vector-notation
                                 ", a="
                                 vector-notation))))

(defun parse-particle (name string)
  (or (register-groups-bind ((#'parse-integer p0 p1 p2
                                              v0 v1 v2
                                              a0 a1 a2))
          (*particle-scanner* string)
        (make-particle :name name
                       :pos (vector p0 p1 p2)
                       :vec (vector v0 v1 v2)
                       :acc (vector a0 a1 a2)))
      (error "Parsing ~s to particle failed." string)))

(defun read-input (filename)
  (with-open-file (in filename)
    (loop :for i :upfrom 0
          :for line := (read-line in nil)
          :while line
          :collect (parse-particle i line))))

(defun manhattan (vector)
  (reduce #'+
          (map 'vector #'abs vector)))

(defun scalar*vector (s v)
  (map 'vector (curry #'* s) v))

(defun scalar-product (&rest vectors)
  (reduce #'+ (apply #'map 'vector #'* vectors)))

(defun vector-component (vector direction-vector)
  (scalar*vector (/ (scalar-product direction-vector vector)
                    (scalar-product direction-vector direction-vector))
                 direction-vector))

(defun closer (p0 p1 key)
  (case (signum (- (manhattan (funcall key p0))
                   (manhattan (funcall key p1))))
    (-1 p0)
    (1 p1)
    (t nil)))

(defun directed-velocity (particle)
  (if (zerop (manhattan (particle-acc particle)))
      (particle-vec particle)
      (vector-component (particle-vec particle)
                        (particle-acc particle))))

(defun directed-position (particle)
  (if (zerop (manhattan (particle-acc particle)))
      (if (zerop (manhattan (particle-vec particle)))
          (particle-pos particle)
          (vector-component (particle-pos particle)
                            (particle-vec particle)))
      (vector-component (particle-pos particle)
                        (particle-acc particle))))

(defun closer-particle (particle0 particle1)
  (or (closer particle0 particle1 #'particle-acc)
      (closer particle0 particle1 #'directed-velocity)
      (closer particle0 particle1 #'directed-position)))

(defun work0 ()
  (reduce #'closer-particle
          (read-input "input.txt")))
