(in-package #:cl-user)

(defpackage #:aoc-20
  (:use #:cl
        #:alexandria
        #:cl-arrows
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
  ;; In the long term, acceleration dominates everything; if that is equal,
  ;; initial velocity in the direction of acceleration dominates; if both are
  ;; still equal, initial position in the direction of acceleration decides.  If
  ;; that is still equal, the particles are always the same distance.  We
  ;; disregard this here, as it seems not to matter in our input.
  (or (closer particle0 particle1 #'particle-acc)
      (closer particle0 particle1 #'directed-velocity)
      (closer particle0 particle1 #'directed-position)))

(defun work0 ()
  (reduce #'closer-particle
          (read-input "input.txt")))

(defun integer-sqrt (n)
  (let ((root (isqrt n)))
    (if (= (* root root) n)
        root
        (sqrt n))))

(defun rational-sqrt (n)
  "For rationals of perfect squares, returns the square root as a rational,
for other numbers, returns the square root as a float."
  (if (rationalp n)
      (/ (integer-sqrt (numerator n))
         (integer-sqrt (denominator n)))
      (sqrt n)))

(defun smallest (list)
  (first (sort list #'<)))

(defun 1d-collision-times (dp dv da)
  (if (zerop da)
      (if (zerop dv)
          (zerop dp) ; t means: completely overlaps in this dimension
          (list (- (/ dp dv))))
      (let* ((p (+ 1/2 (/ dv da)))
             (q (/ dp da 1/2))
             (d2 (- (* p p) q))
             (_ (when (minusp d2)
                  (return-from 1d-collision-times nil)))
             (d (rational-sqrt d2)))
        (declare (ignore _))
        (list (- (- p) d)
              (+ (- p) d)))))

(defun collision-time (p0 p1)
  (let* ((dp (map 'vector #'- (particle-pos p0) (particle-pos p1)))
         (dv (map 'vector #'- (particle-vec p0) (particle-vec p1)))
         (da (map 'vector #'- (particle-acc p0) (particle-acc p1)))
         (axis-times (map 'list #'1d-collision-times dp dv da)))
    (->> axis-times
         (remove t)
         (reduce #'intersection)
         (remove-if #'minusp)
         (remove-if-not #'integerp)
         (smallest))))

(defun all-collisions (particles)
  (loop :for (p0 . ps) :on particles
        :nconc (loop :for p1 :in ps
                     :nconc (when-let (collision-time (collision-time p0 p1))
                              (list (list collision-time
                                          (particle-name p0)
                                          (particle-name p1)))))))

(defun group (list key &optional (test #'eql))
  (loop :with groups := (make-hash-table :test test)
        :for element :in list
        :do (push element (gethash (funcall key element) groups ()))
        :finally (return groups)))

(defun work1 ()
  ;; The solution cannot be just to simulate for long enough, because it is
  ;; unclear how long that needs to be.  I therefore calculate all collisions
  ;; and then resolve them in chronological order.
  (let* ((particles (read-input "input.txt"))
         (collisions (group (all-collisions particles) #'first))
         (times (sort (hash-table-keys collisions) #'<)))
    (loop :for destroyed-particles := ()
            :then (union destroyed-particles just-destroyed)
          :for time :in times
          :for current-collisions
            := (remove-if (lambda (collision)
                            (intersection (rest collision)
                                          destroyed-particles))
                          (gethash time collisions))
          :for just-destroyed := (-> (mapcan #'rest current-collisions)
                                     (remove-duplicates))
          :finally (return (- (length particles)
                              (length destroyed-particles))))))

;;; For checking these calculations, I used the following helpers:

(defun calc-pos (particle time)
  (map 'vector
       #'+
       (particle-pos particle)
       (map 'vector (curry #'* time) (particle-vec particle))
       (map 'vector (curry #'* 1/2 time (1+ time)) (particle-acc particle))))

(defun tick-particle (particle)
  (let ((vec (map 'vector
                  #'+
                  (particle-vec particle)
                  (particle-acc particle))))
    (make-particle :name (particle-name particle)
                   :acc (particle-acc particle)
                   :vec vec
                   :pos (map 'vector
                             #'+
                             (particle-pos particle)
                             vec))))

(defun find-collisions (particles)
  (loop :for (p0 . ps) :on particles
        :nconc (loop :for p1 :in ps
                     :when (every #'=
                                  (particle-pos p0)
                                  (particle-pos p1))
                       :collect (list p0 p1))))

(defun simulate (particles)
  (loop :for ps := particles :then (mapcar #'tick-particle ps)
        :for time :upfrom 0
        :do (when-let (collisions (find-collisions ps))
              (return (values collisions time)))))
