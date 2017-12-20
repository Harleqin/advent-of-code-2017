(in-package #:cl-user)

(defpackage #:aoc-16
  (:use #:cl
        #:alexandria
        #:cl-arrows
        #:cl-ppcre
        #:split-sequence))

(in-package #:aoc-16)

(defun invert-mapping-vector (v)
  (let ((new-v (make-array (length v))))
    (loop :for p :across v
          :for i :upfrom 0
          :do (setf (svref new-v p) i))
    new-v))

(defstruct dance
  position-pids
  label-pids)

(defun create-dance (n)
  (make-dance :position-pids (coerce (iota n) 'vector)
              :label-pids  (coerce (iota n) 'vector)))

(defun char-label (char-string)
  "Translate the character in a string of length 1 into a label, where #\a is
label 0, #\b is 1 …."
  (- (char-code (char char-string 0))
     (char-code #\a)))

(defun label-char (label)
  "Translate a label into a character, where 0 is #\a, 1 is #\b …."
  (code-char (+ (char-code #\a) label)))

(defun parse-step (string)
  "Parse a single dance step."
  (or (register-groups-bind ((#'parse-integer n))
          ("s(\\d+)" string)
        (list :spin n nil))
      (register-groups-bind ((#'parse-integer a b))
          ("x(\\d+)/(\\d+)" string)
        (list :exchange a b))
      (register-groups-bind ((#'char-label a b))
          ("p(.)/(.)" string)
        (list :partner a b))
      (error "Not a dance step: ~s" string)))

(defun spin-pids (pids n &aux (l (length pids)))
  (loop :with new-pids := (make-array l)
        :for i :from 0 :below l
        :for j := (mod (- i n) l)
        :do (setf (svref new-pids i) (svref pids j))
        :finally (return new-pids)))

(defun swap-pids (pids a b)
  (let ((new-pids (copy-seq pids)))
    (rotatef (svref new-pids a)
             (svref new-pids b))
    new-pids))

(defun dance+step (dance step)
  (destructuring-bind (action a b) step
    (case action
      (:spin
       (make-dance :position-pids (spin-pids (dance-position-pids dance) a)
                   :label-pids (dance-label-pids dance)))
      (:exchange
       (make-dance :position-pids (swap-pids (dance-position-pids dance) a b)
                   :label-pids (dance-label-pids dance)))
      (:partner
       (make-dance :position-pids (dance-position-pids dance)
                   :label-pids (swap-pids (dance-label-pids dance) a b))))))

(defun read-input (filename)
  "Reads dance steps from the first line of the given file."
  (with-open-file (in filename)
    (->> (read-line in)
         (split-sequence #\,)
         (mapcar #'parse-step))))

(defun stage-dance (steps width)
  "Creates a dance from a sequence of steps for a stage of width width."
  (loop :for step :in steps
        :for dance := (dance+step (create-dance width) step)
          :then (dance+step dance step)
        :finally (return dance)))

(defun show-dance (dance)
  "Present a dance as a string."
  (let ((pid-labels (invert-mapping-vector (dance-label-pids dance))))
    (map 'string
         (lambda (pid)
           (label-char (svref pid-labels pid)))
         (dance-position-pids dance))))

(defun work0 ()
  (show-dance (stage-dance (read-input "input.txt") 16)))

(defun pids+ (pids0 pids1 &optional (l (length pids0)))
  (let ((new-pids (make-array l)))
    (loop :for i :from 0 :below l
          :do (setf (svref new-pids i)
                    (svref pids0 (svref pids1 i))))
    new-pids))

(defun dance+ (dance0 dance1 &aux (l (length (dance-position-pids dance0))))
  (make-dance :position-pids (pids+ (dance-position-pids dance0)
                                    (dance-position-pids dance1)
                                    l)
              :label-pids (pids+ (dance-label-pids dance0)
                                 (dance-label-pids dance1)
                                 l)))

(defun dance*scalar (dance n)
  (flet ((simple* (dance n)
           (reduce #'dance+
                   (loop :repeat n
                         :collect dance)
                   :initial-value (create-dance 16))))
    (reduce #'dance+
            (loop :for mag := 1 :then (* 10 mag)
                  :for magdance := dance :then (simple* magdance 10)
                  :for (q r) := (multiple-value-list (floor n 10))
                    :then (multiple-value-list (floor q 10))
                  :while (plusp (+ q r)) ; (or (plusp q) (plusp r))
                  :collect (simple* magdance r))
            :initial-value (create-dance 16))))

(defun work1 ()
  (let ((dance (stage-dance (read-input "input.txt") 16)))
    (show-dance (dance*scalar dance (expt 10 9)))))
