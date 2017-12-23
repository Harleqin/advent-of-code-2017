(in-package #:cl-user)

(defpackage #:aoc-19
  (:use #:cl
        #:alexandria
        #:cl-arrows))

(in-package #:aoc-19)

(defun read-input (filename)
  (with-open-file (in filename)
    (let ((lines (with-open-file (in filename)
                   (loop :for line := (read-line in nil)
                         :while line
                         :collect (coerce line 'list)))))
      (make-array (list (length lines)
                        (length (first lines)))
                  :initial-contents lines))))

(defvar *moves* ())

(defmacro defmove (name add)
  (with-gensyms (pos)
    `(progn (defun ,name (,pos)
              (mapcar #'+ ,pos ',add))
            (pushnew ',name *moves*))))

(defmove up (-1 0))
(defmove right (0 1))
(defmove down (1 0))
(defmove left (0 -1))

(defun invert-move (move)
  (case move
    (up 'down)
    (right 'left)
    (down 'up)
    (left 'right)))

(defun find-char-pos-if (char-pred field start-pos direction)
  (do* ((p start-pos
           (funcall direction p))
        (c #1=(apply #'aref field p) #1#))
       ((funcall char-pred c)
        (values p c))))

(defun find-char-pos (char field start-pos direction)
  (find-char-pos-if (curry #'char= char)
                    field
                    start-pos
                    direction))

(defun start-pos (field)
  (find-char-pos #\| field '(0 0) #'right))

(defun find-direction (field &key from at)
  (find-if (lambda (direction)
             (char/= #\space
                     (apply #'aref
                            field
                            (funcall direction at))))
           (remove (invert-move from) *moves*)))

(defun pos-distance (new old)
  (->> (mapcar #'- new old)
       (mapcar #'abs)
       (reduce #'+)))

(defun walk-the-line (field)
  (loop :with direction := 'down
        :for pos := (start-pos field) :then (funcall direction new-pos)
        :for (new-pos char) := (multiple-value-list
                                (find-char-pos-if (lambda (c)
                                                    (or (alpha-char-p c)
                                                        (member c
                                                                '(#\space #\+)
                                                                :test #'char=)))
                                                  field
                                                  pos
                                                  direction))
        :sum (1+ (pos-distance new-pos pos)) :into distance
        :when (alpha-char-p char)
          :collect char :into chars
        :when (char= char #\+)
          :do (setf direction
                    (find-direction field
                                    :from direction
                                    :at new-pos))
        :when (or (null direction)
                  (char= char #\space))
          :do (return (values (coerce chars 'string)
                              new-pos
                              (1- distance)))))

(defun work0 ()
  (nth-value 0 (walk-the-line (read-input "input.txt"))))

(defun work1 ()
  (nth-value 2 (walk-the-line (read-input "input.txt"))))
