(in-package #:cl-user)

(defpackage #:aoc-23
  (:use #:cl
        #:alexandria
        #:split-sequence))

(in-package #:aoc-23)

(defstruct env
  name
  (registers (make-hash-table))
  (cursor 0)
  (step 1)
  (status :running))

(defun evaluate (env form)
  (etypecase form
    (keyword (gethash form (env-registers env) 0))
    (integer form)))

(defun execute (ops env instruction)
  (apply (gethash (first instruction) ops)
         env
         (rest instruction)))

(defun run (ops env instructions
            &key
              (stopp (constantly nil))
              (countp (constantly t)))
  (loop :for index := (env-cursor env)
        :count (funcall countp env instructions index) :into count
        :unless (array-in-bounds-p instructions index)
          :do (setf (env-status env) :terminated)
              (return (values env count))
        :do (setf (env-step env) 1)
            (execute ops env (aref instructions index))
            (incf (env-cursor env)
                  (env-step env))
        :when (funcall stopp env)
          :do (return (values env count))))

(defun parse-instruction (string)
  (mapcar (lambda (element)
            (or (parse-integer element :junk-allowed t)
                (intern (string-upcase element) '#:keyword)))
          (split-sequence #\space string)))

(defun read-input (filename)
  (with-open-file (in filename)
    (coerce (loop :for line := (read-line in nil)
                  :while line
                  :collect (parse-instruction line))
            'vector)))

(defun -set (env reg val)
  (setf (gethash reg (env-registers env))
        (evaluate env val)))

(defun -sub (env reg val)
  (decf (gethash reg (env-registers env) 0)
        (evaluate env val)))

(defun -mul (env reg val)
  (setf (gethash reg (env-registers env))
        (* (gethash reg (env-registers env) 0)
           (evaluate env val))))

(defun -jnz (env val step)
  (unless (zerop (evaluate env val))
    (setf (env-step env)
          (evaluate env step))))

(defun work0 ()
  (let ((instructions (read-input "input.txt"))
        (ops (plist-hash-table (list :set #'-set
                                     :sub #'-sub
                                     :mul #'-mul
                                     :jnz #'-jnz))))
    (nth-value 1
               (run ops
                    (make-env :name "w0")
                    instructions
                    :countp (lambda (env instructions index)
                              (declare (ignore env))
                              (and (array-in-bounds-p instructions index)
                                   (eq (first (aref instructions index))
                                       :mul)))))))

(defun work1 ()
  (let ((instructions (read-input "input.txt"))
        (ops (plist-hash-table (list :set #'-set
                                     :sub #'-sub
                                     :mul #'-mul
                                     :jnz #'-jnz)))
        (env (make-env :name "w0")))
    (setf (gethash :a (env-registers env)) 1)
    (let ((env-after (run ops env instructions)))
      (gethash :h (env-registers env-after)))))

;;; OK, so this runs into an almost endless loop.  As the challenge said, we
;;; need to optimize.  I don't know how to do that automatically, so I'll do it
;;; by hand.  First, make the program readable:

#+(or)
(let ((a 1)
      (b 57)
      (c 57)
      (d 0)
      (e 0)
      (f 0)
      (g 0)
      (h 0))
  (unless (zerop a)
    (setf b (* b 100))
    (incf b 100000)
    (setf c (+ b 17000)))
  (loop
    (setf f 1
          d 2)
    (loop 
      (setf e 2)
      (loop
        (setf g (- (* d e) b))
        (when (zerop g)
          (setf f 0))
        (incf e)
        (setf g (- e b))
        (when (zerop g)
          (return)))
      (incf d)
      (setf g (- d b))
      (when (zerop g)
        (return)))
    (when (zerop f)
      (incf h))
    (setf g (- b c))
    (when (zerop g)
      (return))
    (incf b 17)))

;; resolve the initial stuff

#+(or)
(let ((a 1)
      (b 105700)
      (c 122700)
      (d 0)
      (e 0)
      (f 0)
      (g 0)
      (h 0))
  (loop
    (setf f 1
          d 2)
    (loop 
      (setf e 2)
      (loop
        (setf g (- (* d e) b))
        (when (zerop g)
          (setf f 0))
        (incf e)
        (setf g (- e b))
        (when (zerop g)
          (return)))
      (incf d)
      (setf g (- d b))
      (when (zerop g)
        (return)))
    (when (zerop f)
      (incf h))
    (setf g (- b c))
    (when (zerop g)
      (return))
    (incf b 17)))

;; g is always a temporary result for checking equality

#+(or)
(let ((a 1)
      (b 105700)
      (c 122700)
      (d 0)
      (e 0)
      (f 0)
      (h 0))
  (loop
    (setf f 1
          d 2)
    (loop 
      (setf e 2)
      (loop
        (when (= (* d e) b)
          (setf f 0))
        (incf e)
        (when (= e b)
          (return)))
      (incf d)
      (when (= d b)
        (return)))
    (when (zerop f)
      (incf h))
    (when (= b c)
      (return))
    (incf b 17)))

;; e is a loop variable of the innermost loop

#+(or)
(let ((a 1)
      (b 105700)
      (c 122700)
      (d 0)
      (f 0)
      (h 0))
  (loop
    (setf f 1
          d 2)
    (loop 
      (loop :for e :from 2 :to b
            :when (= (* d e) b)
              :do (setf f 0))
      (incf d)
      (when (= d b)
        (return)))
    (when (zerop f)
      (incf h))
    (when (= b c)
      (return))
    (incf b 17)))

;; d is a loop variable of the middle loop

#+(or)
(let ((a 1)
      (b 105700)
      (c 122700)
      (f 0)
      (h 0))
  (loop
    (setf f 1)
    (loop :for d :from 2 :to b
          :do (loop :for e :from 2 :to b
                    :when (= (* d e) b)
                      :do (setf f 0)))
    (when (zerop f)
      (incf h))
    (when (= b c)
      (return))
    (incf b 17)))

;; b is a loop variable of the outer loop, c is constant, a is not needed

#+(or)
(let ((f 0)
      (h 0))
  (loop :for b :from 105700 :to 122700 :by 17
    (setf f 1)
    (loop :for d :from 2 :to b
          :do (loop :for e :from 2 :to b
                    :when (= (* d e) b)
                      :do (setf f 0)))
    (when (zerop f)
      (incf h))))

;; A-ha.  This is looking for primes, in a rather inefficient way.  To be more
;; precise: it counts the number of non-primes between 105700 and 122700
;; (inclusive) in steps of 17.

(defun work-senior ()
  (loop :for b :from 105700 :to 122700 :by 17
        :count (not (primep b))))

(defun primep (n)
  (cond ((< n 2) nil)
        ((= n 2) t)
        ((evenp n) nil)
        (t (loop :for x :from 3 :to (isqrt n) :by 2
                 :never (zerop (mod n x))))))
