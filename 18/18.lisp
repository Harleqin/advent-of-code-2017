(in-package #:cl-user)

(defpackage #:aoc-18
  (:use #:cl
        #:alexandria
        #:split-sequence))

(in-package #:aoc-18)

(defstruct env
  name
  (registers (make-hash-table))
  (cursor 0)
  (step 1)
  (status :running))

(defstruct (env0 (:include env))
  sound
  recover)

(defun evaluate (env form)
  (etypecase form
    (keyword (gethash form (env-registers env) 0))
    (integer form)))

(defun execute (ops env instruction)
  (apply (gethash (first instruction) ops)
         env
         (rest instruction)))

(defun run (ops env instructions &optional (stopp (constantly nil)))
  (loop :for index := (env-cursor env)
        :count t :into count
        :unless (array-in-bounds-p instructions index)
          :do (setf (env-status env) :terminated)
              (return (values env count))
        :do (setf (env-step env) 1)
            (execute ops env (aref instructions index))
            (incf (env-cursor env)
                  (env-step env))
        :when (funcall stopp env)
          :do (return (values env count))))

(defun -set (env reg val)
  (setf (gethash reg (env-registers env))
        (evaluate env val)))

(defun -add (env reg val)
  (incf (gethash reg (env-registers env) 0)
        (evaluate env val)))

(defun -mul (env reg val)
  (setf (gethash reg (env-registers env))
        (* (gethash reg (env-registers env) 0)
           (evaluate env val))))

(defun -mod (env reg val)
  (setf (gethash reg (env-registers env))
        (mod (gethash reg (env-registers env) 0)
             (evaluate env val))))

(defun -jgz (env val step)
  (when (plusp (evaluate env val))
    (setf (env-step env)
          (evaluate env step))))

(defun -sound (env val)
  (setf (env0-sound env)
        (evaluate env val)))

(defun -recover (env val)
  (unless (zerop (evaluate env val))
    (push (env0-sound env)
          (env0-recover env))))

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

(defun work0 ()
  (let* ((instructions (read-input "input.txt"))
         (ops (plist-hash-table (list :set #'-set
                                      :add #'-add
                                      :mul #'-mul
                                      :mod #'-mod
                                      :jgz #'-jgz
                                      :snd #'-sound
                                      :rcv #'-recover)))
         (env (run ops
                   (make-env0 :name "p")
                   instructions
                   (lambda (env)
                     (env0-recover env)))))
    (last-elt (env0-recover env))))

(defstruct queue
  (queue (make-array 10
                     :adjustable t
                     :fill-pointer 0))
  (read-pointer 0))

(defun enqueue (queue val)
  (vector-push-extend val (queue-queue queue)))

(defun queue-empty-p (queue)
  (= (queue-read-pointer queue)
     (fill-pointer (queue-queue queue))))

(defun dequeue (queue)
  (if (queue-empty-p queue)
      (error "Queue ~s is empty." queue)
      (aref (queue-queue queue)
            (shiftf #1=(queue-read-pointer queue)
                    (1+ #1#)))))

(defun queue-event-count (queue)
  (fill-pointer (queue-queue queue)))

(defstruct (env1 (:include env))
  (inbox (make-queue))
  outbox)

(defun -send (env val)
  (enqueue (env1-outbox env)
           (evaluate env val)))

(defun -receive (env reg)
  (if (queue-empty-p (env1-inbox env))
      (setf (env-status env) :parked
            (env-step env) 0)
      (setf (gethash reg (env-registers env)) (dequeue (env1-inbox env))
            (env-status env) :running)))

(defun parkedp (env)
  (eq (env-status env) :parked))

(defun work1 ()
  (let ((p0 (make-env1 :name "p0"))
        (p1 (make-env1 :name "p1"))
        (ops (plist-hash-table (list :set #'-set
                                     :add #'-add
                                     :mul #'-mul
                                     :mod #'-mod
                                     :jgz #'-jgz
                                     :snd #'-send
                                     :rcv #'-receive)))
        (instructions (read-input "input.txt")))
    (setf (gethash :p (env-registers p1)) 1
          (env1-outbox p0) (env1-inbox p1)
          (env1-outbox p1) (env1-inbox p0))
    (loop :for p :in (circular-list p0 p1)
          :do (run ops
                   p
                   instructions
                   #'parkedp)
          :when (and (parkedp p0)
                     (parkedp p1)
                     (queue-empty-p (env1-inbox p0))
                     (queue-empty-p (env1-inbox p1)))
            :do (setf (env-status p0) :terminated
                      (env-status p1) :terminated)
          :when (and (eq (env-status p0) :terminated)
                     (eq (env-status p1) :terminated))
            :do (return (queue-event-count (env1-inbox p0))))))
