(in-package #:cl-user)

(defpackage #:aoc-24
  (:use #:cl
        #:split-sequence))

(in-package #:aoc-24)

(defstruct comp
  ends
  weight)

(defun create-comp (ends)
  (make-comp :ends ends
             :weight (apply #'+ ends)))

(defun read-input (filename)
  (with-open-file (in filename)
    (loop :for line := (read-line in nil)
          :while line
          :collect (create-comp (mapcar #'parse-integer
                                        (split-sequence #\/ line))))))

(defstruct bridge
  (comps ())
  (weight 0)
  (length 0)
  (end 0)
  leftovers
  (donep nil))

(defun add (bridge comp)
  (make-bridge :comps (cons comp (bridge-comps bridge))
               :weight (+ (bridge-weight bridge)
                          (comp-weight comp))
               :length (1+ (bridge-length bridge))
               :end (first (remove (bridge-end bridge)
                                   (comp-ends comp)
                                   :count 1))
               :leftovers (remove comp (bridge-leftovers bridge))
               :donep nil))

(defun finish-bridge (bridge)
  (let ((finished-bridge (copy-bridge bridge)))
    (setf (bridge-donep finished-bridge) t)
    finished-bridge))

(defun expand-bridge (bridge)
  (if (bridge-donep bridge)
      (list bridge)
      (let ((next (loop :for comp :in (bridge-leftovers bridge)
                        :when (member (bridge-end bridge)
                                      (comp-ends comp))
                          :collect (add bridge comp))))
        (or next
            (list (finish-bridge bridge))))))

(defun split-by-liveness (expanded done)
  (loop :for bridge :in expanded
        :if (bridge-donep bridge)
          :collect bridge :into newly-done
        :else
          :collect bridge :into live
        :finally (return (list live
                               (nconc newly-done done)))))

(defun build-bridges (comps)
  (loop :for (live-bridges done-bridges)
          := (list (list (make-bridge :leftovers comps))
                   ())
            :then (split-by-liveness (mapcan #'expand-bridge live-bridges)
                                     done-bridges)
        :for counter :upfrom 0
        :until (endp live-bridges)
        :finally (return (values done-bridges counter))))

(defun work0 ()
  (let* ((comps (read-input "input.txt"))
         (bridges (build-bridges comps)))
    (reduce #'max (mapcar #'bridge-weight bridges))))

(defun work1 ()
  (let* ((comps (read-input "input.txt"))
         (bridges (build-bridges comps))
         (sorted (sort bridges #'> :key #'bridge-length))
         (longest-length (bridge-length (first sorted)))
         (longest-bridges (loop :for bridge :in sorted
                                :while (= (bridge-length bridge) longest-length)
                                :collect bridge)))
    (first (sort longest-bridges #'> :key #'bridge-weight))))
