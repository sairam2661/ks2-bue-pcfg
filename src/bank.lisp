;;;;
;;;; Memory-efficient bank of programs
;;;;
(in-package #:systems.duck.ks2.bue)

(defclass bank ()
  ((programs :initform (make-array 10 :adjustable t :initial-element nil)
             :accessor programs)
   (term-hashes :initform (make-hash-table :test #'equal)
                :accessor term-hashes)))

(defun make-bank (grammar)
  (declare (ignore grammar))
  (make-instance 'bank))

(defun %ensure-bank-ht (bank metric)
  (when (<= (length (programs bank)) metric)
    (setf (programs bank) 
          (adjust-array (programs bank)
                       (max (ceiling (* 1.3 (length (programs bank))))
                       (1+ metric))
          :initial-element nil)))
  (when (null (aref (programs bank) metric))
    (setf (aref (programs bank) metric) (make-hash-table))))

(defun hash-term (term)
  "Create a unique hash for a term based on its structure"
  (with-output-to-string (s)
    (format s "~A:~A" 
           (ast:non-terminal term)
           (g:name (ast:production term)))
    (dolist (child (ast:children term))
      (format s "|~A" (hash-term child)))))

(defun add-to-bank (bank program metric)
  (let ((nt (ast:non-terminal program))
        (term-hash (hash-term program)))
    ;; Only add if we haven't seen this exact structure before
    (unless (gethash term-hash (term-hashes bank))
      (setf (gethash term-hash (term-hashes bank)) t)
      (%ensure-bank-ht bank metric)
      (let ((ht (aref (programs bank) metric)))
        (push program (gethash nt ht))))))

(defun banked-programs (bank nt metric)
  "Get all programs of height <= metric"
  (let ((all-programs nil))
    ;; Ensure we have a hash table for each height
    (loop for h from 0 to metric
          do (%ensure-bank-ht bank h)
          ;; Get programs from hash table at this height
          when (gethash nt (aref (programs bank) h))
          do (setf all-programs
                   (append all-programs
                          (gethash nt (aref (programs bank) h)))))
    all-programs))