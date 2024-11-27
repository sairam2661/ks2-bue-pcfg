;;;;
;;;; Program enumerator
;;;;
(in-package #:systems.duck.ks2.bue)

(defstruct priority-queue
  elements) ; A list of (priority . term) pairs


(defun make-priority-queue ()
  "Create an empty priority queue."
  (make-priority-queue :elements nil))

(defun push-to-priority-queue (queue term priority)
  "Insert a term into the priority queue with a given priority."
  (let ((elements (priority-queue-elements queue)))
    (setf (priority-queue-elements queue)
          (loop for item in elements
                while (<= (car item) priority)
                collect item
                into new-elements
                finally (return (append new-elements
                                         (list (cons priority term))
                                         (rest (member item elements))))))))

(defun pop-from-priority-queue (queue)
  "Remove and return the element with the highest priority (lowest value)."
  (let ((elements (priority-queue-elements queue)))
    (if (null elements)
        (error "Priority queue is empty.")
        (prog1 (cdar elements) ; Return the `term` part of the first element
          (setf (priority-queue-elements queue) (cdr elements))))))

(defun empty-p (queue)
  "Check if the priority queue is empty."
  (null (priority-queue-elements queue)))

(defun dump-contents (bank &optional metric)
  (format t "Dumping bank contents:~%")
  (if metric
      ;; Dump only the specific metric
      (let ((contents (aref (programs bank) metric)))
        (if contents
            (progn
              (format t "METRIC ~a:~%" metric)
              (maphash
               (lambda (nt programs)
                 (format t "  Non-Terminal: ~a~%" nt)
                 (dolist (program programs)
                   (format t "    Program: ~a~%" program)))
               contents))
            (format t "  METRIC ~a is empty.~%" metric)))
      ;; Dump all metrics
      (loop for i below (length (programs bank))
            do (let ((contents (aref (programs bank) i)))
                 (when contents
                   (format t "METRIC ~a:~%" i)
                   (maphash
                    (lambda (nt programs)
                      (format t "  Non-Terminal: ~a~%" nt)
                      (dolist (program programs)
                        (format t "    Program: ~a~%" program)))
                    contents))))))


(defun generate-metric-list/rec (metric arity)
  (if (zerop arity)
      (list nil)
      (let ((result nil))
        (loop for item in (generate-metric-list/rec metric (1- arity))
              do (loop for i from 0 to (1- metric)
                       do (push (cons i item) result)))
        result)))

(defun height-filter (metric item)
  (some #'(lambda (x) (= x (1- metric))) item))

(defun size-filter (metric item)
  (= (apply #'+ item) (1- metric)))

(defun generate-metric-list (metric arity type)
  "Generates a metric list for a production of arity ARITY"
  (remove-if-not (a:curry (case type
                            (:height #'height-filter)
                            (:size #'size-filter))
                          metric)
                 (generate-metric-list/rec metric arity)))

(co:defcoroutine coroutine-new-terms (arg)
  (destructuring-bind (bank grammar metric type) arg
    (loop for prod in (g:productions grammar)
          if (and (zerop metric) (zerop (g:arity prod))) do
              (let ((probability (first (g:probability prod))))
                (co:yield (make-instance 'ast:program-node
                                         :production prod
                                         :probability probability)))
          else do
            (loop for heights in (generate-metric-list metric (g:arity prod) type)
                  for child-opts = (map 'list (a:curry #'banked-programs bank)
                                        (g:occurrences prod)
                                        heights)
                  do
                     (loop with combos = (all-cart-prod child-opts)
                           for combo in combos
                           for probability = (* (first (g:probability prod))
                                                (reduce #'* (mapcar #'ast:probability combo)))
                           do (co:yield (make-instance 'ast:program-node
                                                       :production prod
                                                       :children combo
                                                       :probability probability)))))))

(defun has-more-terms? (candidates)
  (not (null (car candidates))))

(defun next-term (candidates)
  (prog1
      (first (car candidates))
    (setf (car candidates) (rest (car candidates)))))

(defun co-new-terms (bank grammar metric type)
  (list
   (co:make-coroutine 'coroutine-new-terms)
   nil
   (list bank grammar metric type)))

(defun co-has-more-terms? (candidates)
  (let ((res (funcall (first candidates) (third candidates))))
    (setf (second candidates) res)))

(defun co-next-term (candidates)
  (second candidates))

  (defstruct beam-state
  term        ; Current terms in the beam
  log-probability ; Log probability of the state
  depth)          ; Current depth in the search

(defun term-equal (term1 term2)
  "Compare two terms for structural equality"
  (and (eq (ast:non-terminal term1) (ast:non-terminal term2))
       (eq (ast:production term1) (ast:production term2))
       (every #'term-equal 
              (ast:children term1)
              (ast:children term2))))

(defun term-exists-p (bank term depth)
  "Check if a structurally equivalent term already exists in the bank"
  (let ((existing-terms (banked-programs bank (ast:non-terminal term) depth)))
    (some (lambda (existing) 
            (term-equal term existing))
          existing-terms)))
(defun log-probability (term)
  "Convert probability to log space for numerical stability, handling zero probabilities"
  (let ((prob (ast:probability term)))
    (if (zerop prob)
        most-negative-double-float  ; Effectively -infinity for zero probabilities
        (log prob))))

(defun has-nonzero-probability (prod)
  "Check if a production has a non-zero probability"
  (plusp (first (g:probability prod))))

(defun enumerate (semgus-problem metric-type &key (beam-width 100) (max-depth 100))
  "Runs beam search based enumeration with probabilistic guidance"
  (let* ((grammar (semgus:grammar semgus-problem))
         (initial-nt (g:initial-non-terminal grammar))
         (bank (make-bank grammar))
         (beam (make-instance 'priority-queue)))
    
    ;; Generate initial terms for depth 0
    (let ((candidates (co-new-terms bank grammar 0 metric-type)))
      (loop while (co-has-more-terms? candidates)
            for term = (co-next-term candidates)
            for prob = (ast:probability term)
            when (and (plusp prob)
                     (not (term-exists-p bank term 0)))
            do (progn 
                 (add-to-bank bank term 0)
                 (push-to-priority-queue beam 
                                       (make-beam-state :term term
                                                      :log-probability (log prob)
                                                      :depth 0)
                                       (- (log prob))))))
    
    (loop for depth from 0 below max-depth do
      (let ((next-beam (make-instance 'priority-queue))
            (candidates-seen 0))
        
        ;; Generate new terms for current depth
        (let ((candidates (co-new-terms bank grammar depth metric-type)))
          (loop while (co-has-more-terms? candidates)
                for term = (co-next-term candidates)
                for prob = (ast:probability term)
                when (and (plusp prob)
                         (not (term-exists-p bank term depth)))
                do
                (incf candidates-seen)
                
                ;; Check if term solves the problem
                (when (eql initial-nt (ast:non-terminal term))
                  (incf ast:*candidate-concrete-programs*)
                  (when (= 1 (incf (getf ast:*concrete-candidates-by-size* depth 0)))
                    (ast:add-checkpoint depth))
                  (ast:trace-program term)
                  (when (semgus:check-program semgus-problem term)
                    (return-from enumerate term)))
                
                ;; Add unique term to bank and beam
                (add-to-bank bank term depth)
                (push-to-priority-queue next-beam
                                      (make-beam-state :term term
                                                     :log-probability (log prob)
                                                     :depth depth)
                                      (- (log prob)))))
        
        ;; Generate terms using existing terms in the bank
        (loop while (and (not (empty-p beam))
                        (< candidates-seen beam-width))
              for state = (pop-from-priority-queue beam)
              for current-term = (beam-state-term state)
              for current-depth = (beam-state-depth state)
              do
              ;; Try to combine with terms from the bank to create larger terms
              (loop for prod in (g:productions grammar)
                    when (and (> (g:arity prod) 0)
                            (has-nonzero-probability prod))  ; Skip zero-probability productions
                    do
                    (loop for heights in (generate-metric-list depth (g:arity prod) metric-type)
                          for child-opts = (map 'list (a:curry #'banked-programs bank)
                                              (g:occurrences prod)
                                              heights)
                          when (every #'identity child-opts)  ; ensure all child options exist
                          do
                          (loop for combo in (all-cart-prod child-opts)
                                for new-term = (make-instance 'ast:program-node
                                                           :production prod
                                                           :children combo
                                                           :probability (* (first (g:probability prod))
                                                                        (reduce #'* (mapcar #'ast:probability combo))))
                                for prob = (ast:probability new-term)
                                when (and (plusp prob)
                                         (not (term-exists-p bank new-term depth)))
                                do
                                (incf candidates-seen)
                                (when (eql initial-nt (ast:non-terminal new-term))
                                  (incf ast:*candidate-concrete-programs*)
                                  (when (= 1 (incf (getf ast:*concrete-candidates-by-size* depth 0)))
                                    (ast:add-checkpoint depth))
                                  (ast:trace-program new-term)
                                  (when (semgus:check-program semgus-problem new-term)
                                    (return-from enumerate new-term)))
                                
                                (add-to-bank bank new-term depth)
                                (push-to-priority-queue next-beam
                                                      (make-beam-state :term new-term
                                                                     :log-probability (log prob)
                                                                     :depth depth)
                                                      (- (log prob)))))))
        
        ;; Update beam for next iteration
        (setf beam next-beam)
        
        ;; Debug output
        (format t "Depth ~A: Processed ~A candidates~%" depth candidates-seen)
        (dump-contents bank depth)))
    
    ;; No solution found
    (format t "No solution found within depth limit ~A~%" max-depth)
    nil))
;; Helper function for maintaining top-k solutions
(defun update-top-k (solutions new-solution k)
  "Update the list of top-k solutions, maintaining only the k highest probability solutions"
  (let ((new-solutions (sort (cons new-solution solutions) 
                            #'> 
                            :key #'beam-state-log-probability)))
    (subseq new-solutions 0 (min k (length new-solutions)))))
