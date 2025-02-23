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

(defun generate-metric-list/rec (metric arity)
  "Generate all possible height combinations up to metric"
  (if (zerop arity)
      (list nil)
      (let ((result nil))
        (loop for item in (generate-metric-list/rec metric (1- arity))
              do (loop for i from 0 to metric  ; Changed from (1- metric) to metric
                      do (push (cons i item) result)))
        result)))

(defun height-filter (metric item)
  "Modified height filter to allow building larger terms"
  (and (every (lambda (x) (<= x metric)) item)  ; All heights must be <= metric
       (some (lambda (x) (= x (1- metric))) item)))  ; At least one must be metric-1

(defun size-filter (metric item)
  (= (apply #'+ item) (1- metric)))

(defun generate-metric-list (metric arity type)
  "Generates a metric list for a production of arity ARITY"
  (if (zerop metric)
      ;; For depth 0, allow using terms from previous depths
      (list (make-list arity :initial-element 0))
      ;; For other depths, allow any combination that sums to current depth
      (remove-if-not (lambda (heights)
                       ;; Allow using any heights up to current depth
                       (every (lambda (h) (<= h metric)) heights))
                     (generate-metric-list/rec metric arity))))

(defun estimate-combination-probability (prod child-probs)
  "Estimate the probability of a term using production and child probabilities"
  (* (first (g:probability prod))
     (reduce #'* child-probs)))

;; Helper function to sort combinations by their resulting probability
(defun sort-combinations-by-probability (combinations prod-prob)
  (cl:sort (copy-list combinations) #'>
        :key (lambda (combo)
               (* prod-prob (reduce #'* (mapcar #'ast:probability combo))))))

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

(defun collect-terms-for-depth (bank grammar depth metric-type)
  "Collect all possible terms for a depth and sort by probability"
  (let ((terms nil)
        (candidates (co-new-terms bank grammar depth metric-type)))
    (loop while (co-has-more-terms? candidates)
          for term = (co-next-term candidates)
          for prob = (ast:probability term)
          when (plusp prob)
          do (push (cons prob term) terms))
    ;; Sort by probability, highest first
    (cl:sort terms #'> :key #'car)))

(defun collect-expanded-terms (bank grammar prod depth metric-type current-term)
  "Collect all terms that can be generated from expanding current-term with prod"
  (let ((terms nil))
    (loop for heights in (generate-metric-list depth (g:arity prod) metric-type)
          for child-opts = (map 'list (a:curry #'banked-programs bank)
                               (g:occurrences prod)
                               heights)
          when (every #'identity child-opts)
          do
             (loop for combo in (all-cart-prod child-opts)
                   for new-term = (make-instance 'ast:program-node
                                               :production prod
                                               :children combo
                                               :probability (* (first (g:probability prod))
                                                            (reduce #'* (mapcar #'ast:probability combo))))
                   for new-prob = (ast:probability new-term)
                   when (plusp new-prob)
                   do (push (cons new-prob new-term) terms)))
    ;; Sort by probability, highest first
    (cl:sort terms #'> :key #'car)))

(co:defcoroutine coroutine-new-terms (arg)
  (destructuring-bind (bank grammar metric type) arg
    (let ((sorted-prods (cl:sort (copy-list (g:productions grammar)) #'>
                             :key (lambda (p) (first (g:probability p)))))
          (current-depth metric))
      ;; For depth 0, ONLY yield atomic terms
      (if (zerop current-depth)
          ;; Only zero-arity productions for depth 0
          (loop for prod in sorted-prods
                when (and (zerop (g:arity prod))
                         (plusp (first (g:probability prod))))
                do (co:yield (make-instance 'ast:program-node
                                          :production prod
                                          :probability (first (g:probability prod)))))
          ;; For depth > 0, combine terms from current and previous depths
          (loop for prod in sorted-prods
                when (and (plusp (g:arity prod))
                         (plusp (first (g:probability prod))))
                do 
                (let ((heights (generate-metric-list current-depth (g:arity prod) type)))
                  (loop for height-combo in heights
                        do
                        (let ((child-opts (map 'list 
                                             (lambda (nt h)
                                               (let ((terms (banked-programs bank nt h)))
       
                                                 (when terms
                                                   (cl:sort (copy-list terms) #'> 
                                                         :key #'ast:probability))))
                                             (g:occurrences prod)
                                             height-combo)))
                          ;; Debug child options
                          ;; Only proceed if we have all needed children
                          (when (every #'identity child-opts)
                            (let ((combinations (all-cart-prod child-opts)))
                              (loop for combo in combinations
                                    for probability = (* (first (g:probability prod))
                                                       (reduce #'* (mapcar #'ast:probability combo)))
                                    when (plusp probability)
                                    do (co:yield (make-instance 'ast:program-node
                                                              :production prod
                                                              :children combo
                                                              :probability probability)))))))))))))


(defun enumerate (semgus-problem metric-type &key (beam-width 400) (max-depth 1000))
  "Beam search enumeration with simple scaled pruning"
  (let* ((grammar (semgus:grammar semgus-problem))
         (initial-nt (g:initial-non-terminal grammar))
         (bank (make-bank grammar))
         (current-beam nil)
         (best-prob 0.0)
         (min-prob-threshold 1e-12))
    
    ;; Initialize with atomic terms
    (let ((candidates (co-new-terms bank grammar 0 metric-type)))
      (loop while (co-has-more-terms? candidates)
            for term = (co-next-term candidates)
            for prob = (ast:probability term)
            when (and (plusp prob)
                     (zerop (g:arity (ast:production term))))
            do (progn 
                 (setf best-prob (max best-prob prob))
                 (add-to-bank bank term 0)
                 (setf current-beam 
                       (update-top-k current-beam
                                    (make-beam-state :term term
                                                   :log-probability (log prob)
                                                   :depth 0)
                                    beam-width)))))
    
    (loop for depth from 1 below max-depth
          while current-beam
          do
          (let ((next-beam nil)
                (terms-at-depth 0)
                (terms-pruned 0)
                ;; Simple scaling factor based on depth
                (threshold-scale (expt 0.05 depth)))
            
            (let ((candidates (co-new-terms bank grammar depth metric-type)))
              (loop while (co-has-more-terms? candidates)
                    for term = (co-next-term candidates)
                    for prob = (ast:probability term)
                    when (plusp prob) do
                      (incf terms-at-depth)
                      
                      ;; Only update best-prob with terms we'll actually keep
                      (when (> prob (* best-prob threshold-scale))
                        (setf best-prob (max best-prob prob)))
                      
                      (if (and (> prob (* best-prob threshold-scale))
                             (not (term-exists-p bank term depth)))
                          (progn
                            (when (eql initial-nt (ast:non-terminal term))
                              (when (semgus:check-program semgus-problem term)
                                (format t "~&Found solution at depth ~A with prob ~A~%" 
                                        depth prob)
                                (return-from enumerate term)))
                            
                            (add-to-bank bank term depth)
                            (setf next-beam
                                  (update-top-k next-beam
                                              (make-beam-state :term term
                                                             :log-probability (log prob)
                                                             :depth depth)
                                              beam-width)))
                          (incf terms-pruned))))
            
            ;; Debug output
            (when (> terms-at-depth 0)
              (format t "~&Depth ~A: Total terms=~A, Pruned=~A (~,2F%), Best prob=~,8F, Scale=~,8F~%" 
                      depth 
                      terms-at-depth 
                      terms-pruned
                      (* 100.0 (/ terms-pruned terms-at-depth))
                      best-prob
                      threshold-scale))
            
            (setf current-beam next-beam)))
    nil))

(defun update-top-k (solutions new-solution k)
  "Update the list of top-k solutions, maintaining only the k highest probability solutions"
  (let ((new-solutions (cl:sort (cons new-solution solutions) 
                            #'> 
                            :key #'beam-state-log-probability)))
    (subseq new-solutions 0 (min k (length new-solutions)))))
