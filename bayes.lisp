;; Dharma Bellamkonda
;; Baye's Theorem

;; Generalized Baye's Theorem
;; Calculates P(Fi|E) given a list in form 
;; ((P(F0) P(E|F0)) (P(F1) P(E|F2)...(P(Fn) P(E|Fn))
;; and the index i
(defun general-bayes-theorem (probabilities &optional (index 0))
  (/ (apply #'* (nth index probabilities)) 
     (apply #'+ (mapcar #'(lambda (p) (apply #'* p)) probabilities))))
  