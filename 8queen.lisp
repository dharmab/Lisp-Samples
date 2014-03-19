;; Dharma Bellamkonda
;; 8 Queens Problem

(defun 8-queen (queen-positions left-diagonal-invalid-positions right-diagonal-invalid-positions)
  (when (= (length queen-positions) 8) (return-from 8-queen queen-positions))
  (let ((valid-positions (set-difference 
                           '(0 1 2 3 4 5 6 7) 
                           (union 
                             queen-positions
                             (union
                               left-diagonal-invalid-positions 
                               right-diagonal-invalid-positions)))))
    (setf left-diagonal-invalid-positions 
          (remove-if #'(lambda (x) (< x 0)) 
                     (mapcar #'(lambda (x) (- x 1)) 
                             left-diagonal-invalid-positions)))
    (setf right-diagonal-invalid-positions 
          (remove-if #'(lambda (x) (> x 7)) 
                     (mapcar #'(lambda (x) (+ x 1)) 
                             right-diagonal-invalid-positions)))
    (dolist (x valid-positions)
      (let ((recursive-call 
              (8-queen 
                (append queen-positions (list x))
                (if (> x 0) 
                    (append left-diagonal-invalid-positions (list (- x 1)))
                    left-diagonal-invalid-positions)
                (if (< x 7)
                    (append right-diagonal-invalid-positions (list (+ x 1)))
                    right-diagonal-invalid-positions))))
        (when recursive-call (return-from 8-queen recursive-call))))
    (return-from 8-queen nil)))

(defparameter *solution* (8-queen '(0) '() '()))

(defun chess-notation (col row)
  (format nil "~a~a" (char "abdcdefgh" col) (write-to-string row)))

(loop for i from 1 to (length *solution*) do
      (format t "~a. ~a~%" i (chess-notation i (nth (- i 1) *solution*))))