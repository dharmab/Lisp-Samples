;;; SLCC CSIS 2430 Assignment 2: Sorting
;;; Dharma Bellamkonda

;; Create a sublist of a list given a start and end index
(defun sublist (l start end)
  (butlast (last l (- (list-length l) start)) (- (- (list-length l) 1) end)))

;; Remove an element from a list by index
(defun remove-from-list (l index)
  (append (sublist l 0 (- index 1)) (sublist l (+ index 1) (- (list-length l) 1))))

;; Swap the values of two indicies in a list
(defun list-swap (l first-index second-index)
  (let ((first-element (nth first-index l)) (second-element (nth second-index l)))
    (setf (nth first-index l) second-element)
    (setf (nth second-index l) first-element)
    (return-from list-swap l)))

;; A simpler version of quicksort that is not in-place
(defun quicksort (l)
  (if (<= (list-length l) 1) l
      (let ((pivot-index (random (list-length l))))
        (let ((pivot-value (nth pivot-index l))
              (left (list ()))
              (right (list ())))
          (setf l (remove-from-list l pivot-index))
          (dolist (element l) (if (< element pivot-value) (nconc left (list element))
                                  (nconc right (list element))))
          (append (quicksort (cdr left)) (list pivot-value) (quicksort (cdr right)))))))

;; Bubblesort
(defun bubblesort (l)
  (if (<= (list-length l) 1) l
      (let ((sorted 0))
        (loop do
              (let ((left-index 0) (right-index 1) left-value right-value)
                (loop for i from 1 to (- (list-length l) 1 sorted)
                      do (setf left-value (nth left-index l))
                      (setf right-value (nth right-index l))
                      (if (> left-value right-value) (setf l (list-swap l left-index right-index)))
                      (incf left-index)
                      (incf right-index)))
              (incf sorted)
              while (< sorted (list-length l)))
        l)))

;; Roll-Your-Own sorting algorithm
(defun ownsort (l)
  (let ((min-value (first l)) (max-value (first l)))
    (dolist (element l)
      (if (< element min-value) (setf min-value element))
      (if (> element max-value) (setf max-value element)))
    (let ((counts (make-list (- max-value min-value -1) :initial-element 0)) (sorted-list (list ())))
      (dolist (element l)
        (setf (nth (- element min-value) counts) (incf (nth (- element min-value) counts))))
      (let ((i 0))
        (dolist (element counts)
          (if (> element 0) (nconc sorted-list (make-list element :initial-element (+ i min-value ))))
          (incf i)))
      (cdr sorted-list))))             ; Remove nil from beginning of list

;; Generate a list of random integers
(defun generate-random-ints (n x)
  (let ((random-ints (list ())))
    (loop while (<= (list-length random-ints) n) 
          do (nconc random-ints (list (+ 1 (random x)))))
    (cdr random-ints)))                ; Remove nil from beginning of list  

;; Attempt at an automated benchmark.
;; Always says that ownsort takes 0.000 seconds to execute,
;; despite TIME clocking ownsort correctly when run outside of the function.
;; I do not understand why results are incorrect.
;; Tested with SBLC 1.1.10 under Arch Linux.
(defun sorter-benchmark()
  (let ((dataset (generate-random-ints 50 1000)))
    (format t "Running Quicksort Benchmark...~%")
    (time (quicksort dataset))
    (format t "Running Bubblesort Benchmark...~%")
    (time (bubblesort dataset))
    (format t "Running \"Roll Your Own Sort\" Benchmark...~%")
    (time (ownsort dataset))
    (format t "Benchmarks Complete")))