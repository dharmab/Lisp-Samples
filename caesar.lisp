;;; Dharma Bellamkonda
;;; Caesar Cipher

(defun caesar-cipher (message key &optional (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (map 'string #'(lambda (c)
                  (if (find c alphabet)
                      (char alphabet (mod (+ (position c alphabet) key) (length alphabet)))
                      c)) 
       (string-upcase message)))