;; a) Write a function to return the product of all the numerical atoms from a list, at superficial level.

(defun product-of-numerical-atoms (lst)
    (cond
        ((null lst) 1)
        ((numberp (car lst)) (* (car lst) (product-of-numerical-atoms (cdr lst))))
        (t (product-of-numerical-atoms (cdr lst)))))

(format t "~a~%" (product-of-numerical-atoms '(1 2 3 4 5 6 7 8 (1 2 3) 9 10))) ; 3628800



;; b) Write a function to replace the first occurence of an element E in a given list with an other element O.
(defun replace-first (lst o e)
    (cond
        ((null lst) nil)
        ((eq (car lst) o) (cons e (cdr lst)))
        (t (cons (car lst) (replace-first (cdr lst) o e)))))

(format t "~a~%" (replace-first '(1 2 3 4 5 3) 3 6)) ; (1 2 6 4 5)



;; c) Write a function to compute the result of an arithmetic expression memorised
;;  in preorder on a stack. Examples:
;;  (+ 1 3) ==> 4 (1 + 3)
;;  (+ * 2 4 3) ==> 11 [((2 * 4) + 3)
;;  (+ * 2 4 - 5 * 2 2) ==> 9 ((2 * 4) + (5 - (2 * 2))
(defun compute (lst)
    (cond
        ((null lst) 0)
        ((numberp (car lst)) (car lst))
        ((eq (car lst) '+) (+ (compute (cdr lst)) (compute (cddr lst))))
        ((eq (car lst) '-) (- (compute (cdr lst)) (compute (cddr lst))))
        ((eq (car lst) '*) (* (compute (cdr lst)) (compute (cddr lst))))
        ((eq (car lst) '/) (/ (compute (cdr lst)) (compute (cddr lst))))))



;; d) Write a function to produce the list of pairs (atom n), where atom appears for n times in the parameter
;; list. Example:
;;  (A B A B A C A) --> ((A 4) (B 2) (C 1))
(defun count-occurrences (x lst)
  (cond
    ((null lst) 0)
    ((eq x (car lst)) (1+ (count-occurrences x (cdr lst))))
    (t (count-occurrences x (cdr lst)))))

(defun remove-element (x lst)
  (cond
    ((null lst) nil)
    ((eq x (car lst)) (remove-element x (cdr lst)))
    (t (cons (car lst) (remove-element x (cdr lst))))))

(defun produce-pairs (lst)
  (cond
    ((null lst) nil)
    (t (let ((elem (car lst))
             (count (count-occurrences (car lst) lst)))
         (cons (list elem count) (produce-pairs (remove-element elem lst)))))))

(format t "~a~%" (produce-pairs '(A B A B A C A))) ; ((A 4) (B 2) (C 1))