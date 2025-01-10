;; a) Write a function to insert an element E on the n-th position of a linear list.

(defun insert-nth (lst n e)
    (cond
        ((null lst) nil)
        ((= n 1) (cons e lst))
        (t (cons (car lst) (insert-nth (cdr lst) (- n 1) e)))))

(format t "~a~%" (insert-nth '(1 2 3 4 5) 3 6)) ; (1 2 6 3 4 5)


;; b) Write a function to return the sum of all numerical atoms of a list, at any level.
(defun sum-of-atoms (lst)
    (cond
        ((null lst) 0)
        ((numberp (car lst)) (+ (car lst) (sum-of-atoms (cdr lst))))
        ((listp (car lst)) (+ (sum-of-atoms (car lst)) (sum-of-atoms (cdr lst))))))

(format t "~a~%" (sum-of-atoms '(1 2 (3 (4 5 6)) 7 8 (1 2 3) 9 10))) ; 61


;; c) Write a function to return the set of all sublists of a given linear list. Ex. For list ((1 2 3) ((4 5) 6)) =>
;; ((1 2 3) (4 5) ((4 5) 6))
(defun collect-sublists (lst)
  (cond
    ((null lst) nil)
    ((listp (car lst)) (append  (list (car lst)) 
                                (collect-sublists (car lst)) 
                                (collect-sublists (cdr lst))))
    (t (collect-sublists (cdr lst)))))

(format t "~a~%" (collect-sublists '((1 2 3) ((4 5) 6)))) ; ((1 2 3) (4 5) ((4 5) 6))
(format t "~a~%" (collect-sublists '(1 2 3 (4 5)))) ; ((4 5))


;; d) Write a function to test the equality of two sets, without using the difference of two sets.
(defun remove-occurrences (x lst)
  (cond
    ((null lst) nil)
    ((eq x (car lst)) (remove-occurrences x (cdr lst)))
    (t (cons (car lst) (remove-occurrences x (cdr lst))))))

(defun equal-sets (lst1 lst2)
  (cond
    ((and (null lst1) (null lst2)) t)
    ((or (null lst1) (null lst2)) nil)
    ((member (car lst1) lst2) (equal-sets (cdr lst1) (remove-occurrences (car lst1) lst2)))
    (t nil)))

(format t "~a~%" (equal-sets '(1 2 3) '(3 2 1))) ; t
(format t "~a~%" (equal-sets '(1 2 3) '(3 2 1 4))) ; nil
