;; 5.
;; a) Write twice the n-th element of a linear list. Example: for (10 20 30 40 50) and n=3 will produce (10
;; 20 30 30 40 50).
;; b) Write a function to return an association list with the two lists given as parameters.
;;  Example: (A B C) (X Y Z) --> ((A.X) (B.Y) (C.Z)).
;; c) Write a function to determine the number of all sublists of a given list, on any level.
;;  A sublist is either the list itself, or any element that is a list, at any level. Example:
;;  (1 2 (3 (4 5) (6 7)) 8 (9 10)) => 5 lists:
;; (list itself, (3 ...), (4 5), (6 7), (9 10)).
;; d) Write a function to return the number of all numerical atoms in a list at superficial level.


;; a) 
(defun twice-nth-element (lst n)
  (if (null lst)
      nil
      (if (= n 1)
          (cons (car lst) (cons (car lst) (twice-nth-element (cdr lst) n)))
          (cons (car lst) (twice-nth-element (cdr lst) (- n 1))))))

(format t "~a~%" (twice-nth-element '(10 20 30 40 50) 3))


;; b)
(defun assoc-list (lst1 lst2)
  (if (or (null lst1) (null lst2))
      nil
      (cons (cons (car lst1) (car lst2)) (assoc-list (cdr lst1) (cdr lst2)))))

(format t "~a~%" (assoc-list '(A B C) '(X Y Z)))


;; c)
(defun count-sublists (lst)
  (if (null lst)
      0
      (if (listp (car lst))
          (+ 1 (count-sublists (cdr lst)) (count-sublists (car lst)))
          (+ 1 (count-sublists (cdr lst))))))

(format t "~a~%" (count-sublists '(1 2 (3 (4 5) (6 7)) 8 (9 10))))


;; d)
(defun count-numerical-atoms (lst)
  (if (null lst)
      0
      (if (numberp (car lst))
          (+ 1 (count-numerical-atoms (cdr lst)))
          (count-numerical-atoms (cdr lst)))))

(format t "~a~%" (count-numerical-atoms '(1 2 (3 (4 5) (6 7)) 8 (9 10))))

