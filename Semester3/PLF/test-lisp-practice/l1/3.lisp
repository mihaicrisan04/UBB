
;; a) Write a function that inserts in a linear list a given atom A after the 2nd, 4th, 6th, ... element.
(defun insert-th (lst a pos)
  (cond
    ((null lst) nil)
    ((eq (mod pos 2) 0) (cons (car lst) (cons a (insert-th (cdr lst) a (+ pos 1)))))
    (t (cons (car lst) (insert-th (cdr lst) a (+ pos 1))))))

(format t "~a~%" (insert-th '(1 2 3 4 5 6 7 8 9 10) 'a 1)) ; (1 A 2 3 A 4 5 A 6 7 A 8 9 A 10)


;; b) Write a function to get from a given list the list of all atoms, on any
;;  level, but reverse order. Example:
;;  (((A B) C) (D E)) ==> (E D C B A)

(defun list-atoms (lst)
  (cond
    ((null lst) nil)
    ((listp (car lst)) (append (list-atoms (car lst)) (list-atoms (cdr lst))))
    (t (cons (car lst) (list-atoms (cdr lst))))))

(defun my-reverse (lst)
  (cond 
    ((null lst) nil)
    (t (append (my-reverse (cdr lst)) (list (car lst))))))

(format t "~a~%" (my-reverse (list-atoms '(((A B) C) (D E))))) ; (E D C B A)


;; c) Write a function that returns the greatest common divisor of all numbers in a nonlinear list.

(defun my-gcd (a b)
  (if (= b 0) a (my-gcd b (mod a b))))

(defun gcd-all (lst)
  (cond
    ((null lst) 0)
    ((atom (car lst)) (my-gcd (car lst) (gcd-all (cdr lst))))
    (t (my-gcd (gcd-all (car lst)) (gcd-all (cdr lst))))))

(format t "~a~%" (gcd-all '(1 2 3 4 5 6 7 8 9 10))) ; 1
(format t "~a~%" (gcd-all '(10 15 25))) ; 5


;; d) Write a function that determines the number of occurrences of a given atom in a nonlinear list.
(defun occur (lst x)
  (cond 
    ((null lst) 0)
    ((listp (car lst)) (+ (occur (car lst) x) (occur (cdr lst) x)))
    (t (if (eq (car lst) x) 
      (+ 1 (occur (cdr lst) x)) 
      (occur (cdr lst) x)))))

(format t "~a~%" (occur '(1 2 3 (4 5) 5) 5)) ; 2
(format t "~a~%" (occur '(1 2 3 (4 5) 5) 6)) ; 0
