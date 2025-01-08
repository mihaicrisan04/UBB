;; how to use cond
(cond
  (condition1 body1)
  (condition2 body2)
  ...
  (t default-body))  ;; 't' acts as the default case

(defun example-cond (x)
  (cond
    ((< x 0) 'negative)
    ((= x 0) 'zero)
    ((> x 0) 'positive)
    (t 'unknown)))  ;; default case


;; how to use cons (constructs a new pair (or list) by adding an element to the front of an existing list.)
(cons element list)
(cons 1 '(2 3 4))  ;; (1 2 3 4)


;; how to use append
(append list1 list2 ... listN)
(append '(1 2) '(3 4) '(5 6))  ;; (1 2 3 4 5 6)


;; how to use lambda
(lambda (arg1 arg2 ... argN) body)
(mapcar (lambda (x) (* x x)) '(1 2 3 4))  ;; (1 4 9 16)


;; how to use mapcar and mapcan
(mapcar function list1 list2 ... listN) ;; list of results
(mapcar (lambda (x) (list x x)) '(1 2 3))  ;; ((1 1) (2 2) (3 3))

(mapcan function list1 list2 ... listN) ;;list of results concatenated
(mapcan (lambda (x) (list x x)) '(1 2 3))  ;; (1 1 2 2 3 3)


;; how to use let
(let ((var1 value1)
      (var2 value2)
      ...)
  body)


;; how to use max and min
(max x1 x2 ... xn)
(max 1 3 2 5 4)  ;; 5

(min x1 x2 ... xn)
(min 1 3 2 5 4)  ;; 1


;; how to use operators
(+ x1 x2 ... xn)
(- x1 x2 ... xn)
(* x1 x2 ... xn)
(/ x1 x2 ... xn)
(mod dividend divisor)
(mod 10 3)  ;; 1
(eq (mod 10 3) 1)  ;; t
(#'evenp 2)  ;; t
(#'oddp 2)  ;; nil


;; how to use list
(list x1 x2 ... xn)
(list 1 2 3)  ;; (1 2 3)
(list 'a 'b 'c)  ;; (a b c)


;; --------------------------------------------------------------------------------


;; Insertion sort
(defun insert (x lst)
  (cond
    ((null lst) (list x))
    ((<= x (car lst)) (cons x lst))
    (t (cons (car lst) (insert x (cdr lst))))))

(defun insertion-sort (lst)
  (if (null lst)
      nil
      (insert (car lst) (insertion-sort (cdr lst)))))


;; Member for non-linear lists
(defun member-non-linear-p (x lst)
    (cond
      ((null lst) nil)
      ((atom (car lst)) (if (eq (car lst) x) t (member-non-linear-p x (cdr lst))))
      (t (or (member-non-linear-p x (car lst)) (member-non-linar-p x (cdr lst))))))
    
(format t "~a~%" (member-non-linear-p 5 '(1 2 3 (4 5) 6))) ; t


;; Member for linear lists
(defun member-p (x lst)
  (cond
    ((null lst) nil)
    ((eq x (car lst)) t)
    (t (member-p x (cdr lst)))))


;; Remove duplicates
(defun my-remove-duplicates (lst)
  (cond
    ((null lst) nil)
    ((member-p (car lst) (cdr lst)) (my-remove-duplicates (cdr lst)))
    (t (cons (car lst) (my-remove-duplicates (cdr lst))))))


;; Count sublists of a list including the list itself
(defun count-sublists (lst)
    (cond 
      ((null lst) 0) 
      ((listp (car lst)) (+ 1 (count-sublists (car lst)) (count-sublists (cdr lst))))
      (t (count-sublists (cdr lst)))))

(format t "~a~%" (+ 1 (count-sublists '(1 2 (3 (4 5) (6 7)) 8 (9 10))))) ; 5


;; N-th element of a list
(defun n-th (lst k)
    (cond
      ((and (null lst) (> k 1)) nil)
      ((eq k 1) (car lst))
      ((> k 1) (n-th (cdr lst) (- k 1)))))


;; Reverse a list
(defun my-reverse (lst)
  (cond 
    ((null lst) nil)
    (t (append (my-reverse (cdr lst)) (list (car lst))))))


;; List of atoms from a non-linear list
(defun list-atoms (lst)
  (cond
    ((null lst) nil)
    ((listp (car lst)) (append (list-atoms (car lst)) (list-atoms (cdr lst))))
    (t (cons (car lst) (list-atoms (cdr lst))))))


;; occurences of an atom in a non-linear list
(defun occur (lst x)
  (cond 
    ((null lst) 0)
    ((listp (car lst)) (+ (occur (car lst) x) (occur (cdr lst) x)))
    (t (if (eq (car lst) x) 
      (+ 1 (occur (cdr lst) x)) 
      (occur (cdr lst) x)))))


;; gcd of 2 numbers
(defun my-gcd (a b)
  (if (= b 0) a (my-gcd b (mod a b))))

;; gcd of all numbers in a non-linear list
(defun gcd-all (lst)
  (cond
    ((null lst) 0)
    ((atom (car lst)) (my-gcd (car lst) (gcd-all (cdr lst))))
    (t (my-gcd (gcd-all (car lst)) (gcd-all (cdr lst))))))


;; preorder of a tree of type (2)
(defun preorder (lst)
    (cond
        ((null lst) nil)
        (t (cons (car lst) (mapcan #'preorder (cdr lst))))))

(format t "~a~%" (preorder '(A (B) (C (D) (E))))) ; (A B C D E)