
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
