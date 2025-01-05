
;; a) Write a function to return the n-th element of a list, or NIL if such an element does not exist.
(defun n-th (lst k)
    (cond
      ((and (null lst) (> k 1)) nil)
      ((eq k 1) (car lst))
      ((> k 1) (n-th (cdr lst) (- k 1)))))

(format t "~a~%" (n-th '(1 2 3 4 5) 2)) ; 2
(format t "~a~%" (n-th '(1 2 3 4 5) 5)) ; 5
(format t "~a~%" (n-th '(1 2 3 4 5) 6)) ; nil


;; b) Write a function to check whether an atom E is a member of a list which is not necessarily linear.
(defun my-member (lst e)
    (cond
      ((null lst) nil)
      ((atom (car lst)) (if (eq (car lst) e) t (my-member (cdr lst) e)))
      (t (or (my-member (car lst) e) (my-member (cdr lst) e)))))

;; pseudo code
;; if lst is empty, return nil
;; if the first element of lst is an atom
;;   if it is equal to e, return t
;;   else return my-member with the rest of the list
;; else
;;   return my-member with the first element of lst OR my-member with the rest of the list

(format t "~a~%" (my-member '(1 2 3 (4 5) 6) 5)) ; t
(format t "~a~%" (my-member '(1 2 3 (4 5) 6) 6)) ; t
(format t "~a~%" (my-member '(1 2 3 (4 5) 6) 7)) ; nil


;; c) Write a function to determine the list of all sublists of a given list, on any level.
;;  A sublist is either the list itself, or any element that is a list, at any level. Example:
;;  (1 2 (3 (4 5) (6 7)) 8 (9 10)) => 5 sublists :
;;  ( (1 2 (3 (4 5) (6 7)) 8 (9 10)) (3 (4 5) (6 7)) (4 5) (6 7) (9 10) )
(defun count-sublists (lst)
    (cond 
      ((null lst) 0) 
      ((listp (car lst)) (+ 1 (count-sublists (car lst)) (count-sublists (cdr lst))))
      (t (count-sublists (cdr lst)))))

(format t "~a~%" (+ 1 (count-sublists '(1 2 (3 (4 5) (6 7)) 8 (9 10))))) ; 5
(format t "~a~%" (+ 1 (count-sublists '((1 2 (3 (4 5) (6 7)) 8 (9 10)) (3 (4 5) (6 7)) (4 5) (6 7) (9 10))))) ; 12


;; d) Write a function to transform a linear list into a set.
(defun insert (x lst)
  (cond
    ((null lst) (list x))
    ((<= x (car lst)) (cons x lst))
    (t (cons (car lst) (insert x (cdr lst))))))

(defun insertion-sort (lst)
  (if (null lst)
      nil
      (insert (car lst) (insertion-sort (cdr lst)))))

(defun member-p (x lst)
  (cond
    ((null lst) nil)
    ((eq x (car lst)) t)
    (t (member-p x (cdr lst)))))

(defun my-remove-duplicates (lst)
  (cond
    ((null lst) nil)
    ((member-p (car lst) (cdr lst)) (my-remove-duplicates (cdr lst)))
    (t (cons (car lst) (my-remove-duplicates (cdr lst))))))

(defun list-to-set (lst)
    (insertion-sort (my-remove-duplicates lst)))

(format t "~a~%" (list-to-set '(1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10))) ; (1 2 3 4 5 6 7 8 9 10)