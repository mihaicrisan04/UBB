;; a) Write a function to test whether a list is linear.

(defun linear-p (lst)
    (cond
        ((null lst) t)
        ((listp (car lst)) nil)
        (t (linear-p (cdr lst)))))

(format t "~a~%" (linear-p '(1 2 3 4 5))) ; t
(format t "~a~%" (linear-p '(1 2 (3 4) 5))) ; nil



;; b) Write a function to replace the first occurence of an element E in a given list with an other element O.
(defun replace-first (lst o e)
    (cond
        ((null lst) nil)
        ((eq (car lst) o) (cons e (cdr lst)))
        (t (cons (car lst) (replace-first (cdr lst) o e)))))

(format t "~a~%" (replace-first '(1 2 3 4 5 3) 3 6)) ; (1 2 6 4 5)



;; c) Write a function to replace each sublist of a list with its last element.
;;  A sublist is an element from the first level, which is a list.
;;  Example: (a (b c) (d (e (f)))) ==> (a c (e (f))) ==> (a c (f)) ==> (a c f)
;;  (a (b c) (d ((e) f))) ==> (a c ((e) f)) ==> (a c f)

(defun get-last-elem (lst)
    (cond 
        ((null lst) nil)
        ((eq (length lst) 1) (car lst))
        (t (get-last-elem (cdr lst)))))

(defun replace-sublist (lst)
    (cond 
        ((null lst) nil)
        ((listp (car lst)) (cons (get-last-elem (car lst)) (replace-sublist (cdr lst))))
        (t (cons (car lst) (replace-sublist (cdr lst))))))


(defun replace-sublist-wrapper (lst)
  (if (some #'listp lst)
      (replace-sublist-wrapper (replace-sublist lst))
      lst))

(format t "~a~%" (replace-sublist-wrapper '(a (b c) (d (e (f)))))) ; (A C F)



;; d) Write a function to merge two sorted lists without keeping double values.
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

(defun my-append (lst1 lst2)
    (cond
        ((null lst1) lst2)
        (t (cons (car lst1) (my-append (cdr lst1) lst2)))))

(defun insert (x lst)
  (cond
    ((null lst) (list x))
    ((<= x (car lst)) (cons x lst))
    (t (cons (car lst) (insert x (cdr lst))))))

(defun insertion-sort (lst)
  (if (null lst)
      nil
      (insert (car lst) (insertion-sort (cdr lst)))))

(defun my-merge (lst1 lst2)    
    (my-remove-duplicates (insertion-sort (my-append lst1 lst2))))
    
(format t "~a~%" (my-merge '(1 2 3 4 5) '(3 4 5 6 7))) ; (1 2 3 4 5 6 7)
