;; Write a function to check if an atom is member of a list (the list is non-liniar)


(defun my-member (lst atom)
    (cond
        ((null lst) nil)
        ((atom (car lst))
         (cond 
            ((eq (car lst) atom) t)
            (t (my-member (cdr lst) atom))))
        ((listp (car lst))
         (cond
            ((my-member (car lst) atom) t)
            (t (my-member (cdr lst) atom))))))


(format t "~a~%" (my-member '(a b (c d) e) 'd))
(format t "~a~%" (my-member '(a b (c d) e) 'x))