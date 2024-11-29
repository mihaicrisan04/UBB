;; 7
;; a) Write a function to eliminate the n-th element of a linear list.
;; b) Write a function to determine the successor of a number represented digit by digit as a list, without
;; transforming the representation of the number from list to number. Example: (1 9 3 5 9 9) --> (1 9 3 6 0
;; 0)
;; c) Write a function to return the set of all the atoms of a list.
;;  Exemplu: (1 (2 (1 3 (2 4) 3) 1) (1 4)) ==> (1 2 3 4)
;; d) Write a function to test whether a linear list is a set.

;; a)
(defun elim-nth (lst n)
    (if (= n 1)
        (cdr lst)
        (cons (car lst) (elim-nth (cdr lst) (- n 1)))))

(format t "~a~%" (elim-nth '(10 20 30 40 50) 3))

;; b)
(defun successor (lst)
    (if (null (cdr lst))
        (if (= (car lst) 9)
            (cons 0 nil)
            (cons (+ (car lst) 1) nil))
        (let ((rest (successor (cdr lst))))
         (if (= (car rest) 0)
            (if (= (car lst) 9)
                (cons 0 rest)
                (cons (+ (car lst) 1) rest))
            (cons (car lst) rest)))))

(format t "~a~%" (successor '(1 9 3 5 9 9)))


;; c)
(defun flatten-list (lst)
    (if (null lst)
        nil
        (if (listp (car lst))
            (append (flatten-list (car lst)) (flatten-list (cdr lst)))
            (cons (car lst) (flatten-list (cdr lst))))))

(defun my-remove-duplicates (lst)
    (if (null lst)
        nil
        (let ((rest (my-remove-duplicates (cdr lst))))
         (if (member (car lst) rest)
            rest
            (cons (car lst) rest)))))

(defun insert (element sorted-list)
  (if (or (null sorted-list) (< element (car sorted-list)))
      (cons element sorted-list)
      (cons (car sorted-list) (insert element (cdr sorted-list)))))

(defun insertion-sort (lst)
  (if (null lst)
      nil
      (insert (car lst) (insertion-sort (cdr lst)))))

(defun set-atoms (lst)
  (insertion-sort (my-remove-duplicates (flatten-list lst))))

(format t "~a~%" (set-atoms '(1 (2 (1 3 (2 4) 3) 1) (1 4))))

;; d)
(defun is-set (lst)
    (if (null lst)
        t
        (if (member (car lst) (cdr lst))
            nil
            (is-set (cdr lst)))))

(format t "~a~%" (is-set '(1 2 3 4 5 6 7 8 9 10)))
(format t "~a~%" (is-set '(1 2 3 4 5 6 7 8 9 10 1)))