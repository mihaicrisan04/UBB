;; a linear list is given. insert in the list a given element, E, aftre every N steps, N-given

;; insert-every-n(l1, l2, .., ln, n, e, steps) = [], if lst is null
;;                                             = [l1] + insert-every-n(l2, l3, ..., ln, n-1, e, steps), if n != 1
;;                                             = [l1] + [e] + insert-every-n(l2, l3, ..., ln, steps, e, steps), if n == 1

(defun insert-every-n (lst n e steps)
    (cond
        ((null lst) nil)
        ((= n 1) (cons (car lst) (cons e (insert-every-n (cdr lst) steps e steps))))
        (t (cons (car lst) (insert-every-n (cdr lst) (- n 1) e steps)))))

(defun insert-every-n-wrapper (lst n e)
    (insert-every-n lst n e n))


(format t "~a~%" (insert-every-n-wrapper '(1 2 3 4 5) '2 '7)) ;; (1 2 7 3 4 7 5)
(format t "~a~%" (insert-every-n-wrapper '(1 2 3 4) '3 '7)) ;; (1 2 3 7 4)
(format t "~a~%" (insert-every-n-wrapper '(1 2 3 4) '1 '7)) ;; (1 7 2 7 3 7 4 7)

