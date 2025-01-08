;; Determine the list of nodes accesed in preorder in a tree of type (2).

;; preorder(x, l1, l2, ..., ln) = [x] + preorder(l1) + preorder(l2) + ... + preorder(ln), if x is a node
;; preorder() = [], null

(defun preorder (lst)
    (cond
        ((null lst) nil)
        (t (cons (car lst) (mapcan #'preorder (cdr lst))))))

(format t "~a~%" (preorder '(A (B) (C (D) (E))))) ; (A B C D E)

;; (defun preorder (lst)
;;     (cond
;;         ((null lst) nil)
;;         (t (cons (car lst) (mapcan (lambda (subtree) (preorder sublist nil nil)) (cdr lst))))))
;;         ;; if preorder has to take more arguments, we can use a lambda function to pass them
