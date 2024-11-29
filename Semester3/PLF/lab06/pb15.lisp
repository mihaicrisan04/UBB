

;; A binary tree is memorised in the following way:
;; (node (list-subtree-1) (list-subtree-2) ...)
;; is represented as follows:
;; (A (B) (C (D) (E)))

;; 15. Determine the list of nodes accesed in postorder in a tree of type.

(defun postorder (lst)
  (if (null lst)
      nil
      (append (postorder (second lst))
              (postorder (third lst))
              (list (car lst)))))

(format t "~a~%" (postorder '(A (B) (C (D) (E))))) ; (B D E C A)