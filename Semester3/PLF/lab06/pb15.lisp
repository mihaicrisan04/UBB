

;; A binary tree is memorised in the following way:
;; (node (list-subtree-1) (list-subtree-2) ...)
;; is represented as follows:
;; (A (B) (C (D) (E)))

;; 15. Determine the list of nodes accesed in postorder in a tree of type.

;;      a
;;    / | \
;;  b   c   f
;; / \    / | \
;; d e    g h i
;; posotorder: d e b c g h i f a


;; my-append(l1, l2, ..., ln, m1, m2, ...mn) = l1 + my-append(l2, ..., ln, m1, m2, ... mn), if l1 != null
;;                                           = m1, m2, ..., mn, if l1 == null

;; posotorder(e, l1, ..., ln) = postorder-children(l2, ..., ln) + e, if l1 != nil
;;                            = e, if l1 = nil

;; posotorder-children(l1, ..., ln) = postorder(l1) + postorder-children(l2, ..., ln), if l1 != nil
;;                                   = nil, if l1 = nil

(defun my-append (list1 list2)
  (if (null list1)
      list2
      (cons (car list1) (my-append (cdr list1) list2))))

(defun postorder (lst)
  (if (null lst)
      nil
      (my-append (postorder-children (cdr lst))
                 (list (car lst)))))

(defun postorder-children (children)
  (if (null children)
      nil
      (my-append (postorder (car children))
                 (postorder-children (cdr children)))))

(format t "~a~%" (postorder '(A (B) (C (D) (E)) (F (G) (H) (I))))) ; (B D E C G H I F A)
(format t "~a~%" (postorder '(A (B) (C (D) (E))))) ; (B D E C A)

