;; A binary tree is memorised in the following way:
;; (node (list-subtree-1) (list-subtree-2) ...)
;; is represented as follows:
;; (A (B) (C (D) (E)))

;;      a
;;    / | \
;;  b   c   f
;; / \    / | \
;; d e    g h i

;; Define a function that replaces one node with another one in a n-tree represented as: root
;; list_of_nodes_subtree1... list_of_nodes_subtreen)
;; Eg: tree is (a (b (c)) (d) (e (f))) and node 'b' will be replace with node 'g' => tree (a (g (c)) (d) (e (f)))}


;; (defun replace-node (lst node replacement)
;;   (if (null lst)
;;       nil
;;       (if (eq (car lst) node)
;;           (cons replacement (mapcar #'(lambda (child) (replace-node child node replacement)) (cdr lst)))
;;           (cons (car lst) (mapcar #'(lambda (child) (replace-node child node replacement)) (cdr lst))))))


;; replace-node( root, l1, ..., ln, node, replacement) = null,  if root == null
;;                                                     = root + lambda:(child) replace-node(child, node replacement) (l1, l2, ..., ln), if root != node and root != null
;;                                                     = replacement + lambda:(child) replace-node(child, node replacement) (l1, l2, ..., ln), if root == node and root != null

(defun replace-node (lst node replacement)
  (cond
    ((null lst) nil)
    ((eq (car lst) node)
     (cons replacement (mapcar #'(lambda (child) (replace-node child node replacement)) (cdr lst))))
    (t
     (cons (car lst) (mapcar #'(lambda (child) (replace-node child node replacement)) (cdr lst))))))

(format t "~a~%" (replace-node '(A (B (C)) (D) (E (F))) 'B 'G)) ; (A (G (C)) (D) (E (F)))
(format t "~a~%" (replace-node '(A (B) (C (D) (E)) (F (G) (H) (I))) 'C 'X)) ; (A (B) (X (D) (E)) (F (G) (H) (I)))

;; (defun replace-node (lst node replacement)
;;     (if (null lst)
;;         nil
;;         (if (eq (car lst) node)
;;             (cons replacement (replace-node-children (cdr lst) node replacement))
;;             (cons (car lst) (replace-node-children (cdr lst) node replacement)))))

;; (defun replace-node-children (children node replacement)
;;   (if (null children)
;;       nil
;;       (cons (replace-node (car children) node replacement)
;;             (replace-node-children (cdr children) node replacement))))
