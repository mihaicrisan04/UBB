;; 10. Return the level of a node X in a tree of type (2). The level of the root element is 0.

;; work in progress

(defun level (tree X lvl)
  (cond
    ((null tree) nil)
    ((eq (car tree) X) lvl)
    (t (let ((result (mapcar (lambda (subtree) (level subtree X (+ lvl 1))) (cdr tree))))
         (if (some #'identity result)
             (apply #'max (remove nil result))
             nil)))))

(format t "~a~%" (level '(1 (2 (4 (8) (9)) (5 (10) (11))) (3 (6) (7))) 5 0)) ; 5


