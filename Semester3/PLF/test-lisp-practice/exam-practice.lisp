
;; 4
(defun remove-occurences (x lst) 
    (cond
        ((null lst) nil)
        ((listp (car lst)) (cons (remove-occurences x (car lst)) (remove-occurences x (cdr lst))))
        ((equal (car lst) x) (remove-occurences x (cdr lst)))
        (t (cons (car lst) (remove-occurences x (cdr lst))))))


(format t "~a~%" (remove-occurences 'a '(1 (2 a (3 a)) (a))))


;; 5

(defun last-atom-nonnumeric-p (lst)
  (if (atom lst)
      (not (numberp lst))
      (last-atom-nonnumeric-p (car (last lst)))))

(defun count-sublists-nonnumeric-last (lst)
  (cond
    ((atom lst) 0)
    (t
     (let ((subcount (reduce #'+ (mapcar #'count-sublists-nonnumeric-last lst) :initial-value 0)))
       (if (last-atom-nonnumeric-p lst)
           (1+ subcount)
           subcount)))))

;; Example usage:
;; (count-sublists-nonnumeric-last '(A (B 2) (1 C 4) (D 1 (6 F)) ((G 4) 6) F)) => 2
(format t "~a~%" (count-sublists-nonnumeric-last '(A (B 2) (1 C 4) (D 1 (6 F)) ((G 4) 6) F)))



;; ------------------------------------------------------------------------------------------------------------------------

(defun rm-on-k (lst k &optional (ck 0))
  (cond
    ((null lst) nil)
    ((atom lst) (if (= ck k) 0 lst))
    ((< ck k)
     (mapcar (lambda (x) (rm-on-k x k (+ ck 1))) lst))
    (t lst)))

;; Example usage:
(format t "~a~%" (rm-on-k '(a (1 (2 b)) (c (d))) 2)) ; Output: (a (0 (0 b)) (0 (d)))


;; ------------------------------------------------------------------------------------------------------------------------


(defun rp-div3 (lst)
  (cond
    ((null lst) nil)
    ((atom lst) 
      (cond
        ((numberp lst) (if (zerop (mod lst 3)) nil lst))
        (t lst)))
    (t
      (mapcar #'rp-div3 lst))))

(format t "~a~%" (rp-div3 '(1 (2 a(3 a)) (6))))


;; ------------------------------------------------------------------------------------------------------------------------

