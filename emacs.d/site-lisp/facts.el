;;; facts.el --- Tools for working with my facts database.

;;; Commentary:
;;

(defun facts--parse-list-rec (list current-fact all-facts)
  (let ((head (car list))
        (tail (cdr list)))
    (cond
     ((null head)
      (reverse all-facts))
     ((listp head)
      (facts--parse-list-rec
       tail
       current-fact
       (append (facts--parse-list-rec head current-fact '()) all-facts)))
     ((symbolp head)
      (facts--parse-list-rec
       tail
       (cons head current-fact)
       all-facts))
     ((stringp head)
      (facts--parse-list-rec
       tail
       current-fact
       (cons (reverse (cons head current-fact)) all-facts))))))

(defun facts--parse-sexp (sexp)
  (facts--parse-list-rec sexp '() '()))

(defun facts--matches-filter (filter fact)
  (seq-every-p (lambda (keyword) (memq keyword fact)) filter))

(defun facts (db &rest filter)
  (cl-loop for fact in db
           if (facts--matches-filter filter fact)
           collect fact))

(provide 'facts)

;;; facts.el ends here
