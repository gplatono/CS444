;;;;Tutorial File
#||

||#
;;;Just a stub
(defun stub ()
  ;;
  (let ((a 1) (b 2))
  (format NIL "~a ~a" a b))) ;Line

(defun test (x)
  (cond ((> x 0) "Greater than zero")
	((= x 0) "Equal to zero")
	((< x 0) "Less than zero")))

(defun find_elem(lst value)
  (if (listp lst)
      (loop for item in lst do
	    (if (= value item)
		(return-from find_elem T)))
    NIL)
  "Not a list")

;
(defun rev(lst)
  (if (listp lst)
      (if lst
	  (append (rev (cdr lst)) (list (car lst)))))
  lst)

(defun rec_rev(lst)
  (if (listp lst)
      (setf lst (rev lst))
      (mapcar 'rec ))
    lst)
