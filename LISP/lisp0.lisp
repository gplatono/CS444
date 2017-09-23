(defun occur-in (x y)
  (if (equal x y)
      T
    (if	(and (not (null y)) (listp y))
	(or
	 (occur-in x (car y))
	 (occur-in x (cdr y))))))

(defun deriv1(mono)
  (if (and (listp mono) (eq (length mono) 3))
      (let ((c (car mono)) (x (cadr mono)) (i (caddr mono)))
	(if (and
	     (and (numberp c) (symbolp x))
	     (numberp i))
	    (if (or (eq 0 c) (eq 0 i))
		(list 0 x 0)
	      (list (* c i) x (1- i)))
	  (format t "Wrong input format")))
    (format t "Wrong input format")))

(defun tree-yield(tree)
  (if (atom tree)
      tree
    (mapcar 'tree-yield tree)))
