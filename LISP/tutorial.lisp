
;;;Custom list reversion routine. Recusrive version
;;;args:
;;;lst - list data
(defun rev(lst)
  (cond
   ((listp lst) (cond
		 ((cdr lst) (append (rev (cdr lst)) (list (car lst))))
		 (t lst)))
   (T "Argument is not a list")))

;;;Custom list reversion routine. Iterative version
;;;args:
;;;lst - list data
(defun rev_loop(lst)
  (setq ret_val ())
  (loop for i from (- (length lst) 1) downto 0 do
	(format t "~a" (nth i lst))
	(setq ret_val (append ret_val (list (nth i lst)))))
  ret_val)


(defun check_unit (lst)
  (loop for i in lst do
	(cond
	 ((= i '1) (return-from check_unit T))))
  NIL)
