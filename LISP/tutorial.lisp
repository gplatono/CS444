;;;Just an example of the cond macro
;;;Checks the value of the argument
;;;args:
;;;x - a number (ideally, there should be a type check a la (if (numberp x) ...)
;;;return value - text message
(defun cond_example (x)
  (cond ((> x 0) "Greater than zero")
	((= x 0) "Equal to zero")
	((< x 0) "Less than zero")))



;;;Detects if the value is in the list
;;;args:
;;;lst - list
;;;value - value to loo for
;;;return value - true (T) if value is in lst, NIL otherwise
(defun find_elem(lst value)
  (if (listp lst)
      (loop for item in lst do
	    (if (= value item)
		(return-from find_elem T)))
    NIL)
  "Not a list")



;;;List reversion routine. Recursive version
;;;args:
;;;lst - list data
;;;return value - reversed list
(defun rev(lst)
  (cond
   ((listp lst) (cond
		;;Take reversed cdr of lst and attach car of lst at the end
		 ((cdr lst) (append (rev (cdr lst)) (list (car lst))))
		 (t lst)))
   (t "Argument is not a list")))



;;;List reversion routine. Iterative version
;;;args:
;;;lst - list data
;;;return value - reversed list
(defun rev_loop(lst)
  (let (ret_val)
	;;loop through all the elements starting from the last one
  (loop for i from 0 to (1- (length lst)) do	
	;;take the current element and append ret_val
	(setq ret_val (cons (nth i lst) ret_val)))
  ret_val))



;;;Deep list reverse (reverses all the elements that are lists too)
;;;args:
;;;lst - list data
;;;return value - reversed list
(defun rec_rev(lst)
  (if (listp lst)
	;;reverse lst and then apply deep reverse to 
	;;each element
      (mapcar 'rec_rev (rev lst))
    lst))



;;;Replaces all the occurrences of x in lst by y (including sublists)
;;;args:
;;;lst - list 
;;;x - value to be replaced
;;;y - replacing (new) value
;;;return value - list with all xs replaced by ys
(defun repl(lst x y)
  (if (listp lst)
	;;apply repl to each element of lst
	;;lambda() is used as wrapper to take care of extra arguments of repl 
      (mapcar (lambda (l) (repl l x y)) lst)
    (if (= lst x) y lst)))



;;;Removes the element in lst at position idx
;;;args:
;;;lst - list 
;;;idx - index
;;;return value - lst with idx-th element removed
(defun drop(lst idx)
	;;(subseq a b c) returns the subsequence of a with indices from b to c-1  
  (append (subseq lst 0 idx) (subseq lst (1+ idx) (length lst))))




;;;Inserts val in lst at position idx
;;;args:
;;;lst - list
;;;idx - index
;;;val - value to be inserted
;;;return value - list with val inserted
(defun ins(lst idx val)
  (append (subseq lst 0 idx) (list val) (subseq lst idx (length lst))))

;;;Computes nth Fibonacci number
;;;args:
;;;n - index in the Fibonacci sequence
;;;return value - nth Fibonacci number
(defun fibonacci(n)
  (if (numberp n)
      (if (or (= n 1) (= n 2))
	  1
	(+ (fibonacci (1- n)) (fibonacci (- n 2))))
    "Not a number"))

;;;Computes nth Fibonacci number. Dynamic programming version
;;;Must be used in conjunction with fibonacci_wrapper
;;;args:
;;;n - index in the Fibonacci sequence
;;;table - DP table to look up the subproblem solutions
;;;return value - nth Fibonacci number
(defun fibonacci_dp(n table)
  ;;check the DP table contents. If the values are absent, recurse and fill in the entries
  (if (null (gethash (- n 2) table))
      (setf (gethash (- n 2) table) (fibonacci_dp (- n 2) table)))
  (if (null (gethash (1- n) table))
      (setf (gethash (1- n) table) (fibonacci_dp (1- n) table)))
  ;;Return the value of the nth Fibonacci number
  (+ (gethash (1- n) table) (gethash (- n 2) table)))

;;;Wrapper for fibonacci_dp.
;;;Initializes the DP table, checks the base cases
;;;args:
;;;n - index in the Fibonacci sequence
;;;return value - nth Fibonacci number
(defun fibonacci_wrapper (n)
  (if (or (= n 1) (= n 2))
      1
    (let ((table (make-hash-table)))
      (setf (gethash 1 table) 1)
      (setf (gethash 2 table) 1)
      (fibonacci_dp n table))))
