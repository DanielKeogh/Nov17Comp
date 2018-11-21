;;;; snakes.lisp

(ql:quickload :cl-strings)
(ql:quickload :groupby)

(defun de-interleave (s)
	(loop
		 for i from 0
		 for x in s
		 when (= 0 (mod i 2))
		 collect x))

(defun get-inputs ()
	(with-open-file (f "task4-linux.dat")
		(loop for l = (read-line f nil)
			 while l for s = (mapcar (lambda (q) (1- (parse-integer q))) (cl-strings:split l))
			 collect (de-interleave s)
			 collect (de-interleave (cdr s)))))

(defun print-board (board)
	(let (snakes ladders)
		(loop for c across board
			 do (if (listp c)
						(let* ((s1 (1+ (car c)))
									 (s2 (1+ (cadr c)))
									 (r (list s1 s2)))
							(if (> s1 s2)
									(push r snakes)
									(push r ladders)))))
		(format t "There are ~a ladders: ~{~%~a~} ~%There are ~a snakes:~{~%~a~}"
						(length ladders) ladders
						(length snakes) snakes)))

(defun snakes-and-ladders ()
	(labels ((max-candidate (gr) (apply #'max (mapcar #'car (cadr gr)))))
		(let* ((board (make-array (list 100) :initial-element nil))
					 (jumps
						(loop for n in (get-inputs)
							 append
								 (loop for s1 = -1 then s2
										for s2 in n
										for dif1 = (- s2 s1)
										for dif = (if (and (< dif1 1) (< (- 99 s2) 6)) (abs dif1) dif1)
										do (setf (aref board s2) s2)
										when (or (> dif 6) (< dif 0))
										collect (list s1 s2))))
					 (jump-groups-unsorted (groupby:groupby #'cadr jumps))
					 (jump-groups (sort jump-groups-unsorted
													 (lambda (j1 j2) (< (max-candidate j1) (max-candidate j2)))))) 
			
			(loop for j in jump-groups
				 for dest = (car j)
				 for n = (let ((m (max-candidate j)))
									 (loop
											for i from 0 to 5
											for s from (1+ m) 
											for r = (if (> s 99) (- 99 (mod s 99)) s)
											when (not (aref board r)) do (return r)))
				 do (setf (aref board n) (list n dest)))
			
			(print-board board))))

(snakes-and-ladders)
