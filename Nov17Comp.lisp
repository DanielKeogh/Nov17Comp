;;;; Nov17Comp.lisp

(in-package #:Nov17Comp)
(ql:quickload :cl-strings)
(ql:quickload :groupby)

(defun de-interleave (s)
	(loop
		 for i from 0
		 for x in s
		 when (= 0 (mod i 2))
		 collect x))

(defun get-inputs ()
	(with-open-file (f "~/src/nov17comp/task4-linux.dat")
		(loop for l = (read-line f nil)
			 while l for s = (mapcar (lambda (q) (1- (parse-integer q))) (cl-strings:split l))
			 collect (de-interleave s)
			 collect (de-interleave (cdr s)))))

(defun print-board (board)
	(loop for i from 0 to 99
		 when (= 0 (mod i 10)) do (fresh-line)
		 do (format t "~a "(aref board i))))

(defun snakes-and-ladders ()
	(let* ((board (make-array (list 100) :initial-element nil))
				 (jumps
					(loop for n in (get-inputs)
						 append
							 (loop for s1 = 0 then s2
									for s2 in n
									for dif = (- s2 s1)
									if (and (< dif 6) (>= dif 0) (not (numberp (aref board s2))))
									do (setf (aref board s2) s2)
									else collect (list s1 s2)))))

		(loop for i from 0 to 99
			 when (not (aref board i))
			 do (let* ((s (groupby:groupby (lambda (n) n)
																		 (remove-if-not
																			(lambda (p)
																				(and (< (- i 6) (car p))
																						 (> i (car p))))
																			jumps)))
								 (biggest-count 0)
								 (num 0))
						
						(loop for n in s
							 for cnt = (length (cdr n))
							 when (> cnt biggest-count)
							 do (progn
										(setf biggest-count cnt)
										(setf num n)))

						(setf (aref board i) num)))
		(print-board board)))
