(dotimes (i 1000000)
	(if (not (find #\2 (format nil "~%~d" (1+ i))))
			(format t "~%~d" (1+ i))))
