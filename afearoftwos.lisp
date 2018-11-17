(loop for i from 1 to 1000000
	 when (not (find #\2 (format nil "~%~d" i)))
	 do (format t "~%~d" i))
