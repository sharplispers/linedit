(require :asdf)
(require :linedit)

(when (linedit:tty-p)
  (let ((prompt-fun sb-int:*repl-prompt-fun*))
    (declare (type function prompt-fun))
    (setf sb-int:*repl-prompt-fun* (constantly "")
	  sb-int:*repl-read-form-fun*
	  (lambda (in out)
	    (declare (type stream out)
		     (ignore in))
	    (fresh-line out)
	    (let ((prompt (with-output-to-string (s) 
			    (funcall prompt-fun s))))
	      (handler-case 
		  (linedit:formedit
		   :prompt prompt
		   :prompt2 (make-string (length prompt) 
					 :initial-element #\Space))
		(end-of-file () (quit))))))))
