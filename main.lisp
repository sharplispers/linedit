(in-package :linedit)

(defun linedit (&rest keyword-args)
  (let* ((editor (apply 'make-instance 'editor keyword-args))
	 (backend (editor-backend editor)))
    (with-backend backend      
      (catch 'linedit-done
	(loop
	 (catch 'linedit-loop
	   (next-chord editor))))
      (get-finished-string editor))))

(defun formedit (&rest args &key (prompt1 "") (prompt2 "")
		 &allow-other-keys)
  (let ((args (copy-list args)))
    (dolist (key '(:prompt1 :prompt2))
      (remf args key))
    (catch 'form-done
      (let ((eof-marker (list nil))
	    (table (copy-readtable *readtable*)))
	(set-dispatch-macro-character #\# #\. (constantly (values)) table)
	(do ((str (apply #'linedit :prompt prompt1 args)
		  (concat str
			  (string #\Newline)
			  (apply #'linedit :prompt prompt2 args))))
	    ((let ((form (handler-case (let ((*readtable* table))
					 (read-from-string str))
			   (end-of-file () eof-marker))))
	       (unless (eq eof-marker form)
		 (throw 'form-done str)))))))))
