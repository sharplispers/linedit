;; Copyright (c) 2003 Nikodemus Siivola
;; 
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :linedit)

(defun autocomplete ()
  (let ((string (subline (word-start) (point))))
    (multiple-value-bind (completions max-len) (funcall *complete* string)
      (if completions
	  (cond ((= 1 (length completions))
		 (delete-word-backwards nil)
		 (add-string (first completions)))
		(t
		 (print-in-columns completions
				   :width (+ max-len 2)
				   :printer #'write-string)
		 (refresh)))
	  (beep)))))

;;This version of DIRECTORY-COMPLETE is much nicer, but requires
;;OSICAT, which I haven't yet released. Hence the interim version
;;below. Maybe I should just put some foriegn glue in for this as well.
#+nil (defun directory-complete (string)
  (declare (simple-string string))
  (labels ((real-dir (path)
	     (let ((dir (pathname-directory path)))
	       (cond ((probe-directory path) (probe-directory path))
		     ((null (cdr dir))
		      (pathname (case (car dir)
				  (:absolute "/")
				  (t "./"))))
		     (t
		      (let ((path (make-pathname
				   :directory (butlast dir)
				   :name nil)))
			(or (probe-directory path) (real-dir path))))))))
    (let* ((dir (real-dir (concat string "/")))
	   (namefun (case (car (pathname-directory dir))
		      (:absolute #'namestring)
		      (t #'enough-namestring)))
	   (common nil)
	   (max 0)
	   (hash (make-hash-table :test 'equal)))
      (dolist (path (directory-list dir))
	(let* ((candidate (funcall namefun path))
	       (diff (mismatch string candidate)))
	  (unless (or (not diff)
		      (< diff (length string)))
	    (setf common (if common
			     (subseq candidate 0 (mismatch common candidate))
			     candidate)
		  max (max max (length candidate))
		  (gethash candidate hash) candidate))))
      (if (or (null common)
	      (equal common string))
	  (let (list)
	    (maphash (lambda (key val)
		       (declare (ignore val))
		       (push key list))
		     hash)
	    (values list max))
	  (values (list common) (length common))))))

;; This version of directory complete isn't nice to symlinks, but
;; it's better than nothing while waiting for Osicat.
(defun directory-complete (string)  
  (declare (simple-string string))
  (let ((namefun (case (car (pathname-directory string))
		   (:absolute #'namestring)
		   (t #'enough-namestring)))
	(common nil)
	(max 0)
	(hash (make-hash-table :test 'equal)))
    (dolist (path (directory (concat string "*")))
      (let* ((candidate (funcall namefun path))
	     (diff (mismatch string candidate)))
	(unless (or (not diff)
		    (< diff (length string)))
	  (setf common (if common
			   (subseq candidate 0 (mismatch common candidate))
			   candidate)
		max (max max (length candidate))
		(gethash candidate hash) candidate))))
    (if (or (null common)
	    (<= (length common) (length string)))
	(let (list)
	  (maphash (lambda (key val)
		     (declare (ignore val))
		     (push key list))
		   hash)
	  (values list max))
	(values (list common) (length common)))))

(defun lisp-complete (string)
  (declare (simple-string string))
  (if (or (in-quoted-string-p)
	  (eql #\/ (char string 0)))
      (directory-complete string)
      (let* ((length (length string))
	     (first-colon (position #\: string))
	     (last-colon (position #\: string :from-end t))
	     (state (and first-colon
			 (if (< first-colon last-colon)
			     :internal
			     :external)))
	     (package (and first-colon
			   (find-package (if (plusp first-colon)
					     (string-upcase
					      (subseq string 0 first-colon))
					     :keyword))))
	     (hash (make-hash-table :test #'equal))
	     (common nil)
	     (max 0))
	(flet ((stringify (symbol)
		 (if (upper-case-p (schar string 0))
		     (string symbol)
		     (string-downcase (string symbol))))
	       (push-name (name)
		 (unless (equal string name)
		   (setf common (if common
				    (subseq name 0 (mismatch common name))
				    name)
			 max (max max (length name))
			 (gethash name hash) name))))
	  (when (plusp length)
	    (if package
		(let* ((i (1+ last-colon))
		       (n (- length i)))
		  (labels ((select-symbol-aux (symbol)
			     (let ((name (stringify symbol)))
			       (when (and (>= (length name) n)
					  (equal (subseq string i)
						 (subseq name 0 n)))
				 (push-name (concat string (subseq name n))))))
			   (select-symbol (symbol &optional qualifier)
			     (if qualifier
				 (multiple-value-bind (s state)
				     (find-symbol (symbol-name symbol) package)
				   (declare (ignore s))
				   (when (eq qualifier state)
				     (select-symbol-aux symbol)))
				 (select-symbol-aux symbol))))
		    (ecase state
		      (:internal (do-symbols (sym package)
				   (select-symbol sym :internal)))
		      (:external (do-external-symbols (sym package)
				   (select-symbol sym))))))
		(dolist (package (list-all-packages))
		  (if (eq *package* package)
		      (do-symbols (sym)
			(let ((name (stringify sym)))
			  (when (and (>= (length name) length)
				     (equal string (subseq name 0 length)))
			    (push-name name))))
		      (dolist (name (cons (package-name package)
					  (package-nicknames package)))
			(let ((name (stringify name)))
			  (when (and (>= (length name) length)
				     (equal string (subseq name 0 length)))
			    (push-name name))))))))
	  (if (or (null common)
		  (equal common string))
	      (let (list)
		(maphash (lambda (key val)
			   (declare (ignore val))
			   (push key list))
			 hash)
		(values list max))
	      (values (list common) (length common)))))))
