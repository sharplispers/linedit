;;;; Copyright (c) 2003, 2004 Nikodemus Siivola, Julian Squires
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :linedit)

(defun pathname-directory-pathname (pathname)
  (make-pathname :name nil :type nil
		 :defaults pathname))

(defun underlying-directory-p (pathname)
  (case (file-kind pathname)
    (:directory t)
    (:symbolic-link 
     (file-kind (merge-pathnames (read-link pathname) pathname)))))

(defun logical-pathname-p (pathname)
  (typep (pathname pathname) 'logical-pathname))

(defun logical-pathname-complete (string)
  (values (list string) (length string)))

#+nil
(defun logical-pathname-complete (string)
  (let* ((host (pathname-host string))
	 (rest (subseq string (1+ (mismatch host string))))
	 (rules (remove-if-not (lambda (rule)
				 (mismatch rest (first rule)))))
	 (physicals (mapcar (lambda (rule)
			      (namestring 
			       (translate-pathname string 
						   (first rule)
						   (second rule))))
			    rules))
	 (matches (apply #'append (mapcar #'directory-complete physicals)))
	 (logicals (mapcar (lambda (physical)
			     (let ((rule (find-if (lambda (rule)
						    (misma

  (flet ((maybe-translate-logical-pathname (string)
	   (handler-case
	       (translate-logical-pathname string)
	     (error () 
	       (return-from logical-pathname-complete (values nil 0))))))
    (directory-complete 
     (namestring 
      (maybe-translate-logical-pathname string)))))
    ;; FIXME: refactor chared code with directory complete
    (loop with all
	  with common
	  with max
	  for cand in matches
	  do (let ((diff (mismatch string cand)))
	       (when (and diff (> diff (length string)))
		   (setf common (if common 
				    (subseq common 0 (mismatch common cand))
				    cand)
			 max (max max (length cand))
			 all (cons cand all))))
	  finally (if (or (null common)
			  (<= (length common) (length string)))
		      (return (values all max))
		      (return (values (list common) (length common))))))))))))))))

;;; We can't easily do zsh-style tab-completion of ~us into ~user, but
;;; at least we can expand ~ and ~user.  The other bug here at the
;;; moment is that ~nonexistant will complete to the same as ~.
(defun tilde-expand-string (string)
  "Returns the supplied string, with a prefix of ~ or ~user expanded
to the appropriate home directory."
  (if (and (> (length string) 0)
	   (eql (schar string 0) #\~))
      (flet ((chop (s) 
	       (subseq s 0 (1- (length s)))))
	(let* ((slash-index (loop for i below (length string)
				  when (eql (schar string i) #\/) 
				  return i))
	       (suffix (and slash-index (subseq string slash-index)))
	       (uname (subseq string 1 slash-index))
	       (homedir (or (cdr (assoc :home (user-info uname)))
			    (chop (namestring 
				   (or (probe-file (user-homedir-pathname))
				       (return-from tilde-expand-string 
					 string)))))))
	  (concatenate 'string homedir (or suffix ""))))
      string))

(defun directory-complete (string)
  (declare (simple-string string))
  (let* ((common nil)
	 (all nil)
	 (max 0)
	 (string (tilde-expand-string string))
	 (dir (pathname-directory-pathname string))
	 (namefun (if (relative-pathname-p string)
		      #'namestring
		      (lambda (x) (namestring (merge-pathnames x))))))
    (unless (and (underlying-directory-p dir)
		 (not (wild-pathname-p dir)))
      (return-from directory-complete (values nil 0)))
    (with-directory-iterator (next dir)
      (loop for entry = (next)
	    while entry
	    do (let* ((full (funcall namefun entry))
		      (diff (mismatch string full)))
		 (dbg "~& completed: ~A, diff: ~A~%" full diff)
		 (unless (and diff (< diff (length string)))
		   (dbg "~& common ~A mismatch ~A~&" common 
			(mismatch common full))
		   (setf common (if common
				    (subseq common 0 (mismatch common full))
				    full)
			 max (max max (length full))
			 all (cons full all))))))
    (dbg "~&common: ~A~%" common)
    (if (or (null common)
	    (<= (length common) (length string)))
	(values all max)
	(values (list common) (length common)))))

(defun lisp-complete (string editor)
  (declare (simple-string string))
  (when (plusp (length string))
    (if (in-quoted-string-p editor)
	(if (logical-pathname-p string)
	    (logical-pathname-complete string)
	    (directory-complete string))
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
	       (max-len 0))
       
	  (labels ((stringify (symbol)
		     (if (upper-case-p (schar string 0))
			 (string symbol)
			 (string-downcase (string symbol))))
		   (push-name (name)
		     (setf common (if common
				      (subseq name 0 (mismatch common name))
				      name)
			   max-len (max max-len (length name))
			   (gethash name hash) name))
		   (select-symbol (symbol match)
		     (let ((name (stringify symbol))
			   (end (length match)))
		       (when (and (> (length name) end)	; Skip indetical
				  (equal match (subseq name 0 end)))
			 (push-name (concat string (subseq name end)))))))
	    ;; Skip empty strings
	    (when (plusp length)
	      (if package
		  ;; Symbols with explicit package prefixes.
		  (let* ((start (1+ last-colon))
			 (match (subseq string start)))
		    (ecase state
		      (:internal (do-internal-symbols (sym package)
				   (select-symbol sym match)))
		      (:external (do-external-symbols (sym package)
				   (select-symbol sym match)))))
		
		  ;; Symbols without explicit package prefix + packges
		  (dolist (package (list-all-packages))
		    (if (eq *package* package)
			(do-symbols (sym)
			  (select-symbol sym string))
			;; Package names
			(dolist (name (cons (package-name package)
					    (package-nicknames package)))
			  (select-symbol name string))))))

	    ;; Return list of matches to caller
	    (if (> (length common) (length string))
		(values (list common) (length common))
		(let (list)
		  (maphash (lambda (key val)
			     (declare (ignore val))
			     (push key list))
			   hash)
		  (values list max-len))))))))
