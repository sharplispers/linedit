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

;; This version of directory-complete isn't nice to symlinks, and
;; should be replaced by something backed by foreign glue.
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

(defun lisp-complete (string editor)
  (declare (simple-string string))
  (when (plusp (length string))
    (if (in-quoted-string-p editor)
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
