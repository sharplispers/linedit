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

(defun linedit (&rest keyword-args)
  "Reads a single line of input with line-editing."
  (let ((editor (apply 'make-editor keyword-args)))
    (with-backend editor
      (catch 'linedit-done
	(loop
	 (catch 'linedit-loop
	   (next-chord editor))))
      (redraw-line editor)
      (get-finished-string editor))))

(defun formedit (&rest args &key (prompt1 "") (prompt2 "")
		 &allow-other-keys)
  "Reads a single form of input with line-editing. Returns the form as
a string. Assumes standard readtable."
  (let ((args (copy-list args)))
    (dolist (key '(:prompt1 :prompt2))
      (remf args key))
    (catch 'form-done
      (let ((eof-marker (gensym "EOF"))
	    (table (copy-readtable)))
	;; FIXME: It would be nice to provide an interace of some sort that
	;; the user could use to alter the crucial reader macros in custom readtables.
	(set-macro-character #\: #'colon-reader nil table)
	(set-macro-character #\, (constantly (values)) nil table)
	(set-macro-character #\; #'semicolon-reader nil table)
	(set-dispatch-macro-character #\# #\. (constantly (values)) table)
	(do ((str (apply #'linedit :prompt prompt1 args)
		  (concat str
			  (string #\newline)
			  (apply #'linedit :prompt prompt2 args))))
	    ((let ((form (handler-case (let ((*readtable* table)
					     (*package* (make-package "LINEDIT-SCRATCH")))
					 ;; KLUDGE: This is needed to handle input that starts
					 ;; with an empty line. (At least in the presense of
					 ;; ACLREPL).
					 (unwind-protect
					      (if (find-if-not 'whitespacep str)
						  (read-from-string str)
						  (error 'end-of-file))
					   (delete-package *package*)))
			   (end-of-file () 
			     eof-marker))))
	       (unless (eq eof-marker form)
		 (throw 'form-done str)))))))))

(defun semicolon-reader (stream char)
  (declare (ignore char))
  (loop for char = (read-char stream)
	until (eql char #\newline))
  (values))

(defun colon-reader (stream char)
  (declare (ignore char))
  (read stream t nil t))
