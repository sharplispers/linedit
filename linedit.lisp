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

(defun command (char seq)
  (let ((command (gethash (if char
			      (char-code char)
			      seq)
			  *commands*)))
    (declare (type (or null symbol) command))
    (when command      
      (funcall command))
    (unless command
      (when char
	(add-string (string char)))
      (when *debug*
	(format *debug-io* "~&Unrecognized command: ~S."
		(if char (list char (char-code char))
		    seq))
	(force-output *debug-io*))
      (refresh))))

(defun %read-with-editing ()
  (catch 'return
    (save-copy-for-undo)
    (loop
     (psetf *yank* nil
	    *last-yank* *yank*)
     (catch 'undo
       (multiple-value-bind (char escape) (read-chord)
	 (if (and char (graphic-char-p char))
	     (add-string (string char))
	     (command char escape)))
       (save-copy-for-undo)))))
  
(defun linedit (&key
		(prompt "> ")
		(complete 'lisp-complete)
		(insert t)
		(word-delimiters *word-delimiters*)
		(history t)
		&allow-other-keys)
  (declare (optimize (debug 3)))
  (let* ((*mark* nil)
	 (*undo* (make-undo))
	 (*linedit* (make-linedit))
	 (*prompt* prompt)
	 (*insert* insert)
	 (*complete* complete)
	 (*word-delimiters* word-delimiters)
	 (*history* (cond ((eq t history)
			   (setf *history* (or *history* (make-history))))
			  ((eq nil history) (make-history))
			  (t
			   (aif (probe-file history)
				(read-history history)
				(make-history))))))
    (with-termios
      (refresh)
      (%read-with-editing))
    (history-add (line))
    (unless (member history '(nil t))
      (save-history history))
    (line)))

(defun formedit (&rest keyword-args
		 &key (prompt "> ") (prompt2 "| ")
		 &allow-other-keys)
  (declare (optimize (debug 3)))
  (catch 'form
    (let ((eof-marker (cons nil nil))
	  (saved-read-eval *read-eval*)
	  (*read-eval* nil))
      (do ((str (apply #'linedit :prompt prompt keyword-args) 
		(concat str " " (apply #'linedit :prompt prompt2 keyword-args))))
	  ((let ((form (handler-case (read-from-string str)
			(end-of-file () eof-marker))))
	     (unless (eq eof-marker form)
	       (throw 'form (let ((*read-eval* saved-read-eval))
			      (multiple-value-bind (form n)
				  (read-from-string str)
				(values form (subseq str n))))))))
	(terpri)))))
