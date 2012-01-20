;; Copyright (c) 2010 Nikodemus Siivola, Julian Squires
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

#-ccl
(error "Attempt to load a CCL-specific file in another implementation.")

#+nil (defun toplevel-read (&key (input-stream *standard-input*)
                           (output-stream *standard-output*)
                           (prompt-function #'print-listener-prompt)
                           (eof-value *eof-value*)
		           (map nil))
  (force-output output-stream)
  (funcall prompt-function output-stream)
  (read-toplevel-form input-stream :eof-value eof-value :map map))


;;; Problems with this implementation presently:
;;;  - not well tested outside of simple usage;
;;;  - doesn't wrap prompt (ccl::print-listener-prompt) like sbcl-repl;
;;;  - does not honor variables like ccl::*verbose-eval-selection*;
;;;  - if wrap-current, should feed formedit output to original read-toplevel-form;
;;;  - uninstall-repl/install-repl don't behave exactly like sbcl-repl's fns.
(let (original-rtf original-quiet-flag)
  (defun uninstall-repl ()
    (when original-rtf
      (ccl:remove-method #'ccl::read-toplevel-form
			 (find (find-class 'ccl:input-stream)
			       (ccl:generic-function-methods #'ccl::read-toplevel-form)
			       :key (lambda (x) (first (ccl:method-specializers x)))))
      (ccl:add-method #'ccl::read-toplevel-form original-rtf)
      (setf ccl::*quiet-flag* original-quiet-flag
	    original-rtf nil)
      t))

  (defun install-repl (&key wrap-current eof-quits history killring)
    (when original-rtf
      (warn "INSTALL-REPL failed: Linedit REPL already installed.")
      (return-from install-repl nil))
    (setf original-rtf (find (find-class 'ccl:input-stream)
			 (ccl:generic-function-methods #'ccl::read-toplevel-form)
			 :key (lambda (x) (first (ccl:method-specializers x))))
	  original-quiet-flag ccl::*quiet-flag*
	  ccl::*quiet-flag* t)
    ;; Per CCL comments: READ-TOPLEVEL-FORM should return 3 values: a
    ;; form, a (possibly null) pathname, and a boolean that indicates
    ;; whether or not the result(s) of evaluating the form should be
    ;; printed.
    (defmethod ccl::read-toplevel-form ((in ccl:input-stream) &key eof-value file-name &allow-other-keys)
      (flet ((repl-reader ()
	       (let ((prompt (with-output-to-string (s)
			       ;; KLUDGE: Gross, but *quiet-flag* can't be bound.
			       (setf ccl::*quiet-flag* nil)
			       (ccl::print-listener-prompt s)
			       (setf ccl::*quiet-flag* t))))
		 (handler-case (linedit:formedit :prompt1 prompt
						 :prompt2 (make-string (length prompt)
								       :initial-element #\Space)
                                                 :history history
                                                 :killring killring)
		   (end-of-file ()
		     (if eof-quits
			 (and (fresh-line) (eof-handler "CCL" #'ccl:quit))
			 "#.''end-of-file"))))))
	(if wrap-current
	    (with-input-from-string (s (repl-reader))
	      (funcall (ccl:method-function original) s))
	    (values (read-from-string (repl-reader)) file-name t))))
    t))
