;; Copyright (c) 2003, 2011 Nikodemus Siivola <nikodemus@random-state.net>
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

#-sbcl
(error "Attempt to load an SBCL-specific file in another implementation.")

(let (prompt-fun read-form-fun)
  (declare (type (or null function) prompt-fun read-form-fun))

  (macrolet ((enforce-consistent-state ()
	       `(invariant (or (and prompt-fun read-form-fun)
			       (not (or prompt-fun read-form-fun))))))

    (defun uninstall-repl ()
      (enforce-consistent-state)
      (if prompt-fun
	  (setf sb-int:*repl-prompt-fun* prompt-fun
		sb-int:*repl-read-form-fun* read-form-fun
		prompt-fun nil
		read-form-fun nil)
	  (warn "UNINSTALL-REPL failed: No Linedit REPL present."))
      nil)

    (defun install-repl (&key wrap-current eof-quits history killring)
      (enforce-consistent-state)
      (when prompt-fun
	(warn "INSTALL-REPL failed: Linedit REPL already installed.")
	(return-from install-repl nil))
      (setf prompt-fun sb-int:*repl-prompt-fun*
	    read-form-fun sb-int:*repl-read-form-fun*)
      (flet ((repl-reader (in out)
	       (declare (type stream out)
			(ignore in))
	       (fresh-line out)
	       (let ((prompt (with-output-to-string (s)
			       (funcall prompt-fun s))))
		 (handler-case
		     (linedit:formedit
		      :prompt1 prompt
		      :prompt2 (make-string (length prompt)
					    :initial-element #\Space)
                      :history history
                      :killring killring)
		   (end-of-file (e)
		     (if eof-quits
			 (and (fresh-line) (eof-handler "SBCL" #'sb-ext:quit))
			 ;; Hackins, I know.
			 "#.''end-of-file"))))))
	(setf sb-int:*repl-prompt-fun* (constantly ""))
	(setf sb-int:*repl-read-form-fun*
	      (if wrap-current
		  (lambda (in out)
		    (declare (type stream out in))
		      ;; FIXME: Yich.
		    (terpri)
		    (with-input-from-string (in (repl-reader in out))
		      (funcall read-form-fun in out)))
		  (lambda (in out)
		    (declare (type stream out in))
		    (handler-case (read-from-string (repl-reader in out))
		      (end-of-file ()
			;; We never get here if eof-quits is true, so...
			(fresh-line)
			(write-line "#<end-of-file>")
			(values)))))))
      t)))
