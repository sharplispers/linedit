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

(in-package :linedit-termios)

(defparameter *default-columns* 80)
(defparameter *default-lines* 24)

(defvar *termios* nil)

(define-alien-variable "linedit_OK" int)
(define-alien-variable "linedit_NOT_ATTY" int)
(define-alien-variable "linedit_MEMORY_ERROR" int)
(define-alien-variable "linedit_TCGETATTR_ERROR" int)
(define-alien-variable "linedit_TCSETATTR_ERROR" int)
(define-alien-variable "linedit_ATTR_ERROR" int)
(define-alien-variable "linedit_NO_ATTR_ERROR" int)

(defmacro termios-error (fun format-string &rest format-args)
  `(error (format nil "Termios error in ~A: ~?" ,fun ,format-string ,format-args)))

(defmacro termios-call (function caller)
  `(if *termios*
       (ecase (funcall ,function)
	 (,linedit-ok t)
	 (,linedit-not-atty        (termios-error ,caller "Not a TTY."))
	 (,linedit-memory-error    (termios-error ,caller "Out of memory."))
	 (,linedit-tcgetattr-error (termios-error ,caller "Error in tcagetattr."))
	 (,linedit-tcsetattr-error (termios-error ,caller "Error in tcasetattr."))
         (,linedit-attr-error      (termios-error ,caller "Unexpected saved termios state."))
         (,linedit-no-attr-error   (termios-error ,caller "No saved termios state.")))
       (termios-error ,caller "Used outside termios scope.")))

(define-alien-routine "linedit_save_termios" int)
(define-alien-routine "linedit_restore_termios" int)
(define-alien-routine "linedit_keyboard_mode" int)

(defun save-termios ()
  (termios-call #'linedit-save-termios 'save-termios))

(defun restore-termios ()
  (termios-call #'linedit-restore-termios 'restore-termios))

(defun keyboard-mode ()
  (termios-call #'linedit-keyboard-mode 'keyboard-mode))

(defmacro with-termios (&body forms)
  (with-unique-names (termios-error)
    `(progn
       (when *termios*
	 (error "Nested WITH-TERMIOS contexts."))
       (let ((*termios* t)
	     (,termios-error nil))
	 (unwind-protect
	      (handler-case (progn
			      (save-termios)
			      (keyboard-mode)
			      ,@forms)
		(error (e)
		  (clear-input)
		  (setf ,termios-error e)))
	   (restore-termios))
	 (when ,termios-error
	   (error ,termios-error))))))

(defvar *chord-terminators* (list #\~ #\$))

;; FIXME: Using READ-CHAR is definitely unportable. Urgh.

(defun read-open-chord ()
  (let (chars)
    (do ((c (read-char) (read-char)))
	((member c *chord-terminators*) (nconc (nreverse chars) (list c)))
      (print c)
      (push c chars))))

(defun read-chord ()
  (if *termios*      
      (acase (read-char)
	     (#\Esc
	      (values nil
		      (cons it (acase (read-char)
				      (#\[ (cons
					    it
					    (let ((char (read-char)))
					      (if (digit-char-p char)
						  (cons char
							(read-open-chord))
						  (list char)))))
				      (t (list it))))))
	     (t (values it nil)))
      (error "READ-CHORD outside WITH-TERMIOS scope.")))

(defun get-columns ()
  (let ((cols (alien-funcall (extern-alien "linedit_columns" (function int)))))
    (if (plusp cols)
	cols
	(aif (sb-ext:posix-getenv "COLUMNS")
	     (parse-integer it)
	     *default-columns*))))

(define-symbol-macro *columns* (get-columns))

(defun get-lines ()
  (let ((lines (alien-funcall (extern-alien "linedit_lines" (function int)))))
    (if (plusp lines)
	lines
	(aif (sb-ext:posix-getenv "LINES")
	     (parse-integer it)
	     *default-lines*))))

(define-symbol-macro *lines* (get-lines))
