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

(defvar *history* nil)

(defun history-previous ()
  (aif (buffer-previous (line) *history*)
       (redraw-line it)
       (beep)))

(defun history-next ()
  (aif (buffer-next (line) *history*)
       (redraw-line it)
       (beep)))

(defun history-add (string)
  (buffer-push string *history*))  

(defun make-history ()
  (make-buffer))

(let ((history-file-ident ";; -*- Lisp -*- LINEDIT HISTORY FILE"))

  (defun read-history (pathspec)
    (with-standard-io-syntax 
      (let ((*read-eval* nil)
	    (history nil))
	(with-open-file (f pathspec)
	  (assert (equal history-file-ident (read-line f)))
	  (setf history (read f))
	  (assert (null (read f nil nil))))
	history)))
    
  (defun save-history (pathspec)  
    (ensure-directories-exist pathspec)
    (with-standard-io-syntax
      (with-open-file (f pathspec
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
	(write-line history-file-ident f)
	(prin1 *history* f)
	(terpri f)))))
  