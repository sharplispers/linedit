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

(declaim (type simple-string *word-delimiters*))
(defparameter *word-delimiters* "()[]{}',` \"")

(defvar *debug* nil)

(defun required ()
  (error "Required argument missing."))

(defun concat (&rest strings)
  (apply #'concatenate 'simple-string strings))

(defun word-delimiter-p (char)
  (declare (simple-string *word-delimiters*)
	   (character char))
  (find char *word-delimiters*))

(defun make-whitespace (n)
  (make-string n :initial-element #\space))

(defun whitespacep (char)
  (member char '(#\space #\newline #\tab #\return #\page)))

(defun at-delimiter-p (string index)
  (and (< index (length string)) 
       (word-delimiter-p (char string index))))

(defun start-debug (pathname &rest open-args)
  "Start linedit debugging output to pathname, with additional
open-args passed to `open'."
  (setf *debug* (apply #'open pathname 
		       :direction :output 
		       (append open-args '(:if-exists :append)))))

(defun end-debug ()
  "End linedit debugging output."
  (close *debug*)
  (setf *debug* nil))

(defun dbg (format-string &rest format-args)
  (when *debug*
    (apply #'format *debug* format-string format-args)
    (finish-output *debug*)))

(defun min* (&rest args)
  "Like min, except ignores NILs."
  (apply #'min (remove-if #'null args)))

(defun meta-escape (string)
  (declare (simple-string string))
  (let (stack)
    (loop with last
	  for i from 1 upto (length string)
	  for char across string
	  ;; KLUDGE: Deal with character literals. Not quite sure this is
	  ;; the right and robust way to do it, though.
	  when (and (eql #\\ char) (not (eql #\# last)))
	  do (push #\\ stack)
	  do (push char stack)
	     (setf last char))
    (coerce (nreverse stack) 'simple-string)))
