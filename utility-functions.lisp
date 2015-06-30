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
		       (append open-args '(:if-exists :append
                                           :if-does-not-exist :create)))))

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

(defun yes-or-no (control &rest args)
  "Like Y-OR-N-P, but using linedit functionality."
  ;; Don't save the query response.
  (let ((*history* nil)
        (*killring* nil))
    (loop
      (let ((result (linedit :prompt (format nil "~? (y or n) " control args))))
        (cond
          ((zerop (length result)))
          ((char-equal (elt result 0) #\y)
           (return-from yes-or-no t))
          ((char-equal (elt result 0) #\n)
           (return-from yes-or-no nil)))
        (format *terminal-io* "Please type \"y\" for yes or \"n\" for no.~%")
        (finish-output *terminal-io*)))))

(defun eof-handler (lisp-name quit-fn)
  (handler-case
      (cond ((yes-or-no "Really quit ~A?" lisp-name)
             (fresh-line)
             (funcall quit-fn))
            (t
             (return-from eof-handler "#.''end-of-file")))
    (end-of-file ()
      (fresh-line)
      (funcall quit-fn))))
