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

(in-package :linedit-util)

(defvar *debug* t)

(defmacro acase (form &rest cases)
  `(let ((it ,form))
     (case it
       ,@cases)))

(defmacro aif (condition consequent &optional alternate)
  `(let ((it ,condition))
     (if it ,consequent ,alternate)))

(defmacro awhen (condition &body body)
  `(let ((it ,condition))
     (when it
       ,@body)))

(defmacro with-unique-names ((&rest bindings) &body body)
  `(let ,(mapcar #'(lambda (binding)
                     (destructuring-bind (var prefix)
			 (if (consp binding) binding (list binding binding))
                       `(,var (gensym ,(string prefix)))))
                 bindings)
     ,@body))

(defun debug (&rest args)
  (let ((*package* (load-time-value *package*)))
    (funcall #'print args *debug-io*)
    (terpri *debug-io*)
    (force-output *debug-io*)))

(defun concat (&rest strings)
  (apply #'concatenate 'string strings))

(defun wipe-string (string)
  (write-char #\Return)
  (loop repeat (length string)
	do (write-char #\Space))
  (write-char #\Return))
