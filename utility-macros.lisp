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

(defmacro aif (condition consequent &optional alternative)
  `(let ((it ,condition))
     (if it
	 ,consequent
	 ,alternative)))

(defmacro acase (form &rest cases)
  `(let ((it ,form))
     (case it
       ,@cases)))

(defmacro with-unique-names ((&rest bindings) &body body)
  `(let ,(mapcar #'(lambda (binding)
                     (destructuring-bind (var prefix)
			 (if (consp binding) binding (list binding binding))
                       `(,var (gensym ,(string prefix)))))
                 bindings)
     ,@body))

(defmacro awhen (condition &body body)
  `(aif ,condition
	(progn ,@body)))

(defmacro do-internal-symbols ((var package) &body forms)
  (with-unique-names (state)
    `(do-symbols (,var ,package)
       (multiple-value-bind (,var ,state)
	   (find-symbol (symbol-name ,var) ,package)
	 (when (eq ,state :internal)
	   ,@forms)))))

(defmacro invariant (condition)
  (with-unique-names (value)
    `(let ((,value ,condition))
       (unless ,value
	 (let ((*print-pretty* nil))
	   (error "BUG: You seem to have found a bug in Linedit. Please report ~
                   this incident along with directions to reproduce and the ~
                   following message to linedit-devel@common-lisp.net:~
                   ~
                     `Invariant ~S violated.'"
		  ',condition))))))

(defmacro ensure (symbol expr)
  `(or ,symbol (setf ,symbol ,expr)))

(defmacro dbg-values (&rest places)
  `(when *debug*
     (format *debug* ,(apply #'concatenate 'string 
			     (mapcar (lambda (x)
				       (format nil "~A = ~~A, " x))
				     places))
	     ,@places)
     (terpri *debug*)
     (force-output *debug*)))


