;; Copyright (c) 2003, 2004 Nikodemus Siivola
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

(defparameter *default-columns* 80)
(defparameter *default-lines* 24)
(defparameter *highlight-color* nil
  "Color to use for highlighting parentheses. NIL is the current foreground
color bolded, other options are terminal colors :BLACK, :RED, :GREEN, :YELLOW,
:BLUE, :MAGENTA, :CYAN, and :WHITE.")

(defclass backend ()
  ((ready-p :accessor backend-ready-p :initform nil)
   (translations :reader backend-translations)
   (start :initform 0 :accessor get-start)))

(defmacro with-backend (backend &body forms)
  (with-unique-names (an-error)
    `(let ((,an-error nil))
       (unwind-protect
	    (handler-case (progn
			    (backend-init ,backend)
			    ,@forms)
	      (error (e)
		(setf ,an-error e)))
	 (backend-close ,backend)
	 (awhen ,an-error
	   (error it))))))

(defmacro without-backend (backend &body forms)
  `(unwind-protect
	(progn
	  (backend-close ,backend)
	  ,@forms)
     (backend-init ,backend)))

(defgeneric display (backend &key prompt line point &allow-other-keys))

