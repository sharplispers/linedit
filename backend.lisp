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

(defparameter *default-columns* 80)
(defparameter *default-lines* 24)

(defclass backend ()
  ((ready-p :accessor backend-ready-p :initform nil)
   (translations :reader backend-translations)))

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
