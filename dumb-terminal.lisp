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

;;; The simplest Linedit backend, that copes only with single lines
;;; of limited length.

(defclass dumb-terminal (terminal) ())

(defmethod display ((backend dumb-terminal) &key prompt line point &allow-other-keys)
  (let* ((string (concat prompt line))
	 (length (length string))
	 (point (+ point (length prompt)))
	 (columns (backend-columns backend)))
    (write-char #\return)
    (cond ((< (1+ point) columns) 
	   (write-string (subseq string 0 (min length columns)))
	   (when (< length columns)
	     (write-string (make-whitespace (- columns length))))
	   (write-char #\return)
	   (write-string (subseq string 0 point)))
	  (t
	   (write-string (subseq string (- (1+ point) columns) point))
	   (write-char #\return)
	   (write-string (subseq string (- (1+ point) columns) point)))))
  (force-output))
