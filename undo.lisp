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

(defstruct undo
  (pool (make-array 48 :fill-pointer 0 :adjustable t :element-type 'linedit))
  (index 0))

(declaim (type undo *undo*))
(defvar *undo*)

(defun save-copy-for-undo ()
  (let* ((pool (undo-pool *undo*))
	 (size (fill-pointer pool))
	 (i (undo-index *undo*)))
    (declare (type (array linedit *) pool))
    (unless (zerop i)
      ;; We've gotten here by undoing, so we need to
      ;; reorder the states we've visited.
      (do ((i0 i (1+ i0))
	   (i1 (1- size) (1- i1)))
	  ((>= i0 i1))
	(psetf (aref pool i0) (aref pool i1)
	       (aref pool i1) (aref pool i0))))
    (unless (and (plusp size)
		 (equal (line) (linedit-string (aref pool (1- size)))))
      (vector-push-extend (copy-linedit *linedit*) pool))))

(defun undo ()
  (let* ((pool (undo-pool *undo*))
	 (size (fill-pointer pool))
	 (index (undo-index *undo*)))
    (declare (type (array linedit *) pool))
    (setf (undo-index *undo*) (mod (1+ index) size)
	   *linedit* (copy-linedit (aref pool (- size (undo-index *undo*) 1))))
    (redraw-line)
    (throw 'undo t)))

