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

;;; Mixin that implements undo

(declaim (optimize (debug 3) (safety 3)))

(defclass rewindable ()
  ((rewind-store :reader %rewind-store
		 :initform (make-array 12 :fill-pointer 0 :adjustable t))
   ;; Index is the number of rewinds we've done.
   (rewind-index :accessor %rewind-index
		 :initform 0)))

(defun %rewind-count (rewindable)
  (fill-pointer (%rewind-store rewindable)))

(defun last-state (rewindable)
  (let ((size (%rewind-count rewindable)))
    (if (zerop size)
	(values nil nil)
	(values (aref (%rewind-store rewindable) (1- size)) t))))

(defun save-rewindable-state (rewindable object)
  (let ((index (%rewind-index rewindable))
	(store (%rewind-store rewindable)))
    (unless (zerop index)
      ;; Reverse the tail of pool, since we've
      ;; gotten to the middle by rewinding.
      (setf (subseq store index) (nreverse (subseq store index))))
    (vector-push-extend object store)))

(defmethod rewind-state ((rewindable rewindable))
  (invariant (not (zerop (%rewind-count rewindable))))
  (setf (%rewind-index rewindable) 
	(mod (1+ (%rewind-index rewindable)) (%rewind-count rewindable)))
  (aref (%rewind-store rewindable) 
	(- (%rewind-count rewindable) (%rewind-index rewindable) 1)))
