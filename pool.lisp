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

;;; A pool of states. states can be added to the pool, last one
;;; retrieved, or pool rewound.
;;;
;;; Used to implement undo.

(defclass pool ()
  ((store :reader %pool-store
	  :initform (make-array 12 :fill-pointer 0 :adjustable t))
   ;; Index is the number of rewinds we've done.
   (index :accessor %pool-index
	  :initform 0)))

(defun %pool-size (pool)
  (fill-pointer (%pool-store pool)))

(defun last-insert (pool)
  (let ((size (%pool-size pool)))
    (unless (zerop size)
      (aref (%pool-store pool) (1- size)))))

(defun insert (object pool)
  (let ((i (%pool-index pool))
	(store (%pool-store pool)))
    (unless (zerop i)
      ;; Reverse the tail of pool, since we've
      ;; gotten to the middle by rewinding.
      (setf (subseq store i) (nreverse (subseq store i))))
    (vector-push-extend object store)))

(defun rewind (pool)
  (setf (%pool-index pool) (mod (1+ (%pool-index pool)) (pool-size pool)))
  (aref (%pool-store pool) (- (%pool-size pool) (%pool-index pool) 1)))
