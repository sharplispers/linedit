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

(defclass smart-terminal (terminal)
  ((point-row :initform 1 :accessor point-row)
   (point-col :initform 0 :accessor point-col)
   (active-string :initform "" :accessor active-string)
   (markup-start :initform 0 :accessor get-markup-start)))

(defun set-column-address (n current)
  (if nil
      (ti:tputs ti:column-address n)
      (cond ((< n current)
	     (loop repeat (- current n) 
		   do (ti:tputs ti:cursor-left)))
	    ((> n current)
	     (loop repeat (- n current) 
		   do (ti:tputs ti:cursor-right))))))

(defun smart-terminal-p ()
  (and ti:cursor-up ti:cursor-down ti:clr-eos
       (or ti:column-address (and ti:cursor-left ti:cursor-right))
       (or ti:auto-right-margin ti:enter-am-mode)))

(defmethod backend-init ((backend smart-terminal))
  (call-next-method)
  (when ti:enter-am-mode
    (ti:tputs ti:enter-am-mode)))

(defun find-row (n columns)
  ;; 1+ includes point in row calculations
  (ceiling (1+ n) columns))

(defun find-col (n columns)
  (rem n columns))

(defun move-up-in-column (&key col up clear-to-eos current-col)
  (set-column-address col current-col)
  (loop repeat up do (ti:tputs ti:cursor-up))
  (when clear-to-eos
    (ti:tputs ti:clr-eos)))

(defun fix-wraparound (start end columns)
  ;; If final character ended in the last column the point
  ;; will wrap around to the first column on the same line:
  ;; hence move down if so.
  (when (and (< start end) (zerop (find-col end columns)))
    (ti:tputs ti:cursor-down)))

(defun place-point (&key up col)
  (loop repeat up do (ti:tputs ti:cursor-up))
  (ti:tputs ti:column-address col))

(defmethod display ((backend smart-terminal) &key prompt line point markup)
  (let* ((*terminal-io* *standard-output*)
	 (columns (backend-columns backend))
	 (old-markup-start (get-markup-start backend))
	 (old-col (point-col backend)))
    (multiple-value-bind (marked-line markup-start)
	(if markup
	    (dwim-mark-parens line point 
			      :pre-mark ti:enter-bold-mode
			      :post-mark ti:exit-attribute-mode)
	    (values line point))
	(let* ((new (concat prompt marked-line))
	       (old (active-string backend))
	       (end (+ (length prompt) (length line))) ;; based on unmarked
	       (rows (find-row end columns))
	       (start (min0 markup-start old-markup-start (mismatch new old)))
	       (start-row (find-row start columns))
	       (start-col (find-col start columns))
	       (point* (+ point (length prompt)))
	       (point-row (find-row point* columns))
	       (point-col (find-col point* columns)))
	  (move-up-in-column
	   :col start-col 
	   :up (- (point-row backend) start-row)
	   :clear-to-eos t
	   :current-col old-col)
	  (write-string (subseq new start))
	  (fix-wraparound start end columns)
	  (move-up-in-column 
	   :col point-col
	   :up (- rows point-row)
	   :current-col (find-col end columns))
	  ;; Save state
	  (setf (point-row backend) point-row
		(point-col backend) point-col
		(active-string backend) (concat prompt line)
		(get-markup-start backend) markup-start)
	  (force-output *terminal-io*)))))
