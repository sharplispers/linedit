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
   (active-string :initform "" :accessor active-string)))

(defun smart-terminal-p ()
  (every 'identity
	 '(ti:cursor-up ti:cursor-down 
	   ti:clr-eos ti:column-address 
	   ti:auto-right-margin ti:enter-am-mode)))

(defmethod backend-init ((backend smart-terminal))
  (call-next-method)
  (ti:tputs ti:enter-am-mode))

(defmethod display ((backend smart-terminal) prompt line point)
  (let ((*terminal-io* *standard-output*)
	(columns (backend-columns backend)))
    (flet ((find-row (n)
	     ;; 1+ includes point in row calculations
	     (ceiling (1+ n) columns))
	   (find-col (n)
	     (rem n columns)))
      (let* ((new (concat prompt line))
	     (old (active-string backend))
	     (end (length new))
	     (rows (find-row end))
	     (start (or (mismatch new old) 0))
	     (start-row (find-row start))
	     (start-col (find-col start)))
	;; Move to start of update and clear to eos
	(ti:tputs ti:column-address start-col)
	(loop repeat (- (point-row backend) start-row)
	    do (ti:tputs ti:cursor-up))
      (ti:tputs ti:clr-eos)
      ;; Write updated segment
      (write-string (subseq new start))
      (when (and (< start end) (zerop (find-col end)))
	(ti:tputs ti:cursor-down))
      ;; Place point
      (let* ((point (+ (length prompt) point))
	     (point-row (find-row point))
	     (point-col (find-col point)))
      (loop repeat (- rows point-row)
	    do (ti:tputs ti:cursor-up))
      (ti:tputs ti:column-address point-col)
      ;; Save state
      (setf (point-row backend) point-row
	    (active-string backend) new))))
    (force-output *terminal-io*)))

