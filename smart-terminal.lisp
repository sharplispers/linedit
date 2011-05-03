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
  ((old-point :initform 0 :accessor old-point)
   (old-string :initform "" :accessor old-string)
   (old-markup :initform 0 :accessor old-markup)))

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

(defun move-in-column (&key col vertical clear-to-eos current-col)
  (set-column-address col current-col)
  (if (plusp vertical)
      (loop repeat vertical do (ti:tputs ti:cursor-up))
      (loop repeat (abs vertical) do (ti:tputs ti:cursor-down)))
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

(defun paren-style ()
  (concat
   (when *highlight-color*
     (ti:tparm
      ti:set-a-foreground
      (or (position *highlight-color* '(:black :red :green :yellow :blue :magenta :cyan :white))
          (error "Unknown color: ~S" *highlight-color*))))
   ti:enter-bold-mode))

(defmethod display ((backend smart-terminal) &key prompt line point markup)
  (let* (;; SBCL and CMUCL traditionally point *terminal-io* to /dev/tty,
         ;; and we do output on it assuming it goes to STDOUT. Binding
         ;; *terminal-io* is unportable, so do it only when needed.
         #+(or sbcl cmu)
           (*terminal-io* *standard-output*)
	 (columns (backend-columns backend))
	 (old-markup (old-markup backend))
	 (old-point (old-point backend))
	 (old-col (find-col old-point columns))
	 (old-row (find-row old-point columns))
	 (old (old-string backend))
	 (new (concat prompt line))
	 (end (length new))
	 (rows (find-row end columns)))
    (when (dirty-p backend)
      (setf old-markup 0
	    old-point 0
	    old-col 0
	    old-row 1))
    (multiple-value-bind (marked-line markup)
	(if markup
	    (dwim-mark-parens line point
			      :pre-mark (paren-style)
			      :post-mark ti:exit-attribute-mode)
	    (values line point))
	(let* ((full (concat prompt marked-line))
	       (point (+ point (length prompt)))
	       (point-row (find-row point columns))
	       (point-col (find-col point columns))
	       (diff (mismatch new old))
	       (start (min* old-point point markup old-markup diff end))
	       (start-row (find-row start columns))
	       (start-col (find-col start columns)))
	  (dbg "---~%")
	  (dbg-values (subseq new start))
	  (dbg-values rows point point-row point-col start start-row start-col
		      old-point old-row old-col end diff)
	  (move-in-column
	   :col start-col 
	   :vertical (- old-row start-row)
	   :clear-to-eos t
	   :current-col old-col)
	  (write-string (subseq full start))
	  (fix-wraparound start end columns)
	  (move-in-column 
	   :col point-col
	   :vertical (- rows point-row)
	   :current-col (find-col end columns))
	  ;; Save state
	  (setf	(old-string backend) new
		(old-markup backend) markup
		(old-point backend) point
		(dirty-p backend) nil)))
    (force-output *terminal-io*)))
