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

(defclass dumb-terminal (backend)
  ((translations :initform *dumb-terminal-translations*)))

(uffi:def-function ("linedit_dumb_terminal_columns" c-dumb-terminal-columns)
    ((default :int))
  :returning :int)

(defmethod backend-columns ((backend dumb-terminal))
  (c-dumb-terminal-columns *default-columns*))

(defmethod line-length-limit ((backend dumb-terminal))
  (backend-columns backend))

(uffi:def-function ("linedit_dumb_terminal_lines" c-dumb-terminal-lines)
    ((default :int))
  :returning :int)

(defmethod backend-lines ((backend dumb-terminal))
  (c-dumb-terminal-lines *default-lines*))

(uffi:def-function ("linedit_dumb_terminal_init" c-dumb-terminal-init)
    ()
  :returning :int)

(defmethod backend-init ((backend dumb-terminal))
  (assert (not (backend-ready-p backend)))
  (assert (zerop (c-dumb-terminal-init)))
  (setf (backend-ready-p backend) t))

(uffi:def-function ("linedit_dumb_terminal_close" c-dumb-terminal-close)
    ()
  :returning :int)

(defmethod backend-close ((backend dumb-terminal))
  (assert (backend-ready-p backend))
  (assert (zerop (c-dumb-terminal-close)))
  (setf (backend-ready-p backend) nil))

(defmethod display ((backend dumb-terminal) prompt line)
  (let ((string (line-string line)))
    (flet ((write-prompt ()
	     (write-char #\Return)
	     (write-string prompt)))
      (write-prompt)
      (write-string string)
      (loop repeat (- (backend-columns backend)
		      (length prompt)
		      (length string))
	    do (write-char #\Space))
      (write-prompt)
      (write-string (subseq string 0 (line-point line)))
      (force-output))))

(defmethod read-chord ((backend dumb-terminal))
  (assert (backend-ready-p backend))
  (flet ((read-open-chord ()
	   (do ((chars nil)
		(c #1=(read-char) #1#))
	       ((member c '(#\- #\~ #\$)) (nconc (nreverse chars) (list c)))
	     (push c chars))))
    (let ((chord
	   (acase (read-char)
		  (#\Esc
		   (cons it (acase (read-char)
				   (#\[ (cons
					 it
					 (let ((char (read-char)))
					   (if (digit-char-p char)
					       (cons char
						     (read-open-chord))
					       (list char)))))
				   (t (list it)))))
		  (t (if (graphic-char-p it)
			 it
			 (char-code it))))))
      (gethash chord
	       (backend-translations backend)
	       (if (characterp chord)
		   chord
		   (list 'untranslated chord))))))

(defmethod beep ((b dumb-terminal))
  (declare (ignore b))
  (and (write-char #\Bell *error-output*)
       (force-output *error-output*)))

(defmethod print-in-columns ((backend dumb-terminal) list &key width)
  (terpri)
  (let ((cols (truncate (backend-columns backend) width)))
    (do ((item #1=(pop list) #1#)
	 (i 0 (1+ i))
	 (line 0))
	((null item))
      (when (= i cols)
	(terpri)
	(setf i 0)
	(when (= (1+ (incf line)) (backend-lines backend))
	  (setf line 0)
	  (write-string "--more--")
	  (force-output)
	  (let ((q (read-chord backend)))
	    (write-char #\Return)
	    (when (equal #\q q)
	      (return-from print-in-columns nil)))))
      (write-string item)
      (loop repeat (- width (length item))
	    do (write-char #\Space))))
  (terpri))

(defmethod newline ((backend dumb-terminal))
  (terpri))
