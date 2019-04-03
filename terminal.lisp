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

(defclass terminal (backend)
  ((translations :initform *terminal-translations*)
   (dirty-p :initform t :accessor dirty-p)))

(defmethod backend-columns ((backend terminal))
  (let ((cols (c-terminal-columns *default-columns*)))
    (if (> cols 0)
        cols
        *default-columns*)))

(defmethod backend-lines ((backend terminal))
  (c-terminal-lines *default-lines*))

(defmethod backend-init ((backend terminal))
  (invariant (not (backend-ready-p backend)))
  (invariant (zerop (c-terminal-init)))
  (setf (backend-ready-p backend) t))

(defmethod backend-close ((backend terminal))
  (invariant (backend-ready-p backend))
  (invariant (zerop (c-terminal-close)))
  (setf (backend-ready-p backend) nil))

;;; FIXME: Use read-char-no-hang to detect pastes, and set an
;;; apropriate flag, or something.
(defmethod read-chord ((backend terminal))
  (invariant (backend-ready-p backend))
  (flet ((read-open-chord ()
	   (do ((chars nil)
		(c #1=(read-char) #1#))
	       ((or (member c '(#\- #\~ #\$))
                    (if (char-equal c #\;)
                        (let ((c1 (read-char))
                              (c2 (read-char)))
                          (push #\; chars)
                          (push c1 chars)
                          (push c2 chars)
                          ;; (format t "add (~A,~A,~A): chars:~A~%"
                          ;; 	c c1 c2 chars)
                          t)))
                (nconc (nreverse chars)
                       (if (char-not-equal c #\;)
                           (list c))))
	     (when (char-not-equal c #\;)
	       (push c chars)))))
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
                                     (when (char-not-equal char #\;)
				       (list char))))))
			 (t (list it)))))
	     (t (if (graphic-char-p it)
		    it
		    (char-code it))))))
      (gethash chord
	       (backend-translations backend)
	       (if (characterp chord)
		   chord
		   (list 'untranslated chord))))))

;;; ASCII 7 should ring the terminal bell. This is hopefully marginally more
;;; robust than #\Bel -- some implementations might eg. call it #\Bell, which
;;; is unicode character in eg. SBCL.
(defconstant +terminal-bell+ (code-char 7))

(defmethod beep ((b terminal))
  (declare (ignore b))
  (and (write-char +terminal-bell+ *error-output*)
       (force-output *error-output*)))

(defmethod page ((backend terminal))
  (write-string "--more--")
  (force-output)
  (let ((q (read-chord backend)))
    (write-char #\Return)
    (not (equal #\q q))))

;;; FIXME: Explicit line-wrap needed
(defmethod print-in-columns ((backend terminal) list &key width)
  (let ((max-col (truncate (backend-columns backend) width))
	(col 0)
	(line 0)
	(pad nil))
    (newline backend)
    (dolist (item list)
      (incf col)
      ;; Padding
      (when pad
	(write-string pad)
	(setf pad nil))
      ;; Item
      (write-string item)
      ;; Maybe newline
      (cond ((= col max-col)
	     (newline backend)
	     (setf col 0)
	     (when (= (1+ (incf line)) (backend-lines backend))
	       (setf line 0)
	       (unless (page backend)
		 (return-from print-in-columns nil))))
	    (t 
	     (setf pad (make-string (- width (length item)) 
				    :initial-element #\space)))))
    ;; Optional newline
    (when pad
      (newline backend))))

(defmethod print-in-lines ((backend terminal) string)
  (newline backend)
  (do ((i 0 (1+ i))
       (lines 0))
      ((= i (length string)))
    (let ((c (schar string i)))
      (when (= lines (backend-lines backend))
	(setf lines 0)
	(unless (page backend)
	  (return-from print-in-lines nil)))
      (when (eql #\newline c)
	(incf lines))
      (write-char c)))
  (newline backend))

(defmethod newline ((backend terminal))
  (setf (dirty-p backend) t)
  (write-char #\newline)
  (write-char #\return)
  (force-output))
