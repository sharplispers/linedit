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

(declaim (type simple-string *word-delimiters*))
(defparameter *word-delimiters* "()[]{}',` \"")
(defvar *prompt*)
(defvar *complete*)
(defvar *insert*)

(defun refresh (&optional (erased 0))
  (write-char #\Return)
  (write-string *prompt*)
  (write-string (line))
  (when (plusp erased)
    (loop repeat erased do (write-char #\Space)))
  (write-char #\Return)
  (write-string *prompt*)
  (write-string (subline 0 (point)))
  (force-output))

(defun word-delimiter-p (char)
  (declare (string *word-delimiters*)
	   (character char))
  (find char *word-delimiters*))

(defun add-string (string &optional (refresh t))
  (setf (line) (concat (subline 0 (point)) string (subline (point))))
  (incf (point) (length string))
  (when refresh
    (refresh)))

(defun beep ()
  (and (write-char #\Bell *error-output*)
       (force-output *error-output*)))

(defun start-of-line ()
  (setf (point) 0)
  (refresh))

(defun end-of-line ()
  (setf (point) (length (line)))
  (refresh))

(defun finish-input ()
  (add-string " ")
  (throw 'return t))

(defun delete-char-backwards ()
  (unless (zerop (point))
    (setf (line) (concat (subline 0 (1- (point))) (subline (point))))
    (decf (point))
    (refresh 1)))

(defun char-right ()
  (when (< (point) (length (line)))
    (incf (point))
    (refresh)))

(defun char-left ()
  (unless (zerop (point))
    (decf (point))
    (refresh)))

(defun interrupt-lisp ()
  (unwind-protect
       (signal-interrupt)
    (redraw-line)))

(defun stop-lisp ()
  (unwind-protect
       (signal-stop)
    (redraw-line)))

(defun redraw-line (&optional line)
  (when line
    (setf (line) line
	  (point) (length line)))
  (write-char #\Return)
  (loop repeat *columns* do (write-char #\Space))
  (refresh))

(defun toggle-insert ()
  (setf *insert* (not *insert*)))

(defun delete-char-forwards-or-eof ()
  (if (equal "" (line))
      (error 'end-of-file :stream *standard-input*)
      (when (< (point) (length (line)))
	(setf (line) (concat (subline 0 (point)) (subline (1+ (point)))))
	(refresh 1))))

(defun word-end ()
  (let ((i (if (and (< (point) (length (line)))
		    (word-delimiter-p (line-char (point))))
	       (position-if-not #'word-delimiter-p (line) :start (point))
	       (point))))
    (if i
	(or (position-if #'word-delimiter-p (line) :start i)
	    (length (line)))
	(point))))
	    
(defun forwards-word ()
  (setf (point) (word-end))
  (refresh))

(defun word-start ()
  (let ((i (if (and (plusp (point))
		    (word-delimiter-p (line-char (1- (point)))))
	       (position-if-not #'word-delimiter-p (line)
				:end (point)
				:from-end t)
	       (point))))
    (if i
	(let ((i2 (position-if #'word-delimiter-p (line)
			       :end i
			       :from-end t)))
	  (if i2
	      (1+ i2)
	      0))
	0)))

(defun in-quoted-string-p ()
  (let ((p (1- (word-start))))
    (and (not (minusp p)) (eql #\" (char (line) p)))))

(defun backwards-word ()
  (setf (point) (word-start))
  (refresh))

(defun delete-word-backwards (&optional (refresh t))
  (let* ((i (word-start))
	 (d (- (point) i)))
    (setf (line) (concat (subline 0 i) (subline (point)))
	  (point) i)
    (when refresh
      (refresh d))))

(defun print-in-columns (list &key width (printer #'princ))
  (declare (function printer))
  (terpri)
  (let ((cols (truncate (/ *columns* width))))
    (do ((item (pop list) (pop list))
	 (i 0 (1+ i))
	 (line 0))
	((null item) (terpri))
      (unless (> cols i)
	(terpri)
	(setf i 0)
	(unless (> *lines* (1+ (incf line)))
	  (setf line 0)
	  (let ((str "-- more --"))
	    (write-string str)
	    (force-output)
	    (let ((q (read-chord)))
	      (wipe-string str)
	      (when (equal q #\q)
		(return-from print-in-columns nil))))))
      (let ((text (with-output-to-string (s)
		    (funcall printer item s))))
	(write-string text)
	(loop repeat (- width (length text))
	      do (write-char #\Space))))))
