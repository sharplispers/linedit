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
(defvar *word-delimiters* "()[]{}',` ")
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

(defun redraw-line (&optional line)
  (when line
    (setf (line) line
	  (point) (length line)))
  (write-char #\Return)
  (loop repeat *columns* do (write-char #\Space))
  (refresh))

(defun toggle-insert ()
  (setf *insert* (not *insert*)))

(defun delete-char-forwards ()
  (when (< (point) (length (line)))
    (setf (line) (concat (subline 0 (point)) (subline (1+ (point)))))
    (refresh 1)))

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
	    (read-chord)
	    (wipe-string str))))
      (let ((text (with-output-to-string (s)
		    (funcall printer item s))))
	(write-string text)
	(loop repeat (- width (length text))
	      do (write-char #\Space))))))

(defun autocomplete ()
  (let ((string (subline (word-start) (point))))
    (multiple-value-bind (completions max-len) (funcall *complete* string)
      (if completions
	  (cond ((= 1 (length completions))
		 (delete-word-backwards nil)
		 (add-string (first completions)))
		(t
		 (print-in-columns completions
				   :width (+ max-len 2)
				   :printer #'write-string)
		 (refresh)))
	  (beep)))))

(defun lisp-complete (string)
  (declare (simple-string string))
  (let* ((length (length string))
	 (colon (position #\: string))
	 (hash (make-hash-table :test #'equal))
	 (common nil)
	 (max 0))
    (flet ((stringify (symbol)
	     (if (upper-case-p (schar string 0))
		 (string symbol)
		 (string-downcase (string symbol))))
	   (push-name (name)
	     (unless (equal string name)
	       (setf common (if common
				(subseq name 0 (mismatch common name))
				name)
		     max (max max (length name))
		     (gethash name hash) name))))
      (when (plusp length)
	(if colon
	    (let* ((i (1+ colon))
		   (n (- length i)))
	      (do-external-symbols
		  (sym (find-package
			(if (plusp colon)
			    (string-upcase (subseq string 0 colon))
			    :keyword)))
		(let ((name (stringify sym)))
		  (when (and (>= (length name) n)
			     (equal (subseq string i) (subseq name 0 n)))
		    (push-name (concatenate
				'string string (subseq name n)))))))
	    (dolist (package (list-all-packages))
	      (if (eq *package* package)
		  (do-symbols (sym)
		    (let ((name (stringify sym)))
		      (when (and (>= (length name) length)
				 (equal string (subseq name 0 length)))
			(push-name name))))
		  (dolist (name (cons (package-name package)
				      (package-nicknames package)))
		    (let ((name (stringify name)))
		      (when (and (>= (length name) length)
				 (equal string (subseq name 0 length)))
			(push-name name))))))))
      (if (or (null common)
	      (equal common string))
	  (let (list)
	    (maphash (lambda (key val)
		       (declare (ignore val))
		       (push key list))
		     hash)
	    (values list max))
	  (values (list common) (length common))))))

