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

(defvar *history* nil)

(defclass editor ()
  ((undo-pool :reader undo-pool :initform (make-instance 'pool))
   (line :reader editor-line :initform (make-instance 'line))
   (backend :reader editor-backend
	    :initform (make-instance 'dumb-terminal)
	    :initarg :backend)
   (commands :reader editor-commands
	     :initform *commands*
	     :initarg :commands)
   (completer :reader editor-completer
	      :initform 'lisp-complete
	      :initarg :complete)
   (history :reader editor-history
	    :initform (or *history* (setf *history* (make-instance 'buffer)))
	    :initarg :history)
   (killring :reader editor-killring
	     :initform (make-instance 'buffer)
	     :initarg :killring)
   (insert :reader editor-insert-mode
	   :initform t
	   :initarg :insert-mode)
   (mark :accessor editor-mark
	 :initform nil)
   (yank :accessor editor-yank
	 :initform nil)
   (last-yank :accessor editor-last-yank
	      :initform nil)
   (prompt :reader editor-prompt
	   :initform ""
	   :initarg :prompt)))

(defun save-line-for-undo (editor)
  (let ((pool (undo-pool editor))
	(line (editor-line editor)))
    (unless (equal? line (last-insert pool))
      ;; Save only if different than last saved state.
      (insert (copy line) pool))))

(defvar *debug-info* nil)

(defun next-chord (editor)
  (display (editor-backend editor)
	   (editor-prompt editor)
	   (editor-line editor))
  (forget-yank editor)
  (let* ((chord (read-chord (editor-backend editor)))
	 (command (gethash chord (editor-commands editor)
			   (if (characterp chord)
			       'add-char
			       'unknown-command))))
    (setf *debug-info* (list command chord editor))
    (funcall command chord editor))
  (save-line-for-undo editor))

(defun editor-string (editor)
  (line-string (editor-line editor)))

(defun (setf editor-string) (string editor)
  (let ((limit (line-length-limit (editor-backend editor))))
    (if (and limit (>= (length string) limit))
	(progn
	  (beep editor)
	  (throw 'linedit-loop t))
	(setf (line-string (editor-line editor)) string))))

(defun (setf editor-line) (line editor)
  (setf (slot-value editor 'line) line))

(defun editor-point (editor)
  (line-point (editor-line editor)))

(defun (setf editor-point) (point editor)
  (setf (line-point (editor-line editor)) point))

(defun get-finished-string (editor)
  (buffer-push (editor-string editor) (editor-history editor))
  (newline (editor-backend editor))
  (editor-string editor))

(defmacro with-editor-point-and-string (((point string) editor) &body forms)
  `(let ((,point (editor-point ,editor))
	 (,string (editor-string ,editor)))
     ,@forms))

(defmethod beep ((editor editor))
  (beep (editor-backend editor)))

(uffi:def-function ("linedit_interrupt" c-interrupt)
    ()
  :returning :void)

(defun editor-interrupt (editor)
  (without-backend (editor-backend editor) (c-interrupt)))

(uffi:def-function ("linedit_stop" c-stop)
    ()
  :returning :void)

(defun editor-stop (editor)
  (without-backend (editor-backend editor) (c-stop)))

(defun editor-word-start (editor)
  (with-editor-point-and-string ((point string) editor)
    ;; Find the first point backwards that is NOT a word-start
    (let ((non-start (if (and (plusp point)
			      (word-delimiter-p (char string (1- point))))
			 (position-if-not 'word-delimiter-p string
					  :end point
					  :from-end t)
			 point)))
    (or (when non-start
	  ;; Find the first word-start before that.
	  (let ((start (position-if 'word-delimiter-p string
				    :end non-start
				    :from-end t)))
	    (when start	(1+ start))))
	0))))

(defun editor-word-end (editor)
  (with-editor-point-and-string ((point string) editor)
    ;; Find the first point forwards that is NOT a word-end
    (let ((non-end (if (and (< point (length string))
			    (word-delimiter-p (char string point)))
		       (position-if-not 'word-delimiter-p string :start point)
		       point)))
      (if non-end
	  ;; Find the first word-end after that
	  (or (position-if 'word-delimiter-p string :start non-end)
	      (length string))
	  point))))

(defun editor-word (editor)
  (let ((start (editor-word-start editor))
	(end (editor-word-end editor)))
    (subseq (editor-string editor) start end)))

(defun editor-complete (editor)
  (funcall (editor-completer editor) (editor-word editor) editor))

(defun remember-yank (editor)
  (setf (editor-yank editor) (editor-point editor)))

(defun forget-yank (editor)
  (shiftf (editor-last-yank editor) (editor-yank editor) nil))

(defun try-yank (editor)
  (setf (editor-yank editor) (editor-last-yank editor))
  (editor-yank editor))

(defun editor-replace-word (editor word)
  (with-editor-point-and-string ((point string) editor)
    (let ((start (editor-word-start editor))
	  (end (editor-word-end editor)))
      (setf (editor-string editor)
	    (concat (subseq string 0 start) word (subseq string end))
	    (editor-point editor) (+ start (length word))))))

(defmethod print-in-columns ((editor editor) list &key width)
  (print-in-columns (editor-backend editor) list :width width))

(defmethod print-in-lines ((editor editor) string)
  (print-in-lines (editor-backend editor) string))

(defun in-quoted-string-p (editor)
  (let ((i (editor-word-start editor)))
    (and (plusp i) (eql #\" (schar (editor-string editor) (1- i))))))
