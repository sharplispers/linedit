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

(defvar *version* #.(symbol-name 
		     (with-open-file (f (merge-pathnames "version.lisp-expr"
							 *compile-file-truename*))
		       (read f))))

(defvar *history* nil)
(defvar *killring* nil)

(defclass editor (line rewindable)
  ((commands :reader editor-commands
	     :initform *commands*
	     :initarg :commands)
   (completer :reader editor-completer
	      :initform 'lisp-complete
	      :initarg :complete)
   (history :reader editor-history
	    :initform (or *history* (setf *history* (make-instance 'buffer)))
	    :initarg :history)
   (killring :reader editor-killring
	     :initform (or *killring* (setf *killring* (make-instance 'buffer)))
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

(defmethod initialize-instance :after ((editor editor) &rest initargs)
  (save-state editor))

(defclass smart-editor (editor smart-terminal) ())
(defclass dumb-editor (editor dumb-terminal) ())

(let ((ann nil))
  (defun make-editor (&rest args)
    (ti:set-terminal)
    (let ((type (if (smart-terminal-p)
		    'smart-editor
		    'dumb-editor)))
      (unless ann
	(format t "~&Linedit version ~A [~A mode]~%" *version* (if (eq 'smart-editor type)
								   "smart"
								   "dumb")))
      (setf ann t)
      (apply 'make-instance type args))))

;;; undo

(defun save-state (editor)
  (let ((string (get-string editor))
	(last (last-state editor)))
    (unless (and last (equal string (get-string last)))
      ;; Save only if different than last saved state
      (save-rewindable-state editor (make-instance 'line
						   :string (copy-seq string) 
						   :point (get-point editor))))))

(defmethod rewind-state ((editor editor))
  (let ((line (call-next-method)))
    (setf (get-string editor) (copy-seq (get-string line))
	  (get-point editor) (get-point line))))

(defvar *debug-info* nil)

(defun redraw-line (editor &key markup)
  (display editor 
	   :prompt (editor-prompt editor) 
	   :line (get-string editor) 
	   :point (get-point editor)
	   :markup markup))

(defun next-chord (editor)
  (redraw-line editor :markup t)
  (forget-yank editor)
  (let* ((chord (read-chord editor))
	 (command (gethash chord (editor-commands editor)
			   (if (characterp chord)
			       'add-char
			       'unknown-command))))
    (setf *debug-info* (list command chord editor))
    (funcall command chord editor))
  (save-state editor))

(defun get-finished-string (editor)
  (buffer-push (get-string editor) (editor-history editor))
  (newline editor)
  (get-string editor))

(defmacro with-editor-point-and-string (((point string) editor) &body forms)
  `(let ((,point (get-point ,editor))
	 (,string (get-string ,editor)))
     ,@forms))

(uffi:def-function ("linedit_interrupt" c-interrupt)
    ()
  :returning :void)

(defun editor-interrupt (editor)
  (without-backend editor (c-interrupt)))

(uffi:def-function ("linedit_stop" c-stop)
    ()
  :returning :void)

(defun editor-stop (editor)
  (without-backend editor (c-stop)))

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
    (subseq (get-string editor) start end)))

(defun editor-complete (editor)
  (funcall (editor-completer editor) (editor-word editor) editor))

(defun remember-yank (editor)
  (setf (editor-yank editor) (get-point editor)))

(defun forget-yank (editor)
  (shiftf (editor-last-yank editor) (editor-yank editor) nil))

(defun try-yank (editor)
  (setf (editor-yank editor) (editor-last-yank editor))
  (editor-yank editor))

(defun editor-replace-word (editor word)
  (with-editor-point-and-string ((point string) editor)
    (let ((start (editor-word-start editor))
	  (end (editor-word-end editor)))
      (setf (get-string editor)
	    (concat (subseq string 0 start) word (subseq string end))
	    (get-point editor) (+ start (length word))))))

(defun in-quoted-string-p (editor)
  (let ((i (editor-word-start editor)))
    (and (plusp i) (eql #\" (schar (get-string editor) (1- i))))))
