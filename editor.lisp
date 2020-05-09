;;;; Copyright (c) 2003, 2004 Nikodemus Siivola, Julian Squires
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :linedit)

(defparameter *version* (asdf:component-version (asdf:find-system :linedit)))

(defvar *history* nil)
(defvar *killring* nil)

(defclass editor (line rewindable)
  ((commands :reader editor-commands
	     :initform *commands*
	     :initarg :commands)
   (completer :reader editor-completer
	      :initform 'lisp-complete
	      :initarg :complete)
   (history :accessor editor-history)
   (killring :accessor editor-killring)
   (insert :accessor editor-insert-mode
	   :initform t
	   :initarg :insert-mode)
   (mark :accessor editor-mark
	 :initform nil)
   (yank :accessor editor-yank
	 :initform nil)
   (last-yank :accessor editor-last-yank
	      :initform nil)
   (prompt :accessor editor-prompt
	   :initform ""
	   :initarg :prompt)))

(defmethod initialize-instance :after ((editor editor) &rest initargs &key history killring)
  (let ((history (ensure-buffer (or history *history*))))
    (unless *history*
      (setf *history* history))
    (setf (editor-history editor) history))
  (let ((killring (ensure-buffer (or killring *killring*))))
    (unless *killring*
      (setf *killring* killring))
    (setf (editor-killring editor) killring))
  (save-state editor))

(defclass smart-editor (editor smart-terminal) ())
(defclass dumb-editor (editor dumb-terminal) ())

(defvar *announced* nil)

(defun make-editor (&rest args &key quiet &allow-other-keys)
  (ti:set-terminal)
  (let* ((type (if (smart-terminal-p)
                   'smart-editor
                   'dumb-editor))
         (spec (list *version* type)))
    (unless quiet
      (unless (equal *announced* spec)
        (format t "~&Linedit version ~A, ~A mode, ESC-h for help.~%"
                *version*
                (if (eq 'smart-editor type)
                    "smart"
                    "dumb"))
        (setf *announced* spec)))
    (remf args :quiet)
    (apply 'make-instance type args)))

;;; undo

(defun save-state (editor)
  (let ((string (get-string editor))
	(last (last-state editor)))
    (unless (and last (equal string (get-string last)))
      ;; Save only if different than last saved state
      (save-rewindable-state editor
			     (make-instance 'line
					    :string (copy-seq string)
					    :point (get-point editor))))))

(defmethod rewind-state ((editor editor))
  (let ((line (call-next-method)))
    (setf (get-string editor) (copy-seq (get-string line))
	  (get-point editor) (get-point line))))

(defvar *debug-info* nil)

(defvar *aux-prompt* nil)

(defun redraw-line (editor &key markup)
  (display editor
	   :prompt (concat (editor-prompt editor) *aux-prompt*)
	   :line (get-string editor)
	   :point (get-point editor)
	   :markup markup))

(defvar *last-command* nil)

(defun next-chord (editor)
  (redraw-line editor :markup t)
  (forget-yank editor)
  (let* ((chord (read-chord editor))
	 (command (gethash chord (editor-commands editor)
			   (if (characterp chord)
			       'add-char
			       'unknown-command))))
    (setf *debug-info* (list command chord editor))
    (funcall command chord editor)
    (setf *last-command* command))
  (save-state editor))

(defun get-finished-string (editor)
  (buffer-push (get-string editor) (editor-history editor))
  (newline editor)
  (get-string editor))

(defmacro with-editor-point-and-string (((point string) editor) &body forms)
  `(let ((,point (get-point ,editor))
	 (,string (get-string ,editor)))
     ,@forms))

(defun editor-interrupt (editor)
  (without-backend editor
    ;; On CCL, the signal isn't delivered before this function
    ;; returns, which leads to blindness at the next prompt, or worse.
    #+ccl (ccl::interactive-abort)
    #-ccl (osicat-posix:kill 0 osicat-posix:sigint)))

(defun editor-stop (editor)
  (without-backend editor (osicat-posix:kill 0 osicat-posix:sigtstp)))

(defun editor-word-start (editor)
  "Returns the index of the first letter of current or previous word,
if the point is just after a word, or the point."
  (with-editor-point-and-string ((point string) editor)
    (if (or (not (at-delimiter-p string point))
	    (not (and (plusp point) (at-delimiter-p string (1- point)))))
	(1+ (or (position-if 'word-delimiter-p string :end point :from-end t)
		-1)) ; start of string
	point)))

(defun editor-previous-word-start (editor)
  "Returns the index of the first letter of current or previous word,
if the point was at the start of a word or between words."
  (with-editor-point-and-string ((point string) editor)
    (let ((tmp (cond ((at-delimiter-p string point)
		      (position-if-not 'word-delimiter-p string
				       :end point :from-end t))
		     ((and (plusp point) (at-delimiter-p string (1- point)))
		      (position-if-not 'word-delimiter-p string
				       :end (1- point) :from-end t))
		     (t point))))
      ;; tmp is always in the word whose start we want (or NIL)
      (1+ (or (position-if 'word-delimiter-p string
			   :end (or tmp 0) :from-end t)
	      -1)))))

(defun editor-word-end (editor)
  "Returns the index just beyond the current word or the point if
point is not inside a word."
  (with-editor-point-and-string ((point string) editor)
    (if (at-delimiter-p string point)
	point
	(or (position-if 'word-delimiter-p string :start point)
	    (length string)))))

(defun editor-next-word-end (editor)
  "Returns the index just beyond the last letter of current or next
word, if the point was between words."
  (with-editor-point-and-string ((point string) editor)
    (let ((tmp (if (at-delimiter-p string point)
		   (or (position-if-not 'word-delimiter-p string
					:start point)
		       (length string))
		   point)))
      ;; tmp is always in the word whose end we want (or already at the end)
      (or (position-if 'word-delimiter-p string :start tmp)
	  (length string)))))

(defun editor-word (editor)
  "Returns the current word the point is in or right after, or an
empty string."
  (let ((start (editor-word-start editor))
	(end (editor-word-end editor)))
    (dbg "~&editor-word: ~S - ~S~%" start end)
    (subseq (get-string editor) start end)))

(defun editor-sexp-start (editor)
  (with-editor-point-and-string ((point string) editor)
    (setf point (loop for n from (min point (1- (length string))) downto 0
		      when (not (whitespacep (schar string n)))
		      return n))
    (case (and point (schar string point))
      ((#\) #\] #\}) (or (find-open-paren string point) 0))
      ((#\( #\[ #\{) (max (1- point) 0))
      (#\" (or (find-open-quote string point)
	       (max (1- point) 0)))
      (t (editor-previous-word-start editor)))))

(defun editor-sexp-end (editor)
  (with-editor-point-and-string ((point string) editor)
    (setf point (loop for n from point below (length string)
		      when (not (whitespacep (schar string n)))
		      return n))
    (case (and point (schar string point))
      ((#\( #\[ #\{) (or (find-close-paren string point)
			 (length string)))
      ((#\) #\] #\}) (min (1+ point) (length string)))
      (#\" (or (find-close-quote string (1+ point))
	       (min (1+ point) (length string))))
      (t (editor-next-word-end editor)))))

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
  (quoted-p (get-string editor) (get-point editor)))
