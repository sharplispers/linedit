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

;;; These functions are meant to be call throught the command table
;;; of an editor. These functions should not explicitly call refresh, etc:
;;; that is the responsibility of the editor -- but beeping is ok.
;;;
;;; The arguments passed are: CHORD EDITOR

;;; BASIC EDITING

(defun add-char (char editor)
  (with-editor-point-and-string ((point string) editor)
    (setf (editor-string editor)
	  (concat (subseq string 0 point)
		  (string char)
		  (subseq string point)))
    (incf (editor-point editor))))

(defun delete-char-backwards (chord editor)
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    ;; Can't delegate to editor because of the SUBSEQ index calc.
    (unless (zerop point)
      (setf (editor-string editor)
	    (concat (subseq string 0 (1- point)) (subseq string point))
	    (editor-point editor) (1- point)))))

(defun delete-char-forwards-or-eof (chord editor)
  (declare (ignore chord))
  (if (equal "" (editor-string editor))
      (error 'end-of-file :stream *standard-input*)
      (with-editor-point-and-string ((point string) editor)
	(setf (editor-string editor)
	      (concat (subseq string 0 point) (subseq string (1+ point)))))))

(defun delete-word-backwards (chord editor)
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    (let ((i (editor-word-start editor)))
      (setf (editor-string editor)
	    (concat (subseq string 0 i) (subseq string point))
	    (editor-point editor) i))))

(defun finish-input (chord editor)
  (declare (ignore chord editor))
  (throw 'linedit-done t))

;;; MOVEMENT

(defun move-to-bol (chord editor)
  (declare (ignore chord))
  (setf (editor-point editor) 0))

(defun move-to-eol (chord editor)
  (declare (ignore chord))
  (setf (editor-point editor) (length (editor-string editor))))

(defun move-char-right (chord editor)
  (declare (ignore chord))
  (incf (editor-point editor)))

(defun move-char-left (chord editor)
  (declare (ignore chord))
  (decf (editor-point editor)))

(defun move-word-backwards (chord editor)
  (declare (ignore chord))
  (setf (editor-point editor) (editor-word-start editor)))

(defun move-word-forwards (chord editor)
  (declare (ignore chord))
  (setf (editor-point editor) (editor-word-end editor)))

;;; UNDO

(defun undo (chord editor)
  (declare (ignore chord))
  (setf (editor-line editor) (copy (rewind (undo-pool editor))))
  (throw 'linedit-loop t))

;;; HISTORY

(defun history-previous (chord editor)
  (declare (ignore chord))
  (aif (buffer-previous (editor-string editor) (editor-history editor))
       (setf (editor-string editor) it)
       (beep editor)))

(defun history-next (chord editor) 
  (declare (ignore chord))
  (aif (buffer-next (editor-string editor) (editor-history editor))
       (setf (editor-string editor) it)
       (beep editor)))

;;; KILLING & YANKING

(defun %yank (editor)
  (aif (buffer-peek (editor-killring editor))
       (with-editor-point-and-string ((point string) editor)
	 (setf (editor-string editor)
	       (concat (subseq string 0 (editor-yank editor))
		       it
		       (subseq string point))
	       (editor-point editor) (+ (editor-yank editor) (length it))))
	(beep editor)))

(defun yank (chord editor)
  (declare (ignore chord))
  (remember-yank editor)
  (%yank editor))

(defun yank-cycle (chord editor)
  (declare (ignore chord))
  (if (try-yank editor)
      (progn
	 (buffer-cycle (editor-killring editor))
	 (%yank editor))
      (beep editor)))

(defun kill-to-eol (chord editor)
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    (buffer-push (subseq string point) (editor-killring editor))
    (setf (editor-string editor) (subseq string 0 point))))

(defun kill-to-bol (chord editor)
  ;; Thanks to Andreas Fuchs
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    (buffer-push (subseq string 0 point) (editor-killring editor))
    (setf (editor-string editor) (subseq string point)
	  (editor-point editor) 0)))

(defun copy-region (chord editor)
  (declare (ignore chord))
  (awhen (editor-mark editor)
     (with-editor-point-and-string ((point string) editor)
       (let ((start (min it point))
	     (end (max it point)))
	 (buffer-push (subseq string start end) (editor-killring editor))
	 (setf (editor-mark editor) nil)))))

(defun cut-region (chord editor)
  (declare (ignore chord))
  (awhen (editor-mark editor)
     (with-editor-point-and-string ((point string) editor)
       (let ((start (min it point))
	     (end (max it point)))
	(copy-region t editor)
	(setf (editor-string editor)
	      (concat (subseq string 0 start) (subseq string end))
	      (editor-point editor) start)))))

(defun set-mark (chord editor)
  (declare (ignore chord))
  ;; FIXME: this was (setf mark (unless mark point)) -- modulo correct
  ;; accessors.  Why? Was I not thinking, or am I not thinking now?
  (setf (editor-mark editor) (editor-point editor)))

;;; SIGNALS

(defun interrupt-lisp (chord editor)
  (declare (ignore chord))
  (editor-interrupt editor))

(defun stop-lisp (chord editor)
  (declare (ignore chord))
  (editor-stop editor))

;;; MISCELLANY

(defun help (chord editor)
  (declare (ignore chord))
  (let ((pairs nil)
	(max-id 0)
	(max-f 0))
    (maphash (lambda (id function)
	       (let ((f (string-downcase (symbol-name function))))
		 (push (list id f) pairs)
		 (setf max-id (max max-id (length id))
		       max-f (max max-f (length f)))))
	     (editor-commands editor))
    (print-in-columns editor
		      (mapcar (lambda (pair)
				 (destructuring-bind (id f) pair
				   (with-output-to-string (s)
				     (write-string id s)
				     (loop repeat (- (1+ max-id) (length id))
					   do (write-char #\Space s))
				     (write-string f s))))
			      (nreverse pairs))
		      :width (+ max-id max-f 2))))

(defun unknown-command (chord editor)
  (format *error-output*
	  "~&Unknown command ~S.~%"
	  chord))

(defun complete (chord editor)
  (declare (ignore chord))
  (multiple-value-bind (completions max-len) (editor-complete editor)
    (if completions
	(if (not (cdr completions))
	    (editor-replace-word editor (car completions))
	    (print-in-columns editor completions :width (+ max-len 2)))
	(beep editor))))

(defun apropos-word (chord editor)
  (declare (ignore chord))
  (let* ((word (editor-word editor))
	 (apropi (apropos-list word)))
    (if (null apropi)
	(beep editor)
	(let* ((longest 0)
	       (strings (mapcar (lambda (symbol)
				  (declare (symbol symbol))
				  (let ((str (prin1-to-string symbol)))
				    (setf longest (max longest (length str)))
				    (string-downcase str)))
				apropi)))
	  (print-in-columns editor strings :width (+ longest 2))))))

(defun describe-word (chord editor)
  (declare (ignore chord))
  (print-in-lines editor
		  (with-output-to-string (s)
		    (describe (find-symbol (string-upcase
					    (editor-word editor))) s))))
