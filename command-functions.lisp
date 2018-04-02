;;;; Copyright (c) 2003, 2004, 2012 Nikodemus Siivola, Julian Squires
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

;;; These functions are meant to be call throught the command table
;;; of an editor. These functions should not explicitly call refresh, etc:
;;; that is the responsibility of the editor -- but beeping is ok.
;;;
;;; The arguments passed are: CHORD EDITOR

;;; BASIC EDITING

(defun add-char (char editor)
  (with-editor-point-and-string ((point string) editor)
    (setf (get-string editor)
          (concat (subseq string 0 point)
                  (string char)
                  (if (editor-insert-mode editor)
                      (subseq string point)
                      (when (> (length string) (1+ point))
                        (subseq string (1+ point))))))
    (incf (get-point editor))))

(defun delete-char-backwards (chord editor)
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    ;; Can't delegate to editor because of the SUBSEQ index calc.
    (unless (zerop point)
      (setf (get-string editor) (concat (subseq string 0 (1- point))
                                        (subseq string point))
            (get-point editor) (1- point)))))

(defun delete-char-forwards (chord editor)
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    (setf (get-string editor) (concat (subseq string 0 point)
                                      (subseq string (min (1+ point) (length string)))))))

(defun delete-char-forwards-or-eof (chord editor)
  (if (equal "" (get-string editor))
      (error 'end-of-file :stream *standard-input*)
      (delete-char-forwards chord editor)))

(defun delete-word-forwards (chord editor)
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    (let ((i (get-point editor))
          (j (editor-next-word-end editor)))
      (setf (get-string editor)
            (concat (subseq string 0 i) (subseq string j))))))

(defun delete-word-backwards (chord editor)
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    (let ((i (editor-previous-word-start editor)))
      (setf (get-string editor) (concat (subseq string 0 i)
                                        (subseq string point))
            (get-point editor) i))))

(defun finish-input (chord editor)
  (declare (ignore chord editor))
  (throw 'linedit-done t))

;;; CASE CHANGES

(flet ((frob-case (frob editor)
         (with-editor-point-and-string ((point string) editor)
           (let ((end (editor-next-word-end editor)))
             (setf (get-string editor) (concat
                                        (subseq string 0 point)
                                        (funcall frob
                                                 (subseq string point end))
                                        (subseq string end))
                   (get-point editor) end)))))

  (defun upcase-word (chord editor)
    (declare (ignore chord))
    (funcall #'frob-case #'string-upcase editor))

  (defun downcase-word (chord editor)
    (declare (ignore chord))
    (funcall #'frob-case #'string-downcase editor)))

;;; MOVEMENT

(defun move-to-bol (chord editor)
  (declare (ignore chord))
  (setf (get-point editor) 0))

(defun move-to-eol (chord editor)
  (declare (ignore chord))
  (setf (get-point editor) (length (get-string editor))))

(defun move-char-right (chord editor)
  (declare (ignore chord))
  (incf (get-point editor)))

(defun move-char-left (chord editor)
  (declare (ignore chord))
  (decf (get-point editor)))

(defun move-word-backwards (chord editor)
  (declare (ignore chord))
  (setf (get-point editor) (editor-previous-word-start editor)))

(defun move-word-forwards (chord editor)
  (declare (ignore chord))
  (setf (get-point editor) (editor-next-word-end editor)))

;;; UNDO

(defun undo (chord editor)
  (declare (ignore chord))
  (rewind-state editor)
  (throw 'linedit-loop t))

;;; HISTORY

(defun history-previous (chord editor)
  (declare (ignore chord))
  (aif (buffer-previous (get-string editor) (editor-history editor))
       (setf (get-string editor) it)
       (beep editor)))

(defun history-next (chord editor)
  (declare (ignore chord))
  (aif (buffer-next (get-string editor) (editor-history editor))
       (setf (get-string editor) it)
       (beep editor)))

(defvar *history-search* nil)
(defvar *history-needle* nil)

(defun history-search-needle (editor &key direction)
  (let ((text (if *history-search*
                  (cond ((and *history-needle*
                              (member *last-command* '(search-history-backwards
                                                       search-history-forwards)))
                         *history-needle*)
                        (t
                         (setf *history-needle* (get-string editor))))
                  (let* ((*history-search* t)
                         (*aux-prompt* nil))
                    (linedit :prompt "Search History: ")))))
    (when *history-search*
      (setf *aux-prompt* (concat "[" text "] ")))
    text))

(defun history-search (editor direction)
  (let* ((text (history-search-needle editor))
         (history (editor-history editor))
         (test (lambda (old) (search text old)))
         (match (unless (equal "" text)
                  (ecase direction
                    (:backwards
                     (buffer-find-previous-if test history))
                    (:forwards
                     (buffer-find-next-if test history))))))
    (unless match
      (beep editor)
      (setf match text))
    (setf (get-string editor) match
          (get-point editor) (length match))))

(defun search-history-backwards (chord editor)
  (declare (ignore chord))
  (history-search editor :backwards))

(defun search-history-forwards (chord editor)
  (declare (ignore chord))
  (history-search editor :forwards))

;;; KILLING & YANKING

(defun %yank (editor)
  (aif (buffer-peek (editor-killring editor))
       (with-editor-point-and-string ((point string) editor)
         (setf (get-string editor)
               (concat (subseq string 0 (editor-yank editor))
                       it
                       (subseq string point))
               (get-point editor) (+ (editor-yank editor) (length it))))
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
    (setf (get-string editor) (subseq string 0 point))))

(defun kill-to-bol (chord editor)
  ;; Thanks to Andreas Fuchs
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    (buffer-push (subseq string 0 point) (editor-killring editor))
    (setf (get-string editor) (subseq string point)
          (get-point editor) 0)))

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
        (setf (get-string editor) (concat (subseq string 0 start)
                                          (subseq string end))
              (get-point editor) start)))))

(defun set-mark (chord editor)
  (declare (ignore chord))
  ;; FIXME: this was (setf mark (unless mark point)) -- modulo correct
  ;; accessors.  Why? Was I not thinking, or am I not thinking now?
  (setf (editor-mark editor) (get-point editor)))

;;; SEXP MOTION

(defun forward-sexp (chord editor)
  (declare (ignore chord))
  (setf (get-point editor) (editor-sexp-end editor)))

(defun backward-sexp (chord editor)
  (declare (ignore chord))
  (setf (get-point editor) (editor-sexp-start editor)))

;; FIXME: KILL-SEXP is fairly broken, but works for enough of my
;; common use cases.  Most of its flaws lie in how the EDITOR-SEXP-
;; functions deal with objects other than lists and strings.
(defun kill-sexp (chord editor)
  (declare (ignore chord))
  (with-editor-point-and-string ((point string) editor)
    (let ((start (editor-sexp-start editor))
          (end (min (1+ (editor-sexp-end editor)) (length string))))
      (buffer-push (subseq string start end) (editor-killring editor))
      (setf (get-string editor) (concat (subseq string 0 start)
                                        (subseq string end))
            (get-point editor) start))))

(defun close-all-sexp (chord editor)
  (move-to-eol chord editor)
  (do ((string (get-string editor) (get-string editor)))
      ((not (find-open-paren string (length string))))
    (add-char (case (schar string (find-open-paren string (length string)))
                    (#\( #\))
                    (#\[ #\])
                    (#\{ #\}))
              editor)))

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
  (newline editor)
  (format *standard-output* "Unknown command ~S." chord)
  (newline editor))

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
                    (describe (read-from-string (editor-word editor)) s))))

(defun toggle-insert (chord editor)
  (declare (ignore chord))
  (setf (editor-insert-mode editor) (not (editor-insert-mode editor))))
