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

(defvar *kill* (make-buffer))
(defvar *yank* nil)
(defvar *last-yank* nil)
(defvar *mark*)

(defun primitive-yank ()
  (aif (buffer-peek *kill*)
       (progn
	 (setf (line) (concat (subline 0 *yank*) it (subline (point)))
	       (point) (+ *yank* (length it)))
	 (redraw-line))
       (beep)))

(defun yank-cycle ()
  (setf *yank* *last-yank*)
  (if (and *yank* (buffer-cycle *kill*))
      (primitive-yank)
      (beep)))

(defun yank ()
  (setf *yank* (point))
  (primitive-yank))

(defun kill-ring-push (string)
  (buffer-push string *kill*))

(defun kill-line ()
  (kill-ring-push (subline (point)))
  (setf (line) (subline 0 (point)))
  (redraw-line))

(defun kill-line-to-bol ()
  (kill-ring-push (subline 0 (point)))
  (setf (line) (subline (point)))
  (setf (point) 0)
  (redraw-line))

(defun copy-region ()
  (when *mark*
    (let ((start (min *mark* (point)))
	  (end (max *mark* (point))))
      (kill-ring-push (subline start end))
      (setf *mark* nil))))

(defun cut-region ()
  (if *mark*
      (let ((start (min *mark* (point)))
	    (end (max *mark* (point))))
	(copy-region)
	(setf (line) (concat (subline 0 start) (subline end))
	      (point) start)
	(redraw-line))))

(defun set-mark ()
  (setf *mark* (unless *mark* (point))))
