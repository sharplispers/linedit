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

(defvar *commands* (make-hash-table :test #'equalp))

(defmacro defkey (id &optional action)
  (when action
    (let ((chord (gethash id *chords*)))
      (assert chord)
      `(setf (gethash ',chord *commands*) ,action))))

(defun help ()
  (let ((pairs nil)
	(max-id 0)
	(max-f 0))
    (maphash (lambda (id chord)
	       (let ((function (gethash chord *commands*)))
		 (when function
		   (let ((f (string function)))
		     (push (list id f) pairs)
		     (setf max-id (max max-id (length id))
			   max-f (max max-f (length f)))))))
	     *chords*)
    (print-in-columns (reverse pairs)
		      :width (+ max-id max-f 3)
		      :printer
		      (lambda (pair stream)
			(destructuring-bind (id f) pair
			  (write-string id stream)
			    (loop repeat (- (1+ max-id) (length id))
				  do (write-char #\Space stream))
			    (write-string f stream))))))

(defkey "C-Space" 'set-mark)
(defkey "C-A" 'start-of-line)
(defkey "C-B" 'char-left)
(defkey "C-C" 'interrupt-lisp)
(defkey "C-D" 'delete-char-forwards-or-eof)
(defkey "C-E" 'end-of-line)
(defkey "C-F" 'char-right)
(defkey "C-G")
(defkey "C-Backspace" 'delete-word-backwards)
(defkey "Tab" 'autocomplete)
(defkey "C-J")
(defkey "C-K" 'kill-line)
(defkey "C-L" 'redraw-line)
(defkey "Return" 'finish-input)
(defkey "C-N")
(defkey "C-O")
(defkey "C-P")
(defkey "C-Q")
(defkey "C-R")
(defkey "C-S")
(defkey "C-T")
(defkey "C-U" 'kill-line-to-bol)
(defkey "C-V")
(defkey "C-W" 'cut-region)
(defkey "C-X")
(defkey "C-Y" 'yank)
(defkey "C-Z" 'stop-lisp)
(defkey "C--" 'undo)
(defkey "Backspace" 'delete-char-backwards)

(defkey "M-A")
(defkey "M-B" 'backwards-word)
(defkey "M-C")
(defkey "M-D")
(defkey "M-E")
(defkey "M-F" 'forwards-word)
(defkey "M-G")
(defkey "M-H" 'help)
(defkey "M-I")
(defkey "M-J")
(defkey "M-K")
(defkey "M-L")
(defkey "M-M")
(defkey "M-N")
(defkey "M-O")
(defkey "M-P")
(defkey "M-Q")
(defkey "M-R")
(defkey "M-S")
(defkey "M-T")
(defkey "M-U")
(defkey "M-V")
(defkey "M-W" 'copy-region)
(defkey "M-X")
(defkey "M-Y" 'yank-cycle)
(defkey "M-Z")
(defkey "M-1")
(defkey "M-2")
(defkey "M-3")
(defkey "M-4")
(defkey "M-5")
(defkey "M-6")
(defkey "M-7")
(defkey "M-8")
(defkey "M-9")
(defkey "M-0")

(defkey "Up-arrow" 'history-previous)
(defkey "Down-arrow" 'history-next)
(defkey "Right-arrow" 'char-right)
(defkey "Left-arrow" 'char-left)
(defkey "Insert" 'toggle-insert)
(defkey "Delete" 'delete-char-forwards)
(defkey "C-Delete")
(defkey "Page-up")
(defkey "Page-down")
(defkey "Home" 'start-of-line)
(defkey "End" 'end-of-line)
