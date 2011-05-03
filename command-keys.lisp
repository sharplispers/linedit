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

(defvar *commands* (make-hash-table :test #'equalp))

(defmacro defcommand (command &optional action)
  (when action
    `(setf (gethash ,command *commands*) ,action)))

(defcommand "C-A" 'move-to-bol)
(defcommand "C-B" 'move-char-left)
(defcommand "C-C" 'interrupt-lisp)
(defcommand "C-D" 'delete-char-forwards-or-eof)
(defcommand "C-E" 'move-to-eol)
(defcommand "C-F" 'move-char-right)
(defcommand "C-G")
(defcommand "C-J")
(defcommand "C-K" 'kill-to-eol)
(defcommand "C-L")
(defcommand "C-N" 'history-next)
(defcommand "C-O" 'close-all-sexp)
(defcommand "C-P" 'history-previous)
(defcommand "C-Q")
(defcommand "C-R" 'search-history-backwards)
(defcommand "C-S" 'search-history-forwards)
(defcommand "C-T")
(defcommand "C-U" 'kill-to-bol)
(defcommand "C-V")
(defcommand "C-W" 'cut-region)
(defcommand "C-X")
(defcommand "C-Y" 'yank)
(defcommand "C-Z" 'stop-lisp)
(defcommand "C--" 'undo)

(defcommand "M-A" 'apropos-word)
(defcommand "M-B" 'move-word-backwards)
(defcommand "M-C")
(defcommand "M-D" 'delete-word-forwards)
(defcommand "M-E")
(defcommand "M-F" 'move-word-forwards)
(defcommand "M-G")
(defcommand "M-H" 'help)
(defcommand "M-I" 'describe-word)
(defcommand "M-J")
(defcommand "M-K")
(defcommand "M-L" 'downcase-word)
(defcommand "M-M")
(defcommand "M-N")
(defcommand "M-O")
(defcommand "M-P")
(defcommand "M-Q")
(defcommand "M-R")
(defcommand "M-S")
(defcommand "M-T")
(defcommand "M-U" 'upcase-word)
(defcommand "M-V")
(defcommand "M-W" 'copy-region)
(defcommand "M-X")
(defcommand "M-Y" 'yank-cycle)
(defcommand "M-Z")
(defcommand "M-1")
(defcommand "M-2")
(defcommand "M-3")
(defcommand "M-4")
(defcommand "M-5")
(defcommand "M-6")
(defcommand "M-7")
(defcommand "M-8")
(defcommand "M-9")
(defcommand "M-0")

(defcommand "C-M-b" 'backward-sexp)
(defcommand "C-M-f" 'forward-sexp)
(defcommand "C-M-k" 'kill-sexp)

(defcommand "M-Backspace" 'delete-word-backwards)

(defcommand "C-Space" 'set-mark)
(defcommand "C-Backspace" 'delete-word-backwards)

(defcommand "Tab" 'complete)
(defcommand "Backspace" 'delete-char-backwards)
(defcommand "Return" 'finish-input)

(defcommand "Up-arrow" 'history-previous)
(defcommand "Down-arrow" 'history-next)
(defcommand "Right-arrow" 'move-char-right)
(defcommand "Left-arrow" 'move-char-left)
(defcommand "Insert" 'toggle-insert)
(defcommand "Delete" 'delete-char-forwards)
(defcommand "C-Delete")
(defcommand "Page-up")
(defcommand "Page-down")
(defcommand "Home" 'move-to-bol)
(defcommand "End" 'move-to-eol)
