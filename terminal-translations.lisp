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

(defvar *terminal-translations* (make-hash-table :test #'equalp))

(defmacro deftrans (name &rest chords)
  `(dolist (chord ',chords)
     (let ((old (gethash chord *terminal-translations*)))
       (when (and old (not (equal old ,name)))
	 (warn "Overriding old translation ~S for ~S with ~S." old chord ,name)))
     (setf (gethash chord *terminal-translations*) ,name)))

(deftrans "C-Space" 0)
(deftrans "C-A" 1)
(deftrans "C-B" 2)
(deftrans "C-C" 3)
(deftrans "C-D" 4)
(deftrans "C-E" 5)
(deftrans "C-F" 6)
(deftrans "C-G" 7)
(deftrans "C-Backspace" 8)
(deftrans "Tab" 9)
(deftrans "C-K" 11)
(deftrans "C-L" 12)
(deftrans "Return" 10 13) ;; Newline and return
(deftrans "C-N" 14)
(deftrans "C-O" 15)
(deftrans "C-P" 16)
(deftrans "C-Q" 17)
(deftrans "C-R" 18)
(deftrans "C-S" 19)
(deftrans "C-T" 20)
(deftrans "C-U" 21)
(deftrans "C-V" 22)
(deftrans "C-W" 23)
(deftrans "C-X" 24)
(deftrans "C-Y" 25)
(deftrans "C-Z" 26)
(deftrans "C--" 31)
(deftrans "Backspace" 127)

(deftrans "M-A" (#\Esc #\A) 225)
(deftrans "M-B" (#\Esc #\B) 226)
(deftrans "M-C" (#\Esc #\C) 227)
(deftrans "M-D" (#\Esc #\D) 228)
(deftrans "M-E" (#\Esc #\E) 229)
(deftrans "M-F" (#\Esc #\F) 230)
(deftrans "M-G" (#\Esc #\G) 231)
(deftrans "M-H" (#\Esc #\H) 232)
(deftrans "M-I" (#\Esc #\I) 233)
(deftrans "M-J" (#\Esc #\J) 234)
(deftrans "M-K" (#\Esc #\K) 235)
(deftrans "M-L" (#\Esc #\L) 236)
(deftrans "M-M" (#\Esc #\M) 237)
(deftrans "M-N" (#\Esc #\N) 238)
(deftrans "M-O" (#\Esc #\O) 239)
(deftrans "M-P" (#\Esc #\P) 240)
(deftrans "M-Q" (#\Esc #\Q) 241)
(deftrans "M-R" (#\Esc #\R) 242)
(deftrans "M-S" (#\Esc #\S) 243)
(deftrans "M-T" (#\Esc #\T) 244)
(deftrans "M-U" (#\Esc #\U) 245)
(deftrans "M-V" (#\Esc #\V) 246)
(deftrans "M-W" (#\Esc #\W) 247)
(deftrans "M-X" (#\Esc #\X) 248)
(deftrans "M-Y" (#\Esc #\Y) 249)
(deftrans "M-Z" (#\Esc #\Z) 250)
(deftrans "M-0" (#\Esc #\0) 176)
(deftrans "M-1" (#\Esc #\1) 177)
(deftrans "M-2" (#\Esc #\2) 178)
(deftrans "M-3" (#\Esc #\3) 179)
(deftrans "M-4" (#\Esc #\4) 180)
(deftrans "M-5" (#\Esc #\5) 181)
(deftrans "M-6" (#\Esc #\6) 182)
(deftrans "M-7" (#\Esc #\7) 183)
(deftrans "M-8" (#\Esc #\8) 184)
(deftrans "M-9" (#\Esc #\9) 185)

(deftrans "C-M-f" (#\Esc #\^F) 134)
(deftrans "C-M-b" (#\Esc #\^B) 130)
(deftrans "C-M-k" (#\Esc #\^K) 139) 

(deftrans "Up-arrow"    (#\Esc #\[ #\A))
(deftrans "Down-arrow"  (#\Esc #\[ #\B))
(deftrans "Right-arrow" (#\Esc #\[ #\C))
(deftrans "Left-arrow"  (#\Esc #\[ #\D))
(deftrans "Insert"      (#\Esc #\[ #\2 #\~))
(deftrans "Delete"      (#\Esc #\[ #\3 #\~))
(deftrans "C-Delete"    (#\Esc #\[ #\3 #\^))
(deftrans "Page-up"     (#\Esc #\[ #\5 #\~))
(deftrans "Page-down"   (#\Esc #\[ #\6 #\~))
(deftrans "Home"        (#\Esc #\[ #\7 #\~) (#\Esc #\[ #\1 #\~) (#\Esc #\[ #\H))
(deftrans "End"         (#\Esc #\[ #\8 #\~) (#\Esc #\[ #\4 #\~) (#\Esc #\[ #\F))
