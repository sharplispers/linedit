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

(defvar *terminal-translations* (make-hash-table :test #'equalp))

(defmacro deftrans (name chord)
  `(setf (gethash ,chord *terminal-translations*) ,name))

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
(deftrans "C-J" 10)
(deftrans "C-K" 11)
(deftrans "C-L" 12)
(deftrans "Return" 13)
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

(deftrans "M-A" '(#\Esc #\A))
(deftrans "M-B" '(#\Esc #\B))
(deftrans "M-C" '(#\Esc #\C))
(deftrans "M-D" '(#\Esc #\D))
(deftrans "M-E" '(#\Esc #\E))
(deftrans "M-F" '(#\Esc #\F))
(deftrans "M-G" '(#\Esc #\G))
(deftrans "M-H" '(#\Esc #\H))
(deftrans "M-I" '(#\Esc #\I))
(deftrans "M-J" '(#\Esc #\J))
(deftrans "M-K" '(#\Esc #\K))
(deftrans "M-L" '(#\Esc #\L))
(deftrans "M-M" '(#\Esc #\M))
(deftrans "M-N" '(#\Esc #\N))
(deftrans "M-O" '(#\Esc #\O))
(deftrans "M-P" '(#\Esc #\P))
(deftrans "M-Q" '(#\Esc #\Q))
(deftrans "M-R" '(#\Esc #\R))
(deftrans "M-S" '(#\Esc #\S))
(deftrans "M-T" '(#\Esc #\T))
(deftrans "M-U" '(#\Esc #\U))
(deftrans "M-V" '(#\Esc #\V))
(deftrans "M-W" '(#\Esc #\W))
(deftrans "M-X" '(#\Esc #\X))
(deftrans "M-Y" '(#\Esc #\Y))
(deftrans "M-Z" '(#\Esc #\Z))
(deftrans "M-1" '(#\Esc #\1))
(deftrans "M-2" '(#\Esc #\2))
(deftrans "M-3" '(#\Esc #\3))
(deftrans "M-4" '(#\Esc #\4))
(deftrans "M-5" '(#\Esc #\5))
(deftrans "M-6" '(#\Esc #\6))
(deftrans "M-7" '(#\Esc #\7))
(deftrans "M-8" '(#\Esc #\8))
(deftrans "M-9" '(#\Esc #\9))
(deftrans "M-0" '(#\Esc #\0))

(deftrans "Up-arrow"    '(#\Esc #\[ #\A))
(deftrans "Down-arrow"  '(#\Esc #\[ #\B))
(deftrans "Right-arrow" '(#\Esc #\[ #\C))
(deftrans "Left-arrow"  '(#\Esc #\[ #\D))
(deftrans "Insert"      '(#\Esc #\[ #\2 #\~))
(deftrans "Delete"      '(#\Esc #\[ #\3 #\~))
(deftrans "C-Delete"    '(#\Esc #\[ #\3 #\^))
(deftrans "Page-up"     '(#\Esc #\[ #\5 #\~))
(deftrans "Page-down"   '(#\Esc #\[ #\6 #\~))
(deftrans "Home"        '(#\Esc #\[ #\7 #\~))
(deftrans "End"         '(#\Esc #\[ #\8 #\~))
