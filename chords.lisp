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

(defvar *chords* (make-hash-table :test #'equalp))

(defmacro defchord (name chord)
  `(setf (gethash ,name *chords*) ,chord))

(defchord "C-Space" 0)
(defchord "C-A" 1)
(defchord "C-B" 2)
(defchord "C-C" 3)
(defchord "C-D" 4)
(defchord "C-E" 5)
(defchord "C-F" 6)
(defchord "C-G" 7)
(defchord "C-Backspace" 8)
(defchord "Tab" 9)
(defchord "C-J" 10)
(defchord "C-K" 11)
(defchord "C-L" 12)
(defchord "Return" 13)
(defchord "C-N" 14)
(defchord "C-O" 15)
(defchord "C-P" 16)
(defchord "C-Q" 17)
(defchord "C-R" 18)
(defchord "C-S" 19)
(defchord "C-T" 20)
(defchord "C-U" 21)
(defchord "C-V" 22)
(defchord "C-W" 23)
(defchord "C-X" 24)
(defchord "C-Y" 25)
(defchord "C-Z" 26)
(defchord "C--" 31)
(defchord "Backspace" 127)

(defchord "M-A" '(#\Esc #\A))
(defchord "M-B" '(#\Esc #\B))
(defchord "M-C" '(#\Esc #\C))
(defchord "M-D" '(#\Esc #\D))
(defchord "M-E" '(#\Esc #\E))
(defchord "M-F" '(#\Esc #\F))
(defchord "M-G" '(#\Esc #\G))
(defchord "M-H" '(#\Esc #\H))
(defchord "M-I" '(#\Esc #\I))
(defchord "M-J" '(#\Esc #\J))
(defchord "M-K" '(#\Esc #\K))
(defchord "M-L" '(#\Esc #\L))
(defchord "M-M" '(#\Esc #\M))
(defchord "M-N" '(#\Esc #\N))
(defchord "M-O" '(#\Esc #\O))
(defchord "M-P" '(#\Esc #\P))
(defchord "M-Q" '(#\Esc #\Q))
(defchord "M-R" '(#\Esc #\R))
(defchord "M-S" '(#\Esc #\S))
(defchord "M-T" '(#\Esc #\T))
(defchord "M-U" '(#\Esc #\U))
(defchord "M-V" '(#\Esc #\V))
(defchord "M-W" '(#\Esc #\W))
(defchord "M-X" '(#\Esc #\X))
(defchord "M-Y" '(#\Esc #\Y))
(defchord "M-Z" '(#\Esc #\Z))
(defchord "M-1" '(#\Esc #\1))
(defchord "M-2" '(#\Esc #\2))
(defchord "M-3" '(#\Esc #\3))
(defchord "M-4" '(#\Esc #\4))
(defchord "M-5" '(#\Esc #\5))
(defchord "M-6" '(#\Esc #\6))
(defchord "M-7" '(#\Esc #\7))
(defchord "M-8" '(#\Esc #\8))
(defchord "M-9" '(#\Esc #\9))
(defchord "M-0" '(#\Esc #\0))

(defchord "Up-arrow"    '(#\Esc #\[ #\A))
(defchord "Down-arrow"  '(#\Esc #\[ #\B))
(defchord "Right-arrow" '(#\Esc #\[ #\C))
(defchord "Left-arrow"  '(#\Esc #\[ #\D))
(defchord "Insert"      '(#\Esc #\[ #\2 #\~))
(defchord "Delete"      '(#\Esc #\[ #\3 #\~))
(defchord "C-Delete"    '(#\Esc #\[ #\3 #\^))
(defchord "Page-up"     '(#\Esc #\[ #\5 #\~))
(defchord "Page-down"   '(#\Esc #\[ #\6 #\~))
(defchord "Home"        '(#\Esc #\[ #\7 #\~))
(defchord "End"         '(#\Esc #\[ #\8 #\~))
