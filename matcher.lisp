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

;;;; QUOTES

;; FIXME: should checking for #\", "\"", et cetera.

(defun quoted-p (string index)
  (let ((quoted-p nil))
    (dotimes (n (min index (length string)) quoted-p)
      (when (eql (schar string n) #\")
	(setf quoted-p (not quoted-p))))))

(defun find-open-quote (string index)
  (when (quoted-p string index)
    (loop for n from (1- index) downto 0
	  when (eql (schar string n) #\") return n)))

(defun find-close-quote (string index)
  (when (quoted-p string index)
    (loop for n from (1+ index) below (length string)
	  when (eql (schar string n) #\") return n)))

;;;; PARENS

;; FIXME: This is not the Right Way to do paren matching.
;; * use stack, not counting
;; * don't count #\( #\) &co

(defun after-close-p (string index)
  (and (array-in-bounds-p string (1- index))
       (find (schar string (1- index)) ")]}")))

(defun at-open-p (string index)
  (and (array-in-bounds-p string index)
       (find (schar string index) "([{")))

(defun paren-count-delta (char)
  (case char
    ((#\( #\[ #\{) -1)
    ((#\) #\] #\}) 1)
    (t 0)))

(defun find-open-paren (string index)
  (loop with count = 1
	for n from (1- index) downto 0
	do (incf count (paren-count-delta (schar string n)))
	when (zerop count) return n))

(defun find-close-paren (string index)
  (loop with count = -1
	for n from (1+ index) below (length string)
	do (incf count (paren-count-delta (schar string n)))
	when (zerop count) return n))

(defun dwim-match-parens (string index)
  (cond ((after-close-p string index)
	 (values (find-open-paren string (1- index)) (1- index)))
	((at-open-p string index)
	 (values index (find-close-paren string index)))
	(t 
	 (values nil nil))))

(defun dwim-mark-parens (string index &key pre-mark post-mark)
  (multiple-value-bind (open close) (dwim-match-parens string index)
    (values 
     (if (and open close)
	 (concat (subseq string 0 open)
		 pre-mark
		 (string (schar string open))
		 post-mark
		 (subseq string (1+ open) close)
		 pre-mark
		 (string (schar string close))
		 post-mark
		 (if (> (length string) (1+ close))
		     (subseq string (1+ close))
		     ""))
	 string)
     open)))
