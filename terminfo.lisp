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

(in-package #:linedit-term)

(defparameter *terminfo* (make-hash-table))
(defparameter *infocmp* "/usr/bin/infocmp")

(let ((esc (princ-to-string #\Esc)))
  ;; FIXME; This most certainly doesn't unscape everything that we may
  ;; encounter.
  (defun unescape-terminfo (string)
    (regex-replace-all "\\\\E" string esc)))

(defmacro match-and-define (form &rest matches)
  (with-unique-names (txt len)
    `(let* ((,txt ,form)
	    (,len (length ,txt)))
       (cond
	 ,@(mapcar (lambda (match)
		     (destructuring-bind (str name) match
		       (let ((str-len (length str)))
			 `((and (>= ,len ,str-len)
				(not (mismatch ,str (subseq ,txt 0 ,str-len))))
			   (let ((it (unescape-terminfo (subseq ,txt ,str-len))))
			     (defun ,name ()
			       (write-string it)))))))
		   matches)))))

;; FIXME: Save terminfo data to fasls
(let ((infos
       ;; Would be nice to manage without regexps here...
       (split
	(let ((whitespace '(:greedy-repetition 0 nil :whitespace-char-class)))
	  `(:sequence
	    ,whitespace
	    (:negative-lookbehind #\\)
	    #\,
	    ,whitespace))
	(with-output-to-string (s)
	  (run-program *infocmp* (list (sb-ext:posix-getenv "TERM")) :output s)))))
  (dolist (info infos)
    (match-and-define
     info
     ("cuu1=" move-cursor-up))))
