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

(in-package :asdf)

;;;
;;; Methods for compiling .c's to .so's
;;;

(defvar *gcc* "/usr/bin/gcc")

(defmethod output-files ((o compile-op) (c c-source-file))
  (list (make-pathname :name (component-name c)
		       :type "so"
		       :defaults (component-pathname c))))

(defmethod perform ((o load-op) (c c-source-file))
  (dolist (f (input-files o c))
    (sb-alien:load-1-foreign f)))

(defmethod perform ((o compile-op) (c c-source-file))
  (unless (zerop (run-shell-command "~A ~A -shared -o ~A"
				    *gcc*
				    (namestring (component-pathname c))
				    (namestring (car (output-files o c)))))
    (error 'operation-error :component c :operation o)))

;;;
;;; The actual system
;;;

(defsystem :linedit
    :depends-on (:cl-ppcre)
    :components (
		 (:c-source-file "termios-glue")
		 (:file "packages")
		 (:file "util" :depends-on ("packages"))
		 (:file "termios" :depends-on ("util" "termios-glue"))
		 (:file "terminfo" :depends-on ("util"))
		 (:file "line" :depends-on ("util"))
		 (:file "buffer" :depends-on ("line"))
		 (:file "history" :depends-on ("buffer"))
		 (:file "kill" :depends-on ("buffer"))
		 (:file "undo" :depends-on ("line"))
		 (:file "chords" :depends-on ("packages"))
		 (:file "functions" :depends-on ("packages" "termios"))
		 (:file "commands" :depends-on ("kill" "functions" "chords"))
		 (:file "linedit"
			:depends-on
			("functions" "commands" "kill" "undo"
                         "history" "termios"))
		 (:file "repl" :depends-on ("linedit"))			
		 ))
