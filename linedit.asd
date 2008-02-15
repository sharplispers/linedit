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

(declaim (optimize (debug 3) (safety 3)))

(defpackage :linedit-system
  (:use :cl :asdf))

(in-package :linedit-system)

(defvar *gcc* "/usr/bin/gcc")

(defvar *gcc-options* '(#-darwin "-shared"
			#+darwin "-bundle"
			"-fPIC"))

;;; Separate class so that we don't mess up other packages
(defclass uffi-c-source-file (c-source-file) ())

(defmethod output-files ((o compile-op) (c uffi-c-source-file))
  (list (make-pathname :name (component-name c)
		       :type "so"
		       :defaults (component-pathname c))))

(defmethod perform ((o load-op) (c uffi-c-source-file))
  (let ((loader (intern "LOAD-FOREIGN-LIBRARY" :uffi)))
    (dolist (f (asdf::input-files o c))
      (funcall loader f :module (pathname-name f)))))

(defmethod perform ((o compile-op) (c uffi-c-source-file))
  (unless (zerop (run-shell-command "~A ~A ~{~A ~}-o ~A"
				    *gcc*
				    (namestring (component-pathname c))
				    *gcc-options*
				    (namestring (car (output-files o c)))))
    (error 'operation-error :component c :operation o)))

(defsystem :linedit
    :version "0.16.1"
    :depends-on (:uffi :terminfo :osicat)
    :components
  (;; Common
   (:file "packages")
   (:file "utility-macros" :depends-on ("packages"))
   (:file "utility-functions" :depends-on ("packages"))
   (:file "matcher" :depends-on ("packages"))

   ;; Backend
   (:file "backend" :depends-on ("utility-macros"))
   (:uffi-c-source-file "terminal_glue")
   (:file "terminal-translations" :depends-on ("packages"))
   (:file "terminal" :depends-on ("terminal-translations" "backend" "terminal_glue"))
   (:file "smart-terminal" :depends-on ("terminal" "matcher"))
   (:file "dumb-terminal" :depends-on ("terminal"))

   ;; Editor
   (:file "rewindable" :depends-on ("utility-macros"))
   (:file "line" :depends-on ("utility-macros"))
   (:file "buffer" :depends-on ("utility-macros"))
   (:file "command-keys" :depends-on ("packages"))
   (:uffi-c-source-file "signals")
   (:file "editor" :depends-on ("backend" "rewindable" "signals"
				"line" "buffer" "command-keys"))
   (:file "main" :depends-on ("editor"))
   (:file "complete" :depends-on ("utility-macros"))
   (:file "command-functions" :depends-on ("editor"))
   #+sbcl (:file "sbcl-repl" :depends-on ("main"))))
