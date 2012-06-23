;; Copyright (c) 2003-2012 Nikodemus Siivola <nikodemus@random-state.net>
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

(defpackage :linedit-system
  (:use :cl :asdf))

(in-package :linedit-system)

;;;; C compiler

(defvar *gcc* "/usr/bin/gcc")

(defvar *gcc-options*
  #-(or darwin macosx)
  (list "-shared" "-fPIC")
  #+(or darwin macosx)
  (append
   (list "-dynamic"  "-bundle")
   #+(or x86 x86-64)
   (list "-arch" "x86_64" "-arch" "i386")
   #-sbcl
   (list "/usr/lib/bundle1.o" "-flat_namespace" "-undefined" "suppress")))

;;;; Separate class so that we don't mess up other packages
(defclass uffi-c-source-file (c-source-file) ())

(defmethod output-files ((o compile-op) (c uffi-c-source-file))
  (list (make-pathname :name (component-name c)
                       :type #-(or darwin macosx) "so" #+(or darwin macosx) "dylib"
                       :defaults (component-pathname c))))

(defmethod perform ((o load-op) (c uffi-c-source-file))
  (let ((loader (intern (symbol-name '#:load-foreign-library) :uffi)))
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
  :version "0.17.5"
  :description "Readline-style library."
  :licence "MIT"
  :author "Nikodemus Siivola <nikodemus@sb-studio.net>"
  :depends-on (:uffi :terminfo :osicat :alexandria)
  :defsystem-depends-on (:madeira-port)
  :components
  (;; Common
   (:file "packages")
   (:file "utility-functions" :depends-on ("packages"))
   (:file "utility-macros" :depends-on ("packages" "utility-functions"))
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
   (:file "editor" :depends-on ("backend" "rewindable"
                                "line" "buffer" "command-keys"))
   (:file "main" :depends-on ("editor"))
   (:file "complete" :depends-on ("utility-macros"))
   (:file "command-functions" :depends-on ("editor"))
   (:module "ports"
            :depends-on ("main")
            :serial t
            :components ((:madeira-port "sbcl" :when :sbcl)
                         (:madeira-port "ccl" :when :ccl)
                         (:madeira-port "generic" :unless (:or :sbcl :ccl))))))
