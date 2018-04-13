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

(defpackage :linedit-system
  (:use :cl :asdf))

(in-package :linedit-system)

(defsystem :linedit
  :version "0.17.6"
  :description "Readline-style library."
  :licence "MIT"
  :author "Nikodemus Siivola <nikodemus@random-state.net>"
  :maintainer "Anmol Khirbat <anmol@khirbat.net>"
  :homepage "https://github.com/sharplispers/linedit"
  :depends-on (:cffi :terminfo :osicat :alexandria)
  :components
  (
   ;; Common
   (:file "packages")
   (:file "utility-functions" :depends-on ("packages"))
   (:file "utility-macros" :depends-on ("packages" "utility-functions"))
   (:file "matcher" :depends-on ("packages"))

   ;; Backend
   (:file "backend" :depends-on ("utility-macros"))
   (:file "terminal-glue")
   (:file "terminal-translations" :depends-on ("packages"))
   (:file "terminal" :depends-on ("terminal-translations" "backend" "terminal-glue"))
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
            :components
            ((:file "sbcl" :if-feature :sbcl)
             (:file "ccl" :if-feature :ccl)
             (:file "generic" :if-feature (:not (:or :sbcl :ccl)))))))
