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

(defun uninstall-repl ()
  #.(format nil
            "Uninstalls the Linedit REPL, restoring original handlers. Unsupported on ~A."
            (lisp-implementation-type))
  (error "~S is unsupported on ~A."
         'uninstall-repl
         (lisp-implementation-type)))

(defun install-repl (&key wrap-current eof-quits)
  #.(format nil
            "Installs Linedit at REPL. Original input handlers can be
preserved with :WRAP-CURRENT T. Unsupported on ~A."
            (lisp-implementation-type))
  (declare (ignore wrap-current eof-quits))
  (error "~S is unsupported on ~A."
         'install-repl
         (lisp-implementation-type)))
