;;;; Copyright (c) 2003, 2004 Nikodemus Siivola
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

;;;; Check requirements -- this might be best done in a separate file,
;;;; or an asdf method.
#.(cond 
    ((stringp osicat:*osicat-version*) 
     (error "This version of Linedit requires Osicat version 0.4.0 or later, 
but current Osicat version is ~A." osicat:*osicat-version*))
    ((consp osicat:*osicat-version*)
     (unless (<= 4 (second osicat:*osicat-version*))
       (error "This version of Linedit requires Osicat version 0.4.0 or later, 
but current Osicat version is ~{~A~^.~}." osicat:*osicat-version*)))
    (t (error "No Osicat version found.")))

;;;; Package

(defpackage :linedit
  (:use :cl :osicat)
  (:export
   #:linedit
   #:formedit
   #:*default-columns*
   #:*default-lines*
   #+sbcl #:install-repl
   #+sbcl #:uninstall-repl
   #:start-debug
   #:end-debug
   ))

