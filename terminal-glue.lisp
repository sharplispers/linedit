;; Copyright (c) 2016 Anmol Khirbat
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

(defvar +linedit-ok+              0)
(defvar +linedit-not-atty+        1)
(defvar +linedit-memory-error+    2)
(defvar +linedit-tcgetattr-error+ 3)
(defvar +linedit-tcsetattr-error+ 4)
(defvar +linedit-attr-error+      5)
(defvar +linedit-no-attr-error+   6)

(let (attr)

  (defun c-terminal-init ()
    (if (zerop (isatty 0))
        (return-from c-terminal-init +linedit-not-atty+))

    ;; Save current terminal state in attr
    (if attr
        (return-from c-terminal-init +linedit-attr-error+))

    (setf attr (cffi:foreign-alloc '(:struct termios)))

    (when (minusp (tcgetattr 0 attr))
      (return-from c-terminal-init +linedit-tcgetattr-error+))

    ;; Enter keyboard input mode
    (cffi:with-foreign-object (tmp '(:struct termios))
      (when (minusp (tcgetattr 0 tmp))
        (return-from c-terminal-init +linedit-tcgetattr-error+))

      (cffi:foreign-funcall "cfmakeraw" :pointer tmp :void)

      (cffi:with-foreign-slots ((oflag) tmp (:struct termios))
        (setf oflag (logior oflag tty-OPOST)))

      (if (minusp (tcsetattr 0 TCSAFLUSH tmp))
          +linedit-tcsetattr-error+))

    +linedit-ok+)

  (defun c-terminal-close ()
    ;; Restore saved terminal state from attr
    (if (null attr)
        (return-from c-terminal-close +linedit-no-attr-error+))

    (if (zerop (isatty 0))
        (return-from c-terminal-close +linedit-not-atty+))

    (if (minusp (tcsetattr 0 TCSANOW attr))
        (return-from c-terminal-close +linedit-tcsetattr-error+))

    (cffi:foreign-free attr)
    (setf attr nil)

    +linedit-ok+))

(defun c-terminal-winsize (def side side-env)
  (if (boundp 'TIOCGWINSZ)
      (cffi:with-foreign-object (size '(:struct winsize))
        (and (zerop (ioctl 0 TIOCGWINSZ size))
             (cffi:foreign-slot-value size '(:struct winsize) side)))
      (aif (getenv side-env)
           (parse-integer it)
           def)))

(defun c-terminal-lines (def)
  (c-terminal-winsize def 'osicat-posix:row "LINES"))

(defun c-terminal-columns (def)
  (c-terminal-winsize def 'osicat-posix:col "COLUMNS"))
