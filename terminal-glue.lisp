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

    +linedit-OK+)

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
