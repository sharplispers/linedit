;;;; Copyright (c) 2020, 2021 Jan Moringen
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

(cl:in-package #:linedit)

;;; Client

(defclass linedit-client (render::nesting-highlighting-mixin
                          render::error-message-mixin
                          render::ansi-text-client)
  ((%new-point       :initarg  :new-point
                     :accessor %new-point
                     :initform nil)
   (%character-count :accessor character-count
                     :initform 0)))

(defmethod new-point ((client linedit-client))
  (or (%new-point client) (character-count client)))

(defmethod render:write-character ((client    linedit-client)
                                   (position  t)
                                   (character t)
                                   (node      t))
  (when (= position (render::point client))
    (setf (%new-point client) (character-count client)))
  (incf (character-count client))
  (call-next-method))

(defmethod render:leave-errors ((client linedit-client) (errors sequence))
  (call-next-method)
  (incf (character-count client)
        (+ 2                    ; "[" and "]"
           (1- (length errors)) ; "/" between messages
           (reduce #'+ errors
                   :key (alexandria:compose #'length #'cst:message)))))

;;; Entry points

(defun syntax-highlight (string point)
  (let* ((stream (make-string-output-stream))
         (client (make-instance 'linedit-client :stream stream :point point)))
    (hi::highlight string :client client)
    (values (get-output-stream-string stream)
            (new-point client)
            (character-count client))))

(defun listing (pathname)
  (write-string (syntax-highlight (alexandria:read-file-into-string pathname) 0) *standard-output*))
