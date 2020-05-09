(defpackage :util
  (:use :common-lisp :un-utils.simple-syntax)
  (:export #:yo))

(in-package :util)

(defun yo ()
  (in 'x '+ '- '*))
