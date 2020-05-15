; {{{ boilerplate
; load tools that will let us use libraries
(load "~/quicklisp/setup.lisp") ; tool to load external systems
(require "asdf") ; tool to load local systems

(asdf:load-system "p4bwcl") ; load p4bwcl system
(asdf:load-system "un-utils")

(use-package 'un-utils.simple-syntax) ; always use simple-syntax because we are newbies
; }}}

(let (line)
  (loop for line = (read-line *standard-input* nil 'eof)
    until (eq line 'eof)
    do
      (format t "Common Lisp is fun?  ~A~%" line)))
