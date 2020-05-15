; {{{ boilerplate
; load tools that will let us use libraries
(load "~/quicklisp/setup.lisp") ; tool to load external systems
(require "asdf") ; tool to load local systems

(asdf:load-system "p4bwcl") ; load p4bwcl system
(asdf:load-system "un-utils")

(use-package 'un-utils.simple-syntax) ; always use simple-syntax because we are newbies
; }}}

(import 'collect:collect)

(let ((x 0))
  (while (< x 10)
    (print-line "~A" x)
    (incf x)))

;(let (l '(list 1 2 3 4 5 6 7 8 9 10))
;  (collect-list (list x y)
;    (for x in l)
;    (evenp x)
;    (for y in l)
;    (oddp y)))

;(princ (collect list ((* x x))
;  (collect::in x '(1 2 3 4 5 6 7 8))))
;

; in python we say [i*i for i in [1, 2, 3, 4, 5]]
(princ (loop for i in (list 1 2 3 4 5) collect (* i i)))
