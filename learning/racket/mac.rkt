#lang racket

(require mzlib/defmacro)

(defmacro my-or (x y)
  `(if ,x ,x ,y))
