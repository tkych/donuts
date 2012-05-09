;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;     Last Updated : 2012/05/09 20:57:35 tkych

;; System for donuts

;; Copyright (c) 2012 Takaya OCHIAI

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;====================================================================
;; Donuts: Graph DSL for Common Lisp
;;====================================================================
;; donuts.asd       -- system definition (this file)
;; in-package.lisp  -- internal package
;; cl-utils.lisp    -- cl utils
;; api-package.lisp -- application package
;; README.markdown  -- readme file for github
;; src/
;;   nuts.lisp  -- core
;;   flesh.lisp -- donuts utils
;;   shell.lisp -- shell interface
;; doc/
;;   index-j.html -- Japanese version of donuts documentaion
;;   index.html   -- English version of donuts documentaion
;;   images/      -- image files for index-j.html, index.html

;;====================================================================
;; System
;;====================================================================
(in-package :cl-user)
(defpackage #:donuts-asd (:use :cl :asdf))
(in-package #:donuts-asd)

(defsystem :donuts
  :description "Graph DSL for common lisp"
  :name        "Donuts"
  :version     "0.2.6"
  :licence     "MIT licence"
  :author      "Takaya OCHIAI <tkych.repl@gmail.com>"
  :long-description
  "Donuts is Graphviz interface for common lisp.
It requires the Graphviz system (http://www.graphviz.org/)."
  :depends-on (:cl-ppcre :trivial-shell)
  :serial t
  :components ((:file "in-package")
               (:file "cl-utils")
               (:module "src"
                        :components ((:file "nuts")
                                     (:file "flesh")
                                     (:file "shell")
                                     ))
               (:file "api-package")))

;;====================================================================