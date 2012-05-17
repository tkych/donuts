;;;; Last Updated : 2012/05/18 02:08:38 tkych

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
;; cl-utils.lisp    -- common lisp utilities
;; api-package.lisp -- application package
;; README.markdown  -- readme file for github
;; src/
;;   sesame.lisp     -- hide the treasure
;;   node.lisp       -- node
;;   edge.lisp       -- edge
;;   graph.lisp      -- graph
;;   dot-output.lisp       -- output 
;;   donuts-utils.lisp     -- donuts utilities
;;   shell.lisp            -- shell interface
;;   html-like-labels.lisp -- html like labels
;; doc/
;;   index-ja.org  -- Japanese version of donuts documentaion
;;   index-ja.html -- Japanese version of donuts documentaion
;;   index.org     -- English version of donuts documentaion (Under Translation)
;;   index.html    -- English version of donuts documentaion (Under Translation)
;;   images/       -- image files for index-ja.html, index.html

;;====================================================================
;; System
;;====================================================================
(in-package :cl-user)
(defpackage #:donuts-asd (:use :cl :asdf))
(in-package #:donuts-asd)

(defsystem :donuts
  :description "Graph DSL for common lisp"
  :name        "Donuts"
  :version     "0.3.0"
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
                        :components ((:file "sesame")
                                     (:file "node")
                                     (:file "edge")
                                     (:file "graph")
                                     (:file "dot-output")
                                     (:file "shell")
                                     (:file "html-like-labels")
                                     (:file "donuts-utils")
                                     ))
               (:file "api-package")))

;;====================================================================