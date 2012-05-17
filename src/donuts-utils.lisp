;;;; Last Updated : 2012/05/17 20:38:45 tkych

;; utilities in donuts/src/

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
;; Utilities for Donuts
;;====================================================================
(in-package :in-donuts)

(defmacro make-with (with-type attrs &body body)
  (let ((start (make-sesame :with-start
                            (format nil "~&  { ~(~A~) [~{~A=~A~^,~}];"
                                    with-type (escape-attrs attrs))))
        (end (make-sesame :with-end (format nil "~&  };"))))
    `(& () ',start ,@body ',end)))

(defmacro with-node ((&rest node-attrs) &body body)
  `(make-with :node ,node-attrs ,@body))

(defmacro with-edge ((&rest edge-attrs) &body body)
  `(make-with :edge ,edge-attrs ,@body))

(defmacro with-graph ((&rest graph-attrs) &body body)
  `(make-with :graph ,graph-attrs ,@body))

(defmacro $$ (graph)
  `($ () ,graph))

(defun ~ (&rest nodes)
  (apply #'rank :same nodes))

(defun ? (node &rest edge-attrs)
  (apply #'-> node node edge-attrs))

(defun O (&rest nodes)
  (apply #'--- (conc1 nodes (1st nodes))))

;;====================================================================