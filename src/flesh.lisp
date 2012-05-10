;;;; Last Updated : 2012/05/10 20:57:46 tkych

;; Flesh in donuts

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
;; Flesh in Donuts
;;====================================================================
(in-package :in-donuts)

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro && (&body node-edge-graphs)
    `(& () ,@node-edge-graphs)))

(defmacro $$ (graph)
  `($ () ,graph))

(defun ~ (&rest nodes)
  (apply #'rank :same nodes))

;;--------------------------------------
(defun node-const? (x)
  (or (eql '<> x) (eql '[] x)))

(defmacro ->> (node0 &rest node+edge-attrs)
  (with-gensyms (start-node)
    `(let ((,start-node ,node0))
       (&&
         ,@(mapcar (^ (x) (if (consp x)
                              (if (node-const? (1st x))
                                  `(-> ,start-node ,x)
                                  `(-> ,start-node ,@x))
                              `(-> ,start-node ,x)))
                   node+edge-attrs)))))

(defmacro ==> (&rest node+edge-attrs+node0)
  (with-gensyms (end-node)
    `(let ((,end-node ,(last1 node+edge-attrs+node0)))
       (&&
         ,@(mapcar (^ (x) (if (consp x)
                              (if (node-const? (1st x))
                                  `(-> ,x ,end-node)
                                  `(-> ,(1st x) ,end-node ,@(rest x)))
                              `(-> ,x ,end-node)))
                   (butlast node+edge-attrs+node0))))))

(defun <- (node1 node2 &rest edge-attrs)
  (apply #'-> node2 node1 edge-attrs))

(defmacro <== (node0 &rest node+edge-attrs)
  (with-gensyms (end-node)
    `(let ((,end-node ,node0))
       (&&
         ,@(mapcar (^ (x) (if (consp x)
                              (if (node-const? (1st x))
                                  `(<- ,end-node ,x)
                                  `(<- ,end-node ,@x))
                              `(<- ,end-node ,x)))
                   node+edge-attrs)))))

(defun ? (node &rest edge-attrs)
  (apply #'-> node node edge-attrs))

;;--------------------------------------
(defmacro -< (node0 &rest node+path-attrs)
  (with-gensyms (start-node)
    `(let ((,start-node ,node0))
       (&&
         ,@(mapcar (^ (x) (if (consp x)
                              (if (node-const? (1st x))
                                  `(-- ,start-node ,x)
                                  `(-- ,start-node ,@x))
                              `(-- ,start-node ,x)))
                   node+path-attrs)))))

(defmacro >- (&rest node+edge-attrs+node0)
  (with-gensyms (end-node)
    `(let ((,end-node ,(last1 node+edge-attrs+node0)))
       (&&
         ,@(mapcar (^ (x) (if (consp x)
                              (if (node-const? (1st x))
                                  `(-- ,x ,end-node)
                                  `(-- ,(1st x) ,end-node ,@(rest x)))
                              `(-- ,x ,end-node)))
                   (butlast node+edge-attrs+node0))))))

(defun O (&rest nodes)
  (apply #'--- (conc1 nodes (1st nodes))))

;;====================================================================