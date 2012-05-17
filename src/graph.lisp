;;;; Last Updated : 2012/05/17 21:51:05 tkych

;; Graph in donuts

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
;; Graph
;;====================================================================
(in-package :in-donuts)

(defclass graph ()
  ((name  :accessor :name  :initarg :name  :initform nil)
   (attrs :accessor :attrs :initarg :attrs :initform nil)
   (buff  :accessor :buff  :initarg :buff  :initform nil)
   (cache :accessor :cache :initarg :cache :initform nil)
   (dir   :accessor :dir   :initarg :dir   :initform t)
   (strict :accessor :strict :initarg :strict :initform nil)))

(defun graph? (x) (typep x 'graph))

(defmethod print-object ((obj graph) stream)
  (print-unreadable-object (obj stream)
    (format stream "GRAPH ~A::~{~A=~A~^ : ~}"
            (:name obj) (escape-attrs (:attrs obj)))))

(defun make-graph (graph-attrs &rest nodes-edges-graphs)
  (let ((name   (getf graph-attrs :name))
        (strict (getf graph-attrs :strict)))
    (remf graph-attrs :name) (remf graph-attrs :strict)
    (when (and *directed?*
               (or (find-if #'path? nodes-edges-graphs)
                   (not (every #':dir (remove-if-not
                                       #'graph? nodes-edges-graphs)))))
      (setf *directed?* nil))
    (make-inst 'graph :dir *directed?* :attrs graph-attrs
               :buff nodes-edges-graphs :strict strict
               :name (apply #'format nil
                            (if name
                                `("~A" ,name)
                                `("graph_~A" ,(gentemp "ID_")))))))

(defmacro & ((&rest graph-attrs) &body nodes-edges-graphs)
  `(let ((*directed?* t))
     (make-graph ',graph-attrs ,@nodes-edges-graphs)))

(let ((*directed?* t))
  (defun && (&rest nodes-edges-graphs)
    (when (and *directed?*
               (or (find-if #'path? nodes-edges-graphs)
                   (not (every #':dir (remove-if-not
                                       #'graph? nodes-edges-graphs)))))
      (setf *directed?* nil))
    (prog1
      (make-inst 'graph :dir *directed?*
                 :buff nodes-edges-graphs
                 :name (format nil "graph_~A" (gentemp "ID_")))
      (setf *directed?* t))))

;;--------------------------------------
(defclass cluster (graph) ())

(defmethod print-object ((obj cluster) stream)
  (print-unreadable-object (obj stream)
    (format stream "CLUSTER ~A::~{~A=~A~^ : ~}"
            (:name obj) (escape-attrs (:attrs obj)))))

(defun cluster? (x) (typep x 'cluster))

(defun make-cluster (cluster-attrs &rest nodes-edges-graphs)
  (make-inst 'cluster :dir *directed?* :attrs cluster-attrs
                      :buff nodes-edges-graphs
                      :name (format nil "cluster_~A" (gentemp "ID_"))))

(defmacro [&] ((&rest graph-attrs) &body nodes-edges-graphs)
  `(let ((*directed?* t))
     (make-cluster ',graph-attrs ,@nodes-edges-graphs)))

;;====================================================================