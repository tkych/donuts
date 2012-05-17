;;;; Last Updated : 2012/05/17 21:35:46 tkych

;; edge.lisp in donuts/src/

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
;; Edge
;;====================================================================
(in-package :in-donuts)

(defclass edge ()
  ((name :accessor :name :initarg :name :initform nil)))

(defun edge? (x) (typep x 'edge))

;;--------------------------------------
(defclass normal-edge (edge)
  ((node1 :accessor :node1 :initarg :node1 :initform nil)
   (node2 :accessor :node2 :initarg :node2 :initform nil)
   (attrs :accessor :attrs :initarg :attrs :initform nil)))

(defmethod print-object ((obj normal-edge) stream)
  (print-unreadable-object (obj stream)
    (with-slots (name node1 node2) obj
       (format stream "~A :: ~A -> ~A"
               name (name-of node1) (name-of node2)))))

(defun -> (node1 node2 &rest edge-attrs)
  (make-inst 'normal-edge :name (format nil "edge_~A" (gentemp "ID_"))
             :node1 node1 :node2 node2 :attrs edge-attrs))

;;--------------------------------------
(defclass penetrate-edge (edge)
  ((nodes :accessor :nodes :initarg :nodes :initform nil)))

(defmethod print-object ((obj penetrate-edge) stream)
  (print-unreadable-object (obj stream)
    (with-slots (name nodes) obj
       (format stream "~A :: ~{~A~^ -> ~} " name
               (loop :for node :in nodes :collect (name-of node))))))

(defun penetrate-edge? (x) (typep x 'penetrate-edge))

(defun --> (&rest nodes)
  (make-inst 'penetrate-edge :nodes nodes
             :name (format nil "edge_~A" (gentemp "ID_"))))

;;--------------------------------------
(defclass radiate-edge (edge)
     ((origin-node :accessor :origin-node :initarg :origin-node)
      (nodes       :accessor :nodes       :initarg :nodes)))

(defmethod print-object ((obj radiate-edge) stream)
  (print-unreadable-object (obj stream)
    (with-slots (name origin-node nodes) obj
       (format stream "~A :: ~A -> {~{~A~^; ~}} " name
               (name-of origin-node)
               (loop :for node :in nodes :collect (name-of node))))))

(defun radiate-edge? (x) (typep x 'radiate-edge))

(defun ->> (origin-node &rest nodes)
  (make-inst 'radiate-edge :origin-node origin-node :nodes nodes
             :name (format nil "edge_~A" (gentemp "ID_"))))

;;--------------------------------------
(defclass converge-edge (edge)
     ((nodes :accessor :nodes :initarg :nodes :initform nil)
      (converge-node :accessor :converge-node
                     :initarg :converge-node :initform nil)))

(defmethod print-object ((obj converge-edge) stream)
  (print-unreadable-object (obj stream)
    (with-slots (name nodes converge-node) obj
       (format stream "~A :: {~{~A~^; ~}} -> ~A" name
               (loop :for node :in nodes :collect (name-of node))
               (name-of converge-node)))))

(defun converge-edge? (x) (typep x 'converge-edge))

(defun ==> (&rest nodes)
  (make-inst 'converge-edge
             :nodes (butlast nodes)
             :converge-node (last1 nodes)
             :name (format nil "edge_~A" (gentemp "ID_"))))

;;--------------------------------------------------------------------
(defclass path (edge) ())

(defparameter *directed?* t)

(defun path? (x) (typep x 'path))

;;--------------------------------------
(defclass normal-path (normal-edge path) ())

(defmethod print-object ((obj normal-path) stream)
  (print-unreadable-object (obj stream)
    (with-slots (name node1 node2) obj
       (format stream "~A :: ~A -- ~A"
               name (name-of node1) (name-of node2)))))

(defun -- (node1 node2 &rest path-attrs)
  (setf *directed?* nil)
  (make-inst 'normal-path :name (format nil "path_~A" (gentemp "ID_"))
             :node1 node1 :node2 node2 :attrs path-attrs))

;;--------------------------------------
(defclass penetrate-path (penetrate-edge path) ())

(defmethod print-object ((obj penetrate-path) stream)
  (print-unreadable-object (obj stream)
    (with-slots (name nodes) obj
       (format stream "~A :: ~{~A~^ -- ~} " name
               (loop :for node :in nodes :collect (name-of node))))))

(defun penetrate-path? (x) (typep x 'penetrate-path))

(defun --- (&rest nodes)
  (setf *directed?* nil)
  (make-inst 'penetrate-path :nodes nodes
             :name (format nil "path_~A" (gentemp "ID_"))))


;;--------------------------------------
(defclass radiate-path (radiate-edge path) ())

(defmethod print-object ((obj radiate-path) stream)
  (print-unreadable-object (obj stream)
    (with-slots (name origin-node nodes) obj
       (format stream "~A :: ~A -- {~{~A~^; ~}} " name
               (name-of origin-node)
               (loop :for node :in nodes :collect (name-of node))))))

(defun radiate-path? (x) (typep x 'radiate-path))

(defun -< (origin-node &rest nodes)
  (setf *directed?* nil)
  (make-inst 'radiate-path :origin-node origin-node :nodes nodes
             :name (format nil "path_~A" (gentemp "ID_"))))

;;====================================================================