;;;; Last Updated : 2012/05/20 09:06:39 tkych

;; dot-output.lisp in donuts/src/

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
;; Dot-Output
;;====================================================================
(in-package :in-donuts)

(defparameter *declared* nil)
(defun declared? (node) (member node *declared*))

(defun dot-output (graph)
  (unless (graph? graph) (error "~A isn't graph type." graph))
  (with-slots (name attrs buff dir cache strict) graph
     (~&) (when strict (princ "strict "))
     (format t "~A ~A {~%" (if dir "digraph" "graph") name)
     (awhen attrs
       (format t "~&~{  ~A=~A;~^~&~}~&" (escape-attrs it)))
     (aif cache
          (princ it)
          (let ((output
                 (with-output-to-string (s)
                   (let ((*standard-output* s) (*declared* nil))
                     (output-buff buff)
                     (setf buff nil))))) ;for gc
            (setf cache output)
            (princ output)))
     (format t "~&}")))

(defun output-buff (buff)
  (dolist (x buff)
    (cond ((edge? x)     (output-edge x))
          ((graph? x)    (output-subgraph x))
          ((pre-node? x) (format t "~&  ~S;" x))
          ((node? x)     (declare-node x))
          ((sesame? x)   (output-sesame x)) ;for with
          ((null x)      nil)               ;for null graph
          (t             (error "~A is not graph content type." x)))))

;;--------------------------------------
(defun declare-node (node)
  (when (and (node? node) (not (declared? node)))
    (format t "~&  ~A [~{~A=~A~^,~}];"
            (:name node) (escape-attrs (:attrs node)))
    (push node *declared*)))

(defun output-node (node)
  (cond ((pre-node? node) (format t "~S" node))
        ((sesame? node)   (format t "~A" (open-sesame node))) ;for port
        ((node? node)     (format t "~A" (:name node)))
        (t                (error "~A is not node type." node))))

;;--------------------------------------
(defgeneric output-edge (edge))

(defmethod output-edge ((edge normal-edge))
  (with-slots (node1 node2 attrs) edge
     (declare-node node1)
     (declare-node node2)
     (~&) (princ "  ")
     (output-node node1)
     (if (path? edge) (princ " -- ") (princ " -> "))
     (output-node node2)
     (when attrs
       (format t " [~{~A=~A~^,~}]" (escape-attrs attrs)))
     (princ ";")))

(defmethod output-edge ((edge penetrate-edge))
  (with-slots (nodes) edge
     (dolist (node nodes) (declare-node node))
     (~&) (princ "  ")
     (output-node (1st nodes))
     (dolist (node (rest nodes))
       (if (path? edge) (princ " -- ") (princ " -> "))
       (output-node node))
     (princ ";")))

(defmethod output-edge ((edge radiate-edge))
  (with-slots (origin-node nodes) edge
     (declare-node origin-node)
     (dolist (node nodes) (declare-node node))
     (~&) (princ "  ")
     (output-node origin-node)
     (if (path? edge) (princ " -- {") (princ " -> {"))
     (output-node (1st nodes))
     (dolist (node (rest nodes))
       (princ "; ")
       (output-node node))
     (princ "};")))

(defmethod output-edge ((edge converge-edge))
  (with-slots (nodes converge-node) edge
     (declare-node converge-node)
     (dolist (node nodes) (declare-node node))
     (dolist (node nodes)
      (~&) (princ "  ")
      (output-node node)
      (if (path? edge) (princ " -- ") (princ " -> "))
      (output-node converge-node)
      (princ ";"))))

;;--------------------------------------
(defgeneric output-subgraph (graph))

(defmethod output-subgraph ((graph graph))
  (with-slots (buff cache) graph
     (if cache
         (format t "~A" cache)
         (output-buff buff))))

(defmethod output-subgraph ((cluster cluster))
  (with-slots (name attrs buff cache) cluster
     (format t "~&  subgraph ~A {~%" name)
     (awhen attrs
       (format t "~&~{  ~A=~A;~^~&~}~&" (escape-attrs it)))
     (if cache
         (format t "~A~&  }" cache)
         (progn (output-buff buff)
                (format t "~&  }")))))

;;--------------------------------------
(defparameter *capital-vals* '(:Mdiamond :Msquare :Mcircle :Mrecord))
(defun capital-val? (x) (member x *capital-vals*))

(defparameter *upper-vals*
              '(:LR :RL :TB :BT :BL :BR :TL :TR :RB :RT :LB :LT))
(defun upper-val? (x) (member x *upper-vals*))

(defparameter *URL-attrs* '(:URL :headURL :tailURL :labelURL :edgeURL))
(defun URL-attr? (x) (member x *URL-attrs*))
(defun escape-url-attr (url-attr)
  (format nil "~{~(~A~)~}URL"
          (aif (ppcre:split "URL" (symbol-name url-attr))
               it '(""))))

(defun escape-attrs (alst)
  (let ((len (length alst)) (acc nil))
    (do ((i 0 (1+ i)))
        ((<= len i) (nreverse acc))
      (let ((x (nth i alst)))
        (push
         (if (evenp i)
             (if (URL-attr? x)
                 (escape-url-attr x)
                 (string-downcase (symbol-name x)))
             (cond ((sesame? x)  (open-sesame x)) ;for html-like-label
                   ((stringp x)  (str "\"" x "\""))
                   ((numberp x)  x)
                   ((eql x t)    "true")
                   ((eql x nil)  "false")
                   ((keywordp x)
                    (cond ((upper-val? x)   (symbol-name x))
                          ((capital-val? x) (string-capitalize
                                             (symbol-name x)))
                          (t                (string-downcase
                                             (symbol-name x)))))
                   ((cluster? x) (:name x))
                   (t (error "~A is not an attribute value type." x))))
         acc)))))

;;--------------------------------------
(defun output-sesame (sesame)
  (format t "~&~A" (open-sesame sesame)))

;;====================================================================