;;;; Last Updated : 2012/05/11 17:41:50 tkych

;; Nuts in donuts

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
;; Nuts
;;====================================================================
(in-package :in-donuts)

;;--------------------------------------------------------------------
;; Sesame
;;--------------------------------------------------------------------
(defun make-sesame (sesame content) (cons sesame content))
(defun sesame? (x) (consp x))
(defun sesame-cont (sesame) (cdr sesame))

;;--------------------------------------------------------------------
;; Node
;;--------------------------------------------------------------------
(defun pre-node? (x) (or (stringp x) (numberp x)))

(defclass node ()
  ((name  :accessor :name  :initarg :name  :initform nil)
   (attrs :accessor :attrs :initarg :attrs :initform nil)
   (ports :accessor :ports :initarg :ports :initform nil)))

(defun node? (x) (typep x 'node))

(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream)
    (with-slots (name attrs) obj
       (format stream "~A :: ~{~A=~A~^ : ~}"
               name (escape-attrs attrs)))))

(defun <> (label &rest node-attrs)
  (make-inst 'node :name (format nil "node_~A" (gentemp "ID_"))
                   :attrs (nconc `(:label ,label) node-attrs)))

;;--------------------------------------
(defun escape-port (label)
  (ppcre:regex-replace-all
   "(^|{|\\|):(\\S+?)(?=\\b|}|\\|)" label "\\1<\\2>" :preserve-case t))

(defun find-port (label)
  (labels ((rec (start acc)
             (mvbind (match-start match-end regex-start regex-end)
                 (ppcre:scan "(^|{|\\|):(\\S+?)(?=\\b|}|\\|)"
                             label :start start)
               (if (null match-start)
                   acc
                   (rec match-end
                        (cons (str->key
                               (subseq label (aref regex-start 1)
                                       (aref regex-end 1)))
                              acc))))))
    (rec 0 nil)))

(defun [] (label &rest node-attrs)
  (make-inst 'node :name  (format nil "record_~A" (gentemp "ID_"))
                   :ports (find-port label)
                   :attrs (append `(:shape :record
                                    :label ,(escape-port label))
                                  node-attrs)))

;;--------------------------------------
(defparameter *rank-pos* '(:same :min :max :source :sink))

(defun rank (pos &rest nodes)
  (if (not (member pos *rank-pos*))
      (error "~A is not a rank position." pos)
      (let ((rank-pos
             (format nil "~&  {rank=~(~A~);~{ ~A~^;~}};"
                     pos
                     (loop :for node :in nodes
                           :if (node? node) :collect (:name node)
                           :else :if (pre-node? node)
                           :collect (format nil "~S" node)))))
        (make-sesame 'rank rank-pos))))

;;--------------------------------------
(defparameter *compass* '(:n :ne :e :se :s :sw :w :nw :c :_))
(defun compass-port? (x) (member x *compass*))
(defun port-exist? (node port) (member port (:ports node)))

;; hot spot?
(defun @ (node &rest ports)
    (dolist (port ports)
      (unless (keywordp port) (error "~A isn't keyword." port)))
  (let ((port-num (length ports)))
    (cond
      ((pre-node? node)
       (cond ((<= 2 port-num)
              (error "Too match port ~A" node))
             ((not (compass-port? (1st ports)))
              (error "Node ~S hasn't port :~(~A~)." node (1st ports)))
             (t (port-proc1 node ports))))
      ((node? node)
       (cond ((<= 3 port-num)
              (error "Too match port ~A" node))
             ((and (= 2 port-num)
                   (or (not (port-exist? node (1st ports)))
                       (not (compass-port? (2nd ports)))))
              (error "First port must be record-port, and second port must be commpass-port."))
             ((and (= 1 port-num)
                   (not (port-exist? node (1st ports)))
                   (not (compass-port? (1st ports))))
              (error "Node ~S hasn't port :~(~A~)." node (1st ports)))
             (t (port-proc2 node ports))))
      (t (error "~A isn't node type." node)))))

(defun port-proc1 (node ports)
  (make-sesame 'port
            (format nil "~S:~(~A~)" node (1st ports))))

(defun port-proc2 (node ports)
  (make-inst 'node :attrs (:attrs node)
             :name (format nil "~A~{:~(~A~)~}" (:name node) ports)))

;;--------------------------------------------------------------------
;; Edge
;;--------------------------------------------------------------------
(defclass edge ()
  ((name  :accessor :name  :initarg :name  :initform nil)
   (node1 :accessor :node1 :initarg :node1 :initform nil)
   (node2 :accessor :node2 :initarg :node2 :initform nil)
   (attrs :accessor :attrs :initarg :attrs :initform nil)))

(defun edge? (x) (typep x 'edge))

(defmethod print-object ((obj edge) stream)
  (print-unreadable-object (obj stream)
    (with-slots (name node1 node2) obj
       (format stream "~A :: ~A -> ~A" name
               (if (node? node1) (:name node1) node1)
               (if (node? node2) (:name node2) node2)))))

(defun -> (node1 node2 &rest edge-attrs)
  (make-inst 'edge :name (format nil "edge_~A" (gentemp "ID_"))
             :node1 node1 :node2 node2 :attrs edge-attrs))

;;--------------------------------------
(defclass edges ()
  ((name  :accessor :name  :initarg :name  :initform nil)
   (nodes :accessor :nodes :initarg :nodes :initform nil)))

(defun edges? (x) (typep x 'edges))

(defmethod print-object ((obj edges) stream)
  (print-unreadable-object (obj stream)
    (with-slots (name node1 node2) obj
       (format stream "~A :: ~{~A~^ -> ~} " name
               (loop :for node :in (:nodes obj)
                     :if (node? node) :collect (:name node1)
                     :else :collect node)))))

(defun --> (&rest nodes)
  (make-inst 'edges :name (format nil "edges_~A" (gentemp "ID_"))
                    :nodes nodes))

;;--------------------------------------
(defclass path (edge) ())

(defmethod print-object ((obj path) stream)
  (print-unreadable-object (obj stream)
    (with-slots (name node1 node2) obj
       (format stream "~A :: ~A -- ~A" name
               (if (node? node1) (:name node1) node1)
               (if (node? node2) (:name node2) node2)))))

(defun path? (x) (typep x 'path))

(defparameter *directed?* t)

(defun -- (node1 node2 &rest path-attrs)
  (setf *directed?* nil)
  (make-inst 'path :name (format nil "path_~A" (gentemp "ID_"))
                   :node1 node1 :node2 node2 :attrs path-attrs))

;;--------------------------------------
(defclass paths (edges) ())

(defun paths? (x) (typep x 'paths))

(defmethod print-object ((obj paths) stream)
  (print-unreadable-object (obj stream)
    (with-slots (name node1 node2) obj
       (format stream "~A :: ~{~A~^ -- ~} " name
               (loop :for node :in (:nodes obj)
                     :if (node? node) :collect (:name node1)
                     :else :collect node)))))

(defun --- (&rest nodes)
  (setf *directed?* nil)
  (make-inst 'paths :name (format nil "paths_~A" (gentemp "ID_"))
                    :nodes nodes))

;;--------------------------------------------------------------------
;; Graph
;;--------------------------------------------------------------------
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
    (when *directed?*
      (unless (every #':dir (remove-if-not #'graph? nodes-edges-graphs))
        (setf *directed?* nil)))
    (make-inst 'graph :dir *directed?* :attrs graph-attrs
               :buff nodes-edges-graphs :strict strict
               :name (apply #'format nil
                            (if name
                                `("~A" ,name)
                                `("graph_~A" ,(gentemp "ID_")))))))

(defmacro & ((&rest graph-attrs) &body nodes-edges-graphs)
  `(let ((*directed?* t))
     (make-graph ',graph-attrs ,@nodes-edges-graphs)))

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

;;--------------------------------------------------------------------
;; With
;;--------------------------------------------------------------------
(defmacro make-with (with-type attrs &body body)
  (let ((start (make-sesame 'with-start
                         (format nil "~&  { ~(~A~) [~{~A=~A~^,~}];"
                                 with-type (escape-attrs attrs))))
        (end (make-sesame 'with-end (format nil "~&  };"))))
    `(& () ',start ,@body ',end)))

(defmacro with-node ((&rest node-attrs) &body body)
  `(make-with :node ,node-attrs ,@body))

(defmacro with-edge ((&rest edge-attrs) &body body)
  `(make-with :edge ,edge-attrs ,@body))

(defmacro with-graph ((&rest graph-attrs) &body body)
  `(make-with :graph ,graph-attrs ,@body))

;;--------------------------------------------------------------------
;; Dot-Output
;;--------------------------------------------------------------------
(defparameter *declared* nil)
(defun declared? (node) (member node *declared*))

(defun dot-output (graph)
  (unless (graph? graph) (error "~A isn't graph type" graph))
  (with-slots (name attrs buff dir cache strict) graph
     (~&) (when strict (princ "strict "))
     (format t "~A ~A {~%" (if dir "digraph" "graph") name)
     (awhen attrs
       (format t "~&~{  ~A=~A;~^~&~}~&" (escape-attrs it)))
     (if cache
         (format t "~A" cache)
         (let ((output
                (with-output-to-string (s)
                  (let ((*standard-output* s) (*declared* nil))
                    (output-buff buff)
                    (setf buff nil))))) ;for gc
           (setf cache output)
           (format t "~A" output)))
     (format t "~&}")))

(defun output-buff (buff)
  (dolist (x buff)
    (cond ((edge? x)     (output-edge x))
          ((sesame? x)      (format t "~&~A" (sesame-cont x)))
          ((edges? x)    (output-edges x))
          ((cluster? x)  (output-cluster x))
          ((graph? x)    (output-subgraph x))
          ((pre-node? x) (format t "~&  ~S;" x))
          ((node? x)     (declare-node x))
          (t             (error "Unknown type ~A" x)))))

(defun output-subgraph (graph)
  (with-slots (buff cache) graph
     (if cache
         (format t "~A" cache)
         (output-buff buff))))

(defun output-cluster (cluster)
  (with-slots (name attrs buff cache) cluster
     (format t "~&  subgraph ~A {~%" name)
     (awhen attrs
       (format t "~&~{  ~A=~A;~^~&~}~&" (escape-attrs it)))
     (if cache
         (format t "~A~&  }" cache)
         (progn (output-buff buff)
                (format t "~&  }")))))

(defun output-edge (edge)
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

(defun output-node (node)
  (cond ((pre-node? node) (format t "~S" node))
        ((sesame? node)   (format t "~A" (sesame-cont node))) ;port
        ((node? node)     (format t "~A" (:name node)))
        (t                (error "Unknown type ~A" node))))

(defun output-edges (edges)
  (with-slots (nodes) edges
     (dolist (node nodes) (declare-node node))
     (~&) (princ "  ")
     (output-node (1st nodes))
     (dolist (node (rest nodes))
       (if (paths? edges) (princ " -- ") (princ " -> "))
       (output-node node))
     (princ ";")))

(defun declare-node (node)
  (when (and (node? node) (not (declared? node)))
    (output-declare node)
    (push node *declared*)))

(defun output-declare (node)
  (with-slots (name attrs) node
     (format t "~&  ~A [~{~A=~A~^,~}];" name (escape-attrs attrs))))

(defparameter *Mshape* '(:Mdiamond :Msquare :Mcircle :Mrecord))
(defun Mshape? (x) (member x *Mshape*))

(defparameter *upper-vals*
              '(:LR :RL :TB :BT :BL :BR :TL :TR :RB :RT :LB :LT))
(defun upper-val? (x) (member x *upper-vals*))

(defun html-like? (x)
  (and (consp x) (eql :html-like-label (1st x))))

(defun escape-attrs (alst)
  (let ((len (length alst)) (acc nil))
    (do ((i 0 (1+ i)))
        ((<= len i) (nreverse acc))
      (let ((x (nth i alst)))
        (if (evenp i)
            (push (string-downcase (symbol-name x)) acc)
            (push (cond ((html-like? x) (cdr x))
                        ((stringp x)    (str "\"" x "\""))
                        ((numberp x)    x)
                        ((eql x t)      "true")
                        ((eql x nil)    "false")
                        ((keywordp x)
                         (cond ((upper-val? x) (symbol-name x))
                               ((Mshape? x)    (string-capitalize
                                                (symbol-name x)))
                               (t              (string-downcase
                                                (symbol-name x)))))
                        ((cluster? x) (:name x))
                        (t (error "~A is not an attribute value type."
                                  x)))
                  acc))))))

;;====================================================================